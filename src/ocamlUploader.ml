open Lwt
open Cohttp
open Cohttp_lwt_unix


let uuidt = ref Uuidm.nil

let content_dir = "uploads/"

let download req path =
  (match Request.meth req with
   | `GET -> ()
   | _ -> failwith "invalid request");
  let idx = Str.match_end () in
  let filename = String.sub path idx (String.length path - idx) in
  let (_, _, original_name) = Db.Filename.find filename in
  let filename = content_dir ^ filename in
  let headers = Header.init () in
  let header_content_disposition = Format.sprintf "attachment; filename=%s" original_name in
  let headers = Header.add headers "Content-Disposition" header_content_disposition in
  (* todo file existense check *)
  Server.respond_file ~headers ~fname:filename ()

type post_field = [
    `Upfile of string * string * string
  | `Descr ]

let upload req body =
  let headers = req |> Request.headers |> Header.to_string in
  Cohttp_lwt.Body.to_string body >>= fun body ->
  let open Multipart_form in
  let save_part : filename:string -> Multipart_form.Header.t -> string Lwt_stream.t ->
    unit Lwt.t = fun ~filename _header stream ->
    Lwt_io.with_file ~mode:Output filename (fun channel ->
        Lwt_stream.iter_s (fun str ->
            Lwt_io.write channel str) stream
      ) in
  let random_unique_filename header =
    Logs.info (fun m -> m "filename: %a\n" Header.pp header);
    let cd = match Header.content_disposition header with
      | Some cd -> cd
      | None -> failwith "content-disposition is not exist" in
    let fname = match Content_disposition.name cd with
      | Some fn -> fn
      | None -> failwith "unexpected post name" in
    if fname = "description" then
      `Descr
    else
      let name = Content_disposition.filename cd in
      let uuid = Uuidm.v5 !uuidt (Option.default "" name) in
      uuidt := uuid;
      let uuid = Uuidm.to_string uuid in
      let name = Option.default uuid name in
      `Upfile (uuid, name, content_dir ^ uuid) in
  let identify header = random_unique_filename header in

  let write data =
    let content_type = Header.content_type data.header in
    let body_stream = Lwt_stream.of_list [data.body] in
    (* Logs.info (fun m -> m "leaf: %a\ncontent: %s" Header.pp data.header data.body); *)

    let `Parse th, stream = Multipart_form_lwt.(stream ~identify body_stream content_type) in
    let rec parses posts = Lwt_stream.get stream >>= fun data ->
      match data, posts with
      | None, (Some upf, Some ctn) -> Lwt.return (upf, ctn)
      | None, (Some upf, None) -> Lwt.return (upf, Lwt_stream.of_list ["(No description is given)"])
      | Some (`Upfile (uuid, name, filename), hdr, contents), (None, descr) ->
        save_part ~filename hdr contents >>= fun () ->
        parses (Some (uuid, name), descr)
      | Some (`Descr, _hdr, contents), (upf, None) ->
        parses (upf, Some contents)
      | _ -> failwith "Unexpected contents" in
    let saves () =
      parses (None, None) >>=
      fun ((uuid, name), dsr_ctn) ->
      Lwt_stream.to_list dsr_ctn >>= fun descr ->
      let descr = String.concat "" descr in
      let descr = Netencoding.Html.encode ~in_enc:`Enc_utf8 ~out_enc:`Enc_utf8 () descr in
      let time = Unix.time () in
      Db.Filename.add uuid (descr, time, name);
      Lwt.return (Format.sprintf "%s : %s" uuid name) in


    both th (saves ()) >>= fun (res, file_list) ->
    match res with
    | Ok _ -> Lwt.return file_list
    | Error (`Msg str) -> failwith str in

  let header = Angstrom.parse_string Header.Decoder.header ~consume:Prefix headers in
  let header = match header with | Ok str -> str | Error str -> failwith str in
  write { header; body }
  >>= fun file_list ->
  let body = Format.sprintf "Accepted\n%s\n" (String.concat "\n" [file_list]) in
  Server.respond_string ~status:`OK ~body ()

let index _req =
  (* let uri = req |> Request.uri |> Uri.to_string in
     let meth = req |> Request.meth |> Code.string_of_method in
     let headers = req |> Request.headers |> Header.to_string in *)
  let filenames = Db.Filename.fold
      (fun k v s -> (k, v) :: s) [] in
  let filenames = List.sort (fun (_, (_, t1, _)) (_, (_, t2, _)) -> compare t2 t1) filenames in
  (* ( Cohttp_lwt.Body.to_string body >|= fun body ->
     Printf.sprintf "Uri: %s\nMethod: %s\nHeaders\nHeaders: %s\nBody: %s\nFiles: %s" uri
      meth headers body indexes) *)
  let url = "download/" in
  let body = Index.build_index url filenames in
  Server.respond_string ~status:`OK ~body ()

let server =
  if not @@ Sys.file_exists content_dir then Sys.mkdir content_dir 0o777 else ();
  let callback _conn req body =
    let path = req |> Request.uri |> Uri.path in
    let download_re = Str.regexp "/download/*" in
    let upload_re = Str.regexp "/upload" in
    begin
      if Str.string_match download_re path 0 then download req path
      else if Str.string_match upload_re path 0 then upload req body
      else index req
    end
  in
  Server.create
    ~mode:(`TCP (`Port 8000))
    (Server.make ~callback ())
