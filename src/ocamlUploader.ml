open Lwt.Syntax
open Cohttp
open Cohttp_lwt_unix

module Manager = Manager

let content_dir = Manager.content_dir

(** Generate html page *)
let index_body ?upload_status () =
  (* let uri = req |> Request.uri |> Uri.to_string in
     let meth = req |> Request.meth |> Code.string_of_method in
     let headers = req |> Request.headers |> Header.to_string in *)
  let filenames = Db.FileDB.fold (fun k v s -> (k, v) :: s) [] in
  let filenames = List.sort (fun (_, (_, t1, _)) (_, (_, t2, _)) -> compare t2 t1) filenames in
  (* ( Cohttp_lwt.Body.to_string body >|= fun body ->
     Printf.sprintf "Uri: %s\nMethod: %s\nHeaders\nHeaders: %s\nBody: %s\nFiles: %s" uri
      meth headers body indexes) *)
  let path = "download/" in
  Index.build_index path ?upload_status filenames

let download req path =
  let () =
    match Request.meth req with
    | `GET -> ()
    | _ -> failwith "invalid request" in
  let idx = Str.match_end () in
  let filename = String.sub path idx (String.length path - idx) in
  let (_, _, original_name) = Db.FileDB.find filename in
  let filename = content_dir ^ filename in
  let headers = Header.init () in
  let header_content_disposition = Format.sprintf "attachment; filename= %s" original_name in
  let headers = Header.add headers "Content-Disposition" header_content_disposition in
  (* todo file existense check *)
  Server.respond_file ~headers ~fname:filename ()

type post_field =
  [ `Upfile of string * string * string
  | `Descr
  | `Filename
  ]
[@@ocaml.warning "-34"]

let upload req (body : Cohttp_lwt.Body.t) =
  let headers = req |> Request.headers |> Header.to_string in
  let content_length = match Request.encoding req with | Fixed cl -> Some cl | _ -> None in
  let open Multipart_form in
  let save_part :
    filename:string ->
    Multipart_form.Header.t ->
    string Lwt_stream.t ->
    unit Lwt.t = (* save posted data to a file *)
    fun ~filename _header stream ->
      Lwt_io.with_file
        ~mode:Output filename
        (fun channel -> Lwt_stream.iter_s (Lwt_io.write channel) stream) in
  let random_unique_filename header =
    (* Description and File are sent in other part using multipart_form.
       These are distinguished by name of each content *)
    Logs.info (fun m -> m "filename: %a\n" Header.pp header);
    let cd =
      match Header.content_disposition header with
      | Some cd -> cd
      | None -> failwith "content-disposition is not exist" in
    let fname =
      match Content_disposition.name cd with
      | Some fn -> fn
      | None -> failwith "unexpected post name" in
    if fname = "description" then
      `Descr
    else
      let name = Content_disposition.filename cd in
      let uuid = Manager.gen_uuid name in
      let name = Option.default uuid name in
      `Upfile (uuid, name, content_dir ^ uuid) in
  let identify header = random_unique_filename header in

  let write data =
    let content_type = Header.content_type data.header in
    let body_stream = Cohttp_lwt.Body.to_stream (data.body : Cohttp_lwt.Body.t) in
    let bytes_size = ref 0L in
    let body_stream = (* Just count the length of [body_stream] *)
      Lwt_stream.map
        (fun s -> bytes_size := Int64.(add (of_int (String.length s)) !bytes_size); s)
        body_stream in
    let check_len =
      let* () = Lwt_stream.closed body_stream in
      match content_length with
      | Some content_length ->
          if (!bytes_size = content_length) then Lwt.return_ok ()
          else Lwt.return_error "Connection corrupted"
      | None -> Lwt.return_ok () in
    (* Logs.info (fun m -> m "leaf: %a\ncontent: %s" Header.pp data.header data.body); *)

    let `Parse th, stream = Multipart_form_lwt.stream ~identify body_stream content_type in
    let rec parses posts =
      let* data = Lwt_stream.get stream in
      match data, posts with
      | None, (Some upf, Some ctn) -> Lwt.return (upf, ctn)
      | None, (Some upf, None) ->
          (* Description is not needed *)
          Lwt.return (upf, Lwt_stream.of_list [])
      | Some (`Upfile (uuid, name, filename), hdr, contents), (None, descr) ->
          let* () = save_part ~filename hdr contents in
          parses (Some (uuid, name, filename), descr)
      | Some (`Descr, _hdr, contents), (upf, None) ->
          parses (upf, Some contents)
      | _ -> failwith "Unexpected contents" in
    let saves () =
      let* ((uuid, name, filename), dsr_ctn) = parses (None, None) in
      let* descr = Lwt_stream.to_list dsr_ctn in
      let descr = String.concat "" descr in
      Manager.add_record uuid descr name;
      Lwt.return ((uuid, filename), Format.sprintf "%s : %s" uuid name) in


    let* (res, ((uuid, filename), file)) = Lwt.both th (saves ()) in
    let* () =
      let* res = check_len in
      match res with
      | Ok () -> Lwt.return_unit
      | Error str ->
          Db.FileDB.remove uuid;
          let* () = Lwt_unix.unlink filename in
          failwith str in
    match res with
    | Ok _ -> Lwt.return file
    | Error (`Msg str) -> failwith str in

  let header = Angstrom.parse_string Header.Decoder.header ~consume:Prefix headers in
  let header = match header with | Ok str -> str | Error str -> failwith str in
  let* file = write { header; body } in
  let upload_status = Format.sprintf "Accepted\n%s\n" (String.concat "\n" [file]) in
  let body = index_body ~upload_status () in
  let headers = Cohttp.Header.init_with "Location" "/" in
  Server.respond_string ~headers ~status:`See_other ~body ()

let remove_record req body =
  let headers = req |> Request.headers |> Header.to_string in
  let* body = Cohttp_lwt.Body.to_string body in
  let open Multipart_form in
  let identify _ = () in

  let write data =
    let content_type = Header.content_type data.header in
    let body_stream = Lwt_stream.of_list [data.body] in
    (* Logs.info (fun m -> m "leaf: %a\ncontent: %s" Header.pp data.header data.body); *)

    let `Parse th, stream = Multipart_form_lwt.stream ~identify body_stream content_type in
    let saves () =
      let* data = Lwt_stream.get stream in
      match data with
      | None -> failwith "Recieved body has no data"
      | Some ((), _hdr, contents) ->
          let* fn = Lwt_stream.to_list contents in
          let fn = String.concat "" fn in
          Lwt.return fn
    in
    let* (res, fn) = Lwt.both th (saves ()) in
    match res with
    | Ok _ ->
        let* res = Manager.remove_record fn in
        (match res with
         | Ok () -> Lwt.return (`OK, fn)
         | Error Not_found -> Lwt.return (`Not_found, "")
         | Error _ -> Lwt.return (`Internal_server_error, ""))
    | Error (`Msg str) -> Lwt.return (`Internal_server_error, str) in

  let header = Angstrom.parse_string Header.Decoder.header ~consume:Prefix headers in
  let header = match header with | Ok str -> str | Error str -> failwith str in
  let* (status, str) = write { header; body } in
  let upload_status =
    match status with
    | `OK -> Format.sprintf "Succeeded to delete \n%s\n" (String.concat "\n" [str])
    | `Internal_server_error -> str
    | _ -> "Failed" in
  let body = index_body ~upload_status () in
  Server.respond_string ~status ~body ()
[@@ocaml.warning "-32"]

let debug_addrecord req body =
  let headers = req |> Request.headers |> Header.to_string in
  let* body = Cohttp_lwt.Body.to_string body in
  let open Multipart_form in
  let random_unique_filename header =
    Logs.info (fun m -> m "filename: %a\n" Header.pp header);
    let cd = match Header.content_disposition header with
      | Some cd -> cd
      | None -> failwith "content-disposition is not exist" in
    let fname = match Content_disposition.name cd with
      | Some fn -> fn
      | None -> failwith "unexpected post name" in
    if fname = "description" then `Descr else `Filename
  in
  let identify header = random_unique_filename header in

  let write data =
    let content_type = Header.content_type data.header in
    let body_stream = Lwt_stream.of_list [data.body] in
    (* Logs.info (fun m -> m "leaf: %a\ncontent: %s" Header.pp data.header data.body); *)

    let `Parse th, stream = Multipart_form_lwt.(stream ~identify body_stream content_type) in
    let rec saves (fn, descr) =
      let* data = Lwt_stream.get stream in
      match data with
      | None -> Lwt.return (fn, descr)
      | Some (`Descr, _hdr, contents) ->
          let* descr = Lwt_stream.to_list contents in
          saves (fn, Some descr)
      | Some (`Filename, _hdr, contents) ->
          let* fn = Lwt_stream.to_list contents in
          let fn = String.concat "" fn in
          saves (Some fn, descr)
    in
    let* (res, (filename, descr)) = Lwt.both th (saves (None, None)) in
    let descr = Option.default [""] descr in
    let descr = String.concat "" descr in
    let (uuid, filename) = Manager.add_dummy_record descr filename in
    match res with
    | Ok _ -> Lwt.return (Format.sprintf "%s : %s" uuid filename)
    | Error (`Msg str) -> failwith str in

  let header = Angstrom.parse_string Header.Decoder.header ~consume:Prefix headers in
  let header = match header with | Ok str -> str | Error str -> failwith str in
  let* file_list = write { header; body } in
  let upload_status = Format.sprintf "Accepted\n%s\n" (String.concat "\n" [file_list]) in
  let body = index_body ~upload_status () in
  Server.respond_string ~status:`OK ~body ()
[@@ocaml.warning "-32"]

let index _req =
  let body = index_body () in
  Server.respond_string ~status:`OK ~body ()

let server port =
  if not @@ Sys.file_exists content_dir then Sys.mkdir content_dir 0o777 else ();
  let callback _conn req body =
    let path = req |> Request.uri |> Uri.path in
    let download_re = Str.regexp "/download/*" in
    let upload_re = Str.regexp "/upload" in
    (* let delete_re = Str.regexp "/debug/delete" in *)
    (* let debug_addrecord_re = Str.regexp "/debug/add_record" in *)
    begin
      if Str.string_match download_re path 0 then download req path
      else if Str.string_match upload_re path 0 then upload req body
      (* else if Str.string_match delete_re path 0 then remove_record req body *)
      (* else if Str.string_match debug_addrecord_re path 0 then debug_addrecord req body *)
      else index req
    end
  in
  Server.create
    ~mode:(`TCP (`Port port))
    (Server.make ~callback ())
