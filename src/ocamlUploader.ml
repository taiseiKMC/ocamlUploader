open Lwt
open Cohttp
open Cohttp_lwt_unix

module Rwdb = Dokeysto.Db.RW

let uuidt = ref Uuidm.nil
let content_dir = "uploads/"
let db_filename = "file_names"
let db =
  if Sys.file_exists db_filename
  then Rwdb.open_existing db_filename
  else Rwdb.create db_filename

let download req path =
  (match Request.meth req with
   | `GET -> ()
   | _ -> failwith "invalid request");
  let idx = Str.match_end () in
  let filename = String.sub path idx (String.length path - idx) in
  let filename = content_dir ^ filename in
  (* todo file existense check *)
  Server.respond_file ~fname:filename ()

let upload req body =
  let _uri = req |> Request.uri |> Uri.to_string in
  let _meth = req |> Request.meth |> Code.string_of_method in
  let headers = req |> Request.headers |> Header.to_string in
  Cohttp_lwt.Body.to_string body >>= fun body ->
  let open Multipart_form in
  let save_part : filename:string -> Multipart_form.Header.t -> string Lwt_stream.t ->
    unit Lwt.t = fun ~filename _header stream ->
    Lwt_io.with_file ~mode:Output filename (fun channel ->
        Lwt_stream.iter_s (fun str ->
            Lwt_io.write channel str) stream
      )
  in
  let random_unique_filename header =
    let name = match Header.content_disposition header with
    | Some cd ->
      (match Content_disposition.filename cd with
       | Some filename -> Some filename
       | None -> None)
    | None -> None in
    let uuid = Uuidm.v5 !uuidt (Option.default "" name) in
    uuidt := uuid;
    let uuid = Uuidm.to_string uuid in
    let name = Option.default uuid name in
    (uuid, name, content_dir ^ uuid) in
  let identify header = random_unique_filename header in

  let write data =
    let content_type = Header.content_type data.header in
    let body_stream = Lwt_stream.of_list [data.body] in
    (* Logs.info (fun m -> m "leaf: %a\ncontent: %s" Header.pp data.header data.body); *)
    
    let `Parse th, stream = Multipart_form_lwt.(stream ~identify body_stream content_type) in
    let rec saves file_list = Lwt_stream.get stream >>= function
      | None -> Lwt.return file_list
      | Some ((uuid, name, filename), hdr, contents) ->
        save_part ~filename hdr contents >>= fun () ->
        Rwdb.add db uuid name;
        Rwdb.sync db;
        saves (Format.sprintf "%s : %s" uuid name :: file_list) in
    both th (saves []) >>= fun (res, file_list) ->
    match res with
    | Ok _ -> Lwt.return file_list
    | Error (`Msg str) -> failwith str in

  let header = Angstrom.parse_string Header.Decoder.header ~consume:Prefix headers in
  let header = match header with | Ok str -> str | Error str -> failwith str in
  write { header; body }
  >>= fun file_list ->
  let body = Format.sprintf "Accepted\n%s\n" (String.concat "\n" file_list) in
  Server.respond_string ~status:`OK ~body ()

let index req body =
  let uri = req |> Request.uri |> Uri.to_string in
  let meth = req |> Request.meth |> Code.string_of_method in
  let headers = req |> Request.headers |> Header.to_string in
  let filenames = Rwdb.fold (fun k v s -> Format.sprintf "http://%s/%s : %s" uri k v :: s) db [] in
  let indexes = String.concat "\n" filenames in
  ( Cohttp_lwt.Body.to_string body >|= fun body ->
    Printf.sprintf "Uri: %s\nMethod: %s\nHeaders\nHeaders: %s\nBody: %s\nFiles: %s" uri
      meth headers body indexes)
  >>= fun body ->
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
      else index req body
    end
  in
  Server.create
    ~mode:(`TCP (`Port 8000))
    (Server.make ~callback ())
