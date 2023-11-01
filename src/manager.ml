open Lwt

let content_dir = "uploads/"

let uuidt = ref (Uuidm.v5 Uuidm.nil @@ string_of_float @@ Unix.time ())

let gen_uuid str =
  let uuid = Uuidm.v5 !uuidt (Option.default "" str) in
  uuidt := uuid;
  Uuidm.to_string uuid

let remove_record uuid =
  match Db.FileDB.find_opt uuid with
  | None -> Lwt.return_error Not_found
  | Some _ ->
      let fn = uuid in
      let filename = content_dir ^ fn in
      Lwt_unix.unlink filename >>= fun () ->
      Db.FileDB.remove fn;
      Lwt.return_ok ()

let add_record uuid descr filename =
  let descr = if String.length descr = 0 then "(No description is given)" else descr in
  let descr = Netencoding.Html.encode ~in_enc:`Enc_utf8 ~out_enc:`Enc_utf8 () descr in
  let time = Unix.time () in
  Db.FileDB.add uuid (descr, time, filename)

let add_dummy_record descr filename =
  let uuid = gen_uuid None in
  let filename = Option.default uuid filename in
  add_record uuid descr filename;
  (uuid, filename)
