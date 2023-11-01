open Jingoo

let build_index path ?upload_status files =
  let files = List.map
      (fun (uuid, (descr, _, name)) -> 
         Jg_types.Tobj [
           ("name", Jg_types.Tstr name);
           ("url", Jg_types.Tstr (path ^ uuid));
           ("descr", Jg_types.Tstr descr);
         ]) files in
  let uploaded, upload_status =
    match upload_status with
    | Some upload_status -> true, upload_status
    | None -> false, "" in
  Jg_template.from_file "index.jingoo" ~models:[
    ("title", Jg_types.Tstr "OCamlUploader beta");
    ("is_debug", Jg_types.Tbool true);
    ("files", Jg_types.Tlist files);
    ("uploaded", Jg_types.Tbool uploaded);
    ("upload_status", Jg_types.Tstr upload_status);
  ]
