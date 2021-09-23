open Jingoo

let build_index url files =
  let files = List.map
      (fun (uuid, (descr, _, name)) -> 
         Jg_types.Tobj [
           ("name", Jg_types.Tstr name);
           ("url", Jg_types.Tstr (url ^ uuid));
           ("descr", Jg_types.Tstr descr);
         ]) files in
  Jg_template.from_file "index.jingoo" ~models:[
    ("title", Jg_types.Tstr "OCamlUploader");
    ("is_debug", Jg_types.Tbool true);
    ("files", Jg_types.Tlist files)
  ]