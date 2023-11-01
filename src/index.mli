(**
  [build_index path ?upload_status files] generates html file
  using "index.jingoo" template
  where
  - [path] is the path where uploaded files can be download
  - [upload_status] is the message when an user uploads a file
  - [files] are the list of
    (Filename in server(uuid), (Description, Uploaded time, Filename(Original)))
*)
val build_index :
  string ->
  ?upload_status:string ->
  (string * Db.FileDB.val_ty) list ->
  string
