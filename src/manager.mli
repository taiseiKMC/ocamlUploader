(** APIs to manipulate Db.FileDB *)

(** The directory where uploaded files are saved *)
val content_dir : string

(** [gen_uuid str] generates uuid named by [str] *)
val gen_uuid : string option -> string

(** [remove_record uuid] deletes the file indexed by [uuid] *)
val remove_record : string -> (unit, exn) result Lwt.t

(** [add_record uuid descr filename] adds a record to [Db.FileDB] indexed by [uuid] *)
val add_record : string -> string -> string -> unit

(** Add a record with random-generated uuid *)
val add_dummy_record : string -> string option -> string * string
