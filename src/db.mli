(**
  Wrapper of Dokeysto.Db.RW, A Key-Value Store Library.
  It creates filenames.db and filenames.db.idx
*)

module FileDB : sig
  (** Tuple of (Description, Uploaded time, Filename) *)
  type val_ty = string * float * string
  
  val find : string -> val_ty
  val find_opt : string -> val_ty option
  val add : string -> val_ty -> unit
  val remove : string -> unit
  val fold : (string -> val_ty -> 'a -> 'a) -> 'a -> 'a
end
