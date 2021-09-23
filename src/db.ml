module Rwdb = Dokeysto.Db.RW


module Make (P : sig
    type val_ty
    val encode : val_ty -> string
    val decode : string -> val_ty
  end) = struct
  include P

  let add db key value =
    let encoded = encode value in
    Rwdb.add db key encoded;
    Rwdb.sync db

  let find db key =
    let value = Rwdb.find db key in
    decode value

  let find_opt db key =
    let open Option.Syntax in
    let+ value =
      try Some (Rwdb.find db key) with
      | Not_found -> None in
    decode value

  let fold f db =
    Rwdb.fold (fun k v -> f k (decode v)) db
end


module Filename = struct
  include Make (struct
      type val_ty = string * float * string

      let encode (descr, uptime, filename) =
        Format.sprintf "%s<%f<%s" descr uptime filename

      let decode value =
        let lst = Str.split (Str.regexp "<") value in
        match lst with
        | descr :: uptime :: filename -> descr, (float_of_string uptime), (String.concat "<" filename)
        | _ -> failwith @@ "parse error " ^ (String.concat ", " lst)
    end)

  let db_filename = "filenames.db"
  let db =
    if Sys.file_exists db_filename
    then Rwdb.open_existing db_filename
    else Rwdb.create db_filename

  let find = find db
  let find_opt = find_opt db
  let add = add db
  let fold f = fold f db
end
