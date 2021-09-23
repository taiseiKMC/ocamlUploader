
let default d = function
  | None -> d
  | Some v -> v

let bind opt f =
  match opt with
  | Some v -> f v
  | None -> None

let return v = Some v

let map opt f = bind opt (fun v -> return @@ f v)

let (>>=) = bind
let (>|=) = map

module Syntax = struct
  let (let*) = bind
  let (let+) = map
end
