val default : 'a -> 'a option -> 'a
val bind : 'a option -> ('a -> 'b option) -> 'b option
val return : 'a -> 'a option
val map : 'a option -> ('a -> 'b) -> 'b option

val (>>=) : 'a option -> ('a -> 'b option) -> 'b option
val (>|=) : 'a option -> ('a -> 'b) -> 'b option

module Syntax : sig
  val (let*) : 'a option -> ('a -> 'b option) -> 'b option
  val (let+) : 'a option -> ('a -> 'b) -> 'b option
end
