include (Core : module type of Core with module Option := Core.Option)
include Int.Replace_polymorphic_compare

module Option = struct
  include Core.Option
  let value_exn t = Core.Option.value_exn ?here:None ?message:None ?error:None t
end

module Int_pair = struct
  include Tuple.Make (Int) (Int)
  include Tuple.Comparable (Int) (Int)
end

(* module type Day = sig
 *   val date : int
 *   val parts : (module Solution) list
 * end *)

let pad_int = sprintf "%02d"
