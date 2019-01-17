open! Import

module type Output = sig
  type t
  val to_string : t -> string
end

module Part_intf = struct
  module type Basic = sig
    module Input : Input.S
    module Output : Output

    val part : int
    val solve : Input.t -> Output.t
  end

  module type S = sig
    include Basic

    (* val test_and_print : string -> unit
     * val solve_and_print : Input.t -> unit
     * 
     * val test_input : int -> unit
     * val solve_test_input : int -> unit
     * val solve_input : int -> unit *)

    val solve_file : string -> string
    val solve_input : string -> string

    (* val command : int -> string * Command.t *)
  end

  module type Part = sig
    module type Basic = Basic
    module type S = S

    module Make : Basic -> S
  end
end

module Day_intf = struct
  module type Basic = sig
    val date : int
    val parts : (module Part_intf.Basic) list
  end

  module type S = sig
    val date : int
    val parts : (module Part_intf.S) list

    val command : Command.t
  end

  module type Day = sig
    module type Basic = Basic
    module type S = S

    module Make : Basic -> S
  end
end

module type Solution = sig
  module Part : Part_intf.Part
  module Day : Day_intf.Day
end
