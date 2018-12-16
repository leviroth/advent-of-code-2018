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

module type Input = sig
  type t
  val of_string : string -> t
  val load : string -> t
end

module type Output = sig
  type t
  val to_string : t -> string
end

module Make_parseable_single (T : sig
    type t
    val parser : t Angstrom.t
  end)
  : Input with type t = T.t
= struct
  include T

  let of_string s =
    Angstrom.parse_string parser s
    |> Result.ok_or_failwith

  let load file =
    In_channel.with_file file ~f:(fun in_channel ->
        Angstrom_unix.parse parser in_channel
        |> snd
        |> Result.ok_or_failwith)
end

module Make_parseable (T : sig
    type t
    val parser : t Angstrom.t
  end)
  : Input with type t = T.t list
= struct
  type t = T.t list

  let parser = Angstrom.many T.parser

  let of_string s =
    Angstrom.parse_string parser s
    |> Result.ok_or_failwith

  let load file =
    In_channel.with_file file ~f:(fun in_channel ->
        Angstrom_unix.parse parser in_channel
        |> snd
        |> Result.ok_or_failwith)
end


module type Solution = sig
  module Input : Input
  module Output : Output

  val part : int
  val solve : Input.t -> Output.t
end

module type Day = sig
  val date : int
  val parts : (module Solution) list
end

let pad_int = sprintf "%02d"

let solve_and_print (type a) (module S : Solution with type Input.t = a) (input : a) =
  S.solve input
  |> S.Output.to_string
  |> printf "%s\n"

let test_and_print input_string (module S : Solution) =
  S.Input.of_string input_string
  |> solve_and_print (module S)

let test_input day (module S : Solution) =
  let input_file = sprintf "../input/day%02d.txt" day in
  S.Input.load input_file
  |> solve_and_print (module S)

let solve_test_input day (module S : Solution) =
  let input_file = sprintf "./test_input/day%02d.txt" day in
  S.Input.load input_file
  |> solve_and_print (module S)

let solve_input day (module S : Solution) =
  let input_file = sprintf "./input/day%02d.txt" day in
  S.Input.load input_file
  |> solve_and_print (module S)

let make_solve_command day (module S : Solution) =
  let part_string = pad_int S.part in
  ( part_string
  , Command.basic ~summary:(sprintf "part %s solution" part_string)
      (let open Command.Let_syntax in
       let%map_open test = flag "-test" no_arg ~doc:" Use test input "
       in
       (fun () ->
          match test with
          | false -> solve_input day (module S)
          | true -> solve_test_input day (module S))))

let make_day_command (module D : Day) =
  let date_string = sprintf "%02d" D.date in
  ( date_string
  , Command.group ~summary:(sprintf "day %s solutions" date_string)
      (List.map D.parts ~f:(make_solve_command D.date)))
