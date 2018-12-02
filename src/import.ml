open! Core

module type Input = sig
  type t
  val of_string : string -> t
  val load : string -> t
end

module type Output = sig
  type t
  val to_string : t -> string
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

let test_and_print (module S : Solution) input_string =
  S.Input.of_string input_string
  |> solve_and_print (module S)

let solve_input day (module S : Solution) =
  let input_file = sprintf "../input/day%02d.txt" day in
  S.Input.load input_file
  |> solve_and_print (module S)

let make_solve_command day (module S : Solution) =
  let part_string = pad_int S.part in
  ( part_string
  , Command.basic ~summary:(sprintf "part %s solution" part_string)
      (Command.Param.return (fun () -> solve_input day (module S))))

let make_day_command (module D : Day) =
  let date_string = sprintf "%02d" D.date in
  ( date_string
  , Command.group ~summary:(sprintf "day %s solutions" date_string)
      (List.map D.parts ~f:(make_solve_command D.date)))
