open! Import

open Solution_intf

module Part = struct
  include Part_intf

  module Make (Solution : Part_intf.Basic) = struct
    include Solution

    let solve_file filename =
      Solution.Input.load filename
      |> Solution.solve
      |> Solution.Output.to_string

    let solve_input input =
      Solution.Input.of_string input
      |> Solution.solve
      |> Solution.Output.to_string

  end
end

module Day = struct
  include Day_intf

  module Make (Day : Day_intf.Basic) = struct
    let date = Day.date

    let parts =
      List.map Day.parts ~f:(fun (module This_part : Part.Basic) ->
        (module Part.Make (This_part) : Part.S))

    let summary = sprintf "day %02d solutions" Day.date

    let command =
      let subcommand_of_part (module Part : Part_intf.S) =
        let part_string = pad_int Part.part in
        ( part_string
        , Command.basic ~summary:(sprintf "part %s solution" part_string)
            (Command.Param.return
               (fun () ->
                  let input_file = sprintf "./input/day%02d.txt" Day.date in
                  Part.solve_file input_file
                  |> printf "%s\n")))
      in
      Command.group ~summary
        (List.map parts ~f:subcommand_of_part)
  end
end
