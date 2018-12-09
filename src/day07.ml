open! Import

let date = 7

module Dependency = struct
  type t = char * char

  let parser =
    let open Angstrom in
    lift2
      (fun a b -> b, a)
      (string "Step " *> any_char)
      (string " must be finished before step " *> any_char <* string " can begin.\n")
end

module Part01 = struct
  module Input = Make_parseable (Dependency)
  module Output = String

  let part = 1

  let downward_graph input =
    let dependencies =
      List.map input ~f:(fun (_, x) -> x, [])
      |> Char.Map.of_alist_reduce ~f:(fun _ _ -> [])
    in
    List.fold
      input
      ~init:dependencies
      ~f:(fun map (key, data) -> Map.add_multi map ~key ~data)
    |> Map.map ~f:Char.Set.of_list

  let step downward_graph =
    let root_map = Map.filter downward_graph ~f:Set.is_empty in
    let root =
      Map.keys root_map
      |> List.hd_exn
    in
    let new_map =
      Map.remove downward_graph root
      |> Map.map ~f:(fun set -> Set.remove set root)
    in
    root, new_map

  let rec loop to_print downward =
    match Map.is_empty downward with
    | true ->
      List.rev to_print
      |> String.of_char_list
    | false ->
      let new_words, downward = step downward in
      loop (new_words :: to_print) downward

  let solve input =
    loop [] (downward_graph input)

end

let parts : (module Solution) list =
  [ (module Part01)
  ]

let%expect_test _ =
  List.iter parts ~f:(test_and_print
{|Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.
|});
  [%expect{| CABDFE |}]
