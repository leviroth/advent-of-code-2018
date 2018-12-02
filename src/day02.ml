open! Core
open! Import

let date = 2

module Chars_by_count = struct
  type t = char list Int.Map.t

  let create word =
    let result = Char.Table.create () in
    String.iter word ~f:(fun c ->
        Hashtbl.update result c ~f:(function
            | None -> 1
            | Some n -> n + 1));
    Hashtbl.to_alist result
    |> List.map ~f:Tuple2.swap
    |> Int.Map.of_alist_multi

  let has_count t count =
    Map.find t count
    |> Option.is_some
end

module Part01 = struct
  module Input = Converters.String_list
  module Output = Int

  let part = 1

  let solve input =
    List.map input ~f:Chars_by_count.create
    |> List.fold ~init:(0, 0) ~f:(fun (twos, threes) count ->
        ( twos   + Bool.to_int (Chars_by_count.has_count count 2)
        , threes + Bool.to_int (Chars_by_count.has_count count 3)
        ))
    |> Tuple2.uncurry ( * )
end

module Part02 = struct
  module Input = Converters.String_list
  module Output = String

  let part = 2

  let is_correct (a, b) =
    let a = String.to_list a in
    let b = String.to_list b in
    List.zip_exn a b
    |> List.count ~f:(fun (c1, c2) -> not (Char.equal c1 c2))
    |> Int.equal 1

  let solve input =
    let word_a, word_b =
      List.cartesian_product input input
      |> List.find_exn ~f:is_correct
    in
    String.to_list word_a
    |> List.filter_mapi ~f:(fun i c ->
        match Char.equal c word_b.[i] with
        | true -> Some c
        | false -> None)
    |> String.of_char_list
end

let parts : (module Solution) list =
  [ (module Part01)
  ; (module Part02)
  ]
