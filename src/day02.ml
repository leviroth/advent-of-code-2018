open! Core
open! Import

let date = 2

module Common = struct
  module Input = Converters.String_list
  module Output = Int
end

module Chars_by_count = struct
  type t = char list Int.Map.t

  let create word =
    let result = Char.Table.create () in
    String.iter word ~f:(fun c ->
        Hashtbl.update result c ~f:(function
            | None -> 1
            | Some n -> n + 1));
    Hashtbl.to_alist result
    |> List.map ~f:(Tuple2.swap)
    |> Int.Map.of_alist_multi

  let has_count t count =
    Map.find t count
    |> Option.is_some
end

module Part01 = struct
  include Common
  let part = 1

  let solve input =
    List.map input ~f:Chars_by_count.create
    |> List.fold ~init:(0, 0) ~f:(fun (twos, threes) count ->
        ( twos   + Bool.to_int (Chars_by_count.has_count count 2)
        , threes + Bool.to_int (Chars_by_count.has_count count 3)
        ))
    |> Tuple2.uncurry ( * )
end

let parts : (module Solution) list =
  [ (module Part01) ]
