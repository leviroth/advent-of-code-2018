open! Core
open! Import

let date = 3

module Claim = struct
  type t =
    { id : int
    ; left_offset : int
    ; top_offset : int
    ; width : int
    ; height : int
    }
  [@@deriving fields]

  let contents t =
    let open List.Let_syntax in
    let%bind row = List.range t.top_offset (t.top_offset + t.height) in
    let%bind col = List.range t.left_offset (t.left_offset + t.width) in
    [ (row, col) ]

  let parser =
    let open Angstrom in
    let integer =
      take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string
    in
    let id =
      char '#' *> integer
    in
    let coordinates =
      lift2 Tuple2.create integer (char ',' *> integer)
    in
    let dimensions =
      lift2 Tuple2.create integer (char 'x' *> integer)
    in
    lift3
      (fun id (left_offset, top_offset) (width, height) ->
         { id; left_offset; top_offset; width; height })
      id
      (string " @ " *> coordinates)
      (string ": " *> dimensions <* char '\n')
end

module Common = struct
  module Input = Make_parseable (Claim)
  module Output = Int
end

let get_counts input =
  List.concat_map input ~f:Claim.contents
  |> List.map ~f:(fun x -> x, 1)
  |> Int_pair.Map.of_alist_reduce ~f:(+)

module Part01 = struct
  include Common
  let part = 1

  let solve input =
    get_counts input
    |> Map.data
    |> List.count ~f:(fun x -> x > 1)
end

module Part02 = struct
  include Common
  let part = 2

  let solve input =
    let counts = get_counts input in
    List.find_exn input ~f:(fun claim ->
        Claim.contents claim
        |> List.for_all ~f:(fun x -> Map.find_exn counts x = 1))
    |> Claim.id
end

let parts : (module Solution) list =
  [ (module Part01)
  ; (module Part02)
  ]

let%expect_test _ =
  List.iter parts ~f:(test_input date);
  [%expect{|
    101469
    1067 |}]
