open! Core
open! Import

let date = 3

module Claim = struct
  module T = struct
    type t =
      { left_offset : int
      ; top_offset : int
      ; width : int
      ; height : int
      }
    [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)

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
    let row_number =
      char '#' *> integer
    in
    let coordinates =
      lift2 Tuple2.create integer (char ',' *> integer)
    in
    let dimensions =
      lift2 Tuple2.create integer (char 'x' *> integer)
    in
    lift2
      (fun (left_offset, top_offset) (width, height) ->
         { left_offset; top_offset; width; height })
      (row_number *> string " @ " *> coordinates)
      (string ": " *> dimensions <* char '\n')
end

module Int_pair = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp]
  end
  include T
  include Comparable.Make (T)
end

module Common = struct
  module Input = struct
    type t = Claim.t list

    let parser = Angstrom.many Claim.parser

    let of_string s =
      Angstrom.parse_string parser s
      |> Result.ok_or_failwith

    let load file =
      In_channel.with_file file ~f:(fun in_channel ->
          Angstrom_unix.parse parser in_channel
          |> snd
          |> Result.ok_or_failwith)
  end

  module Output = Int

  let get_counts input =
    List.concat_map input ~f:Claim.contents
    |> List.map ~f:(fun x -> x, 1)
    |> Int_pair.Map.of_alist_reduce ~f:(+)
end


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
    List.findi input ~f:(fun _ claim ->
        Claim.contents claim
        |> List.for_all ~f:(fun x -> Map.find_exn counts x = 1))
    |> Option.value_exn
    |> fst
    |> Int.succ
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
