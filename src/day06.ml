open! Import

let date = 6

module Coordinate = struct
  include Int_pair

  let integer =
    let open Angstrom in
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

  let parser =
    let open Angstrom in
    lift2
      Tuple2.create
      integer
      (char ',' *> skip_while Char.is_whitespace *> integer <* char '\n')

end

module Common = struct
  module Input = Input.Make_parseable_many (Coordinate)
  module Output = Int
end

module Bounds = struct
  type t =
    { left : int
    ; right : int
    ; top : int
    ; bottom : int
    }
  [@@deriving sexp]

  let contains t (x, y) =
    t.left <= x && x <= t.right
    && t.top <= y && y <= t.bottom

  let of_coordinates coordinates =
    let xs, ys = List.unzip coordinates in
    { left = List.min_elt ys ~compare |> Option.value_exn
    ; right = List.max_elt ys ~compare |> Option.value_exn
    ; top = List.min_elt xs ~compare |> Option.value_exn
    ; bottom = List.max_elt xs ~compare |> Option.value_exn
    }
end

let distance (x, y) (x', y') =
  abs (x - x') + abs (y - y')

module Part01 = struct
  include Common

  let part = 1

  let find_closest coordinate roots =
    let min_dist =
      List.map roots ~f:(distance coordinate)
      |> List.min_elt ~compare
      |> Option.value_exn
    in
    match List.filter roots ~f:(fun root -> distance coordinate root = min_dist) with
    | [ singleton ] -> Some singleton
    | _ -> None

  let solve input =
    let range = List.range ~stop:`inclusive in
    let bounds = Bounds.of_coordinates input in
    let all_coordinates =
      List.cartesian_product
        (range bounds.left bounds.right)
        (range bounds.top bounds.bottom)
    in
    let closest_map =
      List.fold all_coordinates ~init:Coordinate.Map.empty ~f:(fun map coordinate ->
        Map.set map ~key:coordinate ~data:(find_closest coordinate input))
    in
    let infinites =
      let edges =
        List.concat
          [ List.cartesian_product [ bounds.left ] (range bounds.top bounds.bottom)
          ; List.cartesian_product [ bounds.right ] (range bounds.top bounds.bottom)
          ; List.cartesian_product (range bounds.top bounds.bottom) [ bounds.left ]
          ; List.cartesian_product (range bounds.top bounds.bottom) [ bounds.right ]
          ]
      in
      List.filter_map edges ~f:(fun coordinate ->
        Map.find closest_map coordinate
        |> Option.join)
      |> Coordinate.Set.of_list
    in
    Map.filter_map closest_map ~f:(
      Option.value_map ~default:None ~f:(fun closest ->
        match not (Set.mem infinites closest) with
        | true -> Some closest
        | false -> None))
    |> Map.to_alist
    |> List.map ~f:Tuple2.swap
    |> Coordinate.Map.of_alist_multi
    |> Map.data
    |> List.map ~f:List.length
    |> List.max_elt ~compare
    |> Option.value_exn

end

module Part02 = struct
  include Common

  let part = 2

  let solve input =
    let range = List.range ~stop:`inclusive in
    let bounds = Bounds.of_coordinates input in
    let all_coordinates =
      List.cartesian_product
        (range bounds.left bounds.right)
        (range bounds.top bounds.bottom)
    in
    List.count all_coordinates ~f:(fun coordinate ->
      List.sum (module Int) input ~f:(distance coordinate) < 10000)
end

let parts : (module Solution.Part.Basic) list =
  [ (module Part01)
  ; (module Part02)
  ]
