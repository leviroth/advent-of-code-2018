open! Import

let date = 8

module Node = struct
  type t =
    { metadata : int list
    ; children : t list
    }

  let integer =
    let open Angstrom in
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

  let parser =
    let open Angstrom in
    fix (fun p ->
        integer <* char ' ' >>= fun child_count ->
        integer <* char ' ' >>= fun metadata_count ->
        count child_count p >>= fun children ->
        count
          metadata_count
          (integer <* (skip Char.is_whitespace <|> end_of_input)) >>= fun metadata ->
        return { metadata; children })
end

module Common = struct
  module Input = Input.Make_parseable (Node)
  module Output = Int
end

module Part01 = struct
  include Common

  let part = 1

  let rec solve { Node.metadata; children } =
    List.fold metadata ~init:0 ~f:(+)
    + List.fold children ~init:0 ~f:(fun total child -> total + solve child)

end

module Part02 = struct
  include Common

  let part = 2

  let rec solve { Node.metadata; children } =
    match children with
    | [] -> List.fold metadata ~init:0 ~f:(+)
    | _ ->
      let cache = Int.Table.create () in
      List.fold metadata ~init:0 ~f:(fun total one_based_index ->
          total +
          match one_based_index with
          | 0 -> 0
          | _ ->
            let zero_based_index = one_based_index - 1 in
            match Int.Table.find cache zero_based_index with
            | Some result -> result
            | None ->
              let result =
                List.nth children zero_based_index
                |> Option.map ~f:solve
                |> Option.value ~default:0
              in
              Int.Table.set cache ~key:zero_based_index ~data:result;
              result)

end

let parts : (module Solution.Part.Basic) list =
  [ (module Part01)
  ; (module Part02)
  ]
