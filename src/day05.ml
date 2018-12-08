open! Import

let date = 5

module Part01 = struct
  module Input = Converters.String
  module Output = Int

  let part = 1

  let reactive x y =
    Char.equal (Char.uppercase x) (Char.uppercase y)
      && not (Bool.equal (Char.is_uppercase x) (Char.is_uppercase y))

  let reduce_list list =
    let open Doubly_linked in
    let rec loop list maybe_elt =
      match maybe_elt with
      | None -> ()
      | Some elt ->
        match Doubly_linked.next list elt with
        | None -> ()
        | Some next_elt ->
          match reactive (Elt.value elt) (Elt.value next_elt) with
          | true ->
            let elt_for_next_loop =
              match prev list elt with
              | None -> next list next_elt
              | Some e -> Some e
            in
            Doubly_linked.remove list elt;
            Doubly_linked.remove list next_elt;
            loop list elt_for_next_loop
          | false ->
            loop list (Some next_elt)
    in
    loop list (first_elt list)

  let solve input =
    let list =
    String.to_list input
    |> Doubly_linked.of_list
    in
    reduce_list list;
    Doubly_linked.length list

end

module Part02 = struct
  module Input = Converters.String
  module Output = Int

  let part = 2

  let all_letters =
    List.range (Char.to_int 'A') (Char.to_int 'Z') ~stop:`inclusive
    |> List.map ~f:Char.of_int_exn

  let remove_polymers s letter =
    let uppercase_letter = Char.uppercase letter in
    String.filter s ~f:(fun c -> not (Char.equal uppercase_letter (Char.uppercase c)))

  let solve input =
    List.map all_letters ~f:(remove_polymers input)
    |> List.map ~f:Part01.solve
    |> List.min_elt ~compare
    |> Option.value_exn
end

let parts : (module Solution) list =
  [ (module Part01)
  ; (module Part02)
  ]

let%expect_test _ =
  List.iter parts ~f:(test_input date);
  [%expect{|
    11118
    6948 |}]
