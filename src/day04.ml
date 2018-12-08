open! Import

let date = 4

module What_happened = struct
  type t =
    | Guard_started of int
    | Slept
    | Woke_up
  [@@deriving sexp]

  let get_guard_exn t =
    match t with
    | Guard_started n -> n
    | Slept | Woke_up -> failwith "expected guard"

  let integer =
    let open Angstrom in
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

  let parser =
    let open Angstrom in
    let parse_guard =
      string "Guard #" *> integer <* string " begins shift" >>| (fun x -> Guard_started x)
    in
    let parse_slept =
      string "falls asleep" *> return Slept
    in
    let parse_woke_up =
      string "wakes up" *> return Woke_up
    in
    choice [ parse_guard; parse_slept; parse_woke_up ]
end

module Event = struct
  type t =
    { time : Time.t
    ; what_happened : What_happened.t
    }
  [@@deriving fields, sexp]

  let parser =
    let open Angstrom in
    let brackets t = char '[' *> t <* char ']' in
    let parse_time =
      lift2
        (fun date_string time_string ->
           Time.of_date_ofday ~zone:Time.Zone.utc
             (Date.of_string date_string)
             (Time.Ofday.of_string time_string))
        (take 10)
        (char ' ' *> take 5)
    in
    lift2
      (fun time what_happened -> { time; what_happened })
      (brackets parse_time)
      (char ' ' *> What_happened.parser <* char '\n')

  let minute_of_time time =
    Time.to_ofday time ~zone:Time.Zone.utc
    |> Time.Ofday.to_parts
    |> fun { Time.Span.Parts.min ; _ } -> min

  let to_intervals events =
    let current_guard =
      List.hd_exn events
      |> what_happened
      |> What_happened.get_guard_exn
    in
    let rec loop current_guard last_time (events : t list) acc =
      match events with
      | [] -> acc
      | next_event :: rest ->
        match next_event.what_happened with
        | Guard_started n ->
          assert (Option.is_none last_time);
          loop n None rest acc
        | Slept ->
          assert (Option.is_none last_time);
          loop current_guard (Some next_event.time) rest acc
        | Woke_up ->
          match last_time with
          | None -> failwith "expected open interval"
          | Some starting_time ->
            let interval = (minute_of_time starting_time, minute_of_time next_event.time) in
            loop current_guard None rest ((current_guard, interval) :: acc)
    in
    loop current_guard None events []
end

module Common = struct
  module Input = Make_parseable (Event)
  module Output = Int
end

let get_intervals_by_guards input =
  let sorted_input =
    List.sort input ~compare:(Comparable.lift Time.compare ~f:Event.time)
  in
  Event.to_intervals sorted_input

let coverage_by_minute spans =
  let map =
    List.range 0 60
    |> List.map ~f:(fun x -> x, 0)
    |> Int.Map.of_alist_exn
  in
  List.fold spans ~init:map ~f:(fun map (start, stop) ->
      List.range start stop
      |> List.fold ~init:map ~f:(Map.update ~f:(function
          | None -> assert false
          | Some n -> n + 1)))

module Part01 = struct
  include Common
  let part = 1

  let score_interval (start, stop) = stop - start

  let solve input =
    let intervals_by_guards =
      get_intervals_by_guards input
      |> Int.Map.of_alist_multi
      |> Int.Map.to_alist
    in
    let guard, intervals =
      List.max_elt
        intervals_by_guards
        ~compare:(
          Comparable.lift Int.compare
            ~f:(fun (_, spans) ->
                List.sum (module Int) spans ~f:score_interval))
      |> Option.value_exn
    in
    let minute, _ =
      coverage_by_minute intervals
      |> Int.Map.to_alist
      |> List.max_elt ~compare:(Comparable.lift Int.compare ~f:snd)
      |> Option.value_exn
    in
    guard * minute

end

module Part02 = struct
  include Common
  let part = 2

  let solve input =
    let intervals_by_guards = get_intervals_by_guards input in
    let guard, minute =
      List.fold intervals_by_guards ~init:Int_pair.Map.empty ~f:(fun map (guard, (start, stop)) ->
          List.range start stop
          |> List.fold ~init:map ~f:(fun map minute ->
              Int_pair.Map.update map (guard, minute) ~f:(Option.value_map ~default:1 ~f:Int.succ)))
      |> Int_pair.Map.to_alist
      |> List.max_elt ~compare:(Comparable.lift Int.compare ~f:snd)
      |> Option.value_exn
      |> fst
    in
    guard * minute

end

let parts : (module Solution) list =
  [ (module Part01)
  ; (module Part02)
  ]

let%expect_test _ =
  List.iter parts ~f:(test_input date);
  [%expect{|
    30630
    136571 |}]
