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

module Part02 = struct
  module Input = Make_parseable (Dependency)
  module Output = Int

  let part = 2

  let required_time c =
    61 + (Char.to_int c) - (Char.to_int 'A')

  let downward_graph input =
    let dependencies =
      List.map input ~f:(fun (_, x) -> x, [])
      |> Char.Map.of_alist_reduce ~f:(fun _ _ -> [])
    in
    List.fold
      input
      ~init:dependencies
      ~f:(fun map (key, data) -> Map.add_multi map ~key ~data)
    |> Map.map ~f:(fun l -> Char.Set.of_list l)

  let set_diff_list set list =
    List.fold list ~init:set ~f:Set.remove

  let take_n_from_heap heap n =
    let rec aux heap n acc =
      match n with
      | 0 -> (List.rev acc, heap)
      | _ ->
        match Fheap.pop heap with
        | None -> (List.rev acc, heap)
        | Some (head, rest) ->
          aux rest (n - 1) (head :: acc)
    in
    aux heap n []

  let remove_dependencies finished_jobs graph =
    Map.partition_map graph ~f:(fun dependencies ->
        let new_dependencies = set_diff_list dependencies finished_jobs in
        match Set.is_empty new_dependencies with
        | true -> `Fst ()
        | false -> `Snd new_dependencies)
    |> Tuple2.map_fst ~f:Map.keys

  let finished_jobs time in_progress =
    let rec loop in_progress acc =
      match Fheap.pop in_progress with
      | None -> in_progress, acc
      | Some ((finish_time, job), rest) ->
        match time >= finish_time with
        | false -> in_progress, acc
        | true -> loop rest (job :: acc)
    in
    loop in_progress []

  let start_jobs time in_progress ready =
    let available_workers = 5 - (Fheap.length in_progress) in
    let to_add, still_ready = take_n_from_heap ready available_workers in
    let now_in_progress =
      List.map to_add ~f:(fun job -> time + required_time job, job)
      |> List.fold ~init:in_progress ~f:Fheap.add
    in
    now_in_progress, still_ready

  let rec step time in_progress ready graph =
    let still_in_progress, finished_jobs = finished_jobs time in_progress in
    let newly_ready_jobs, new_graph = remove_dependencies finished_jobs graph in
    let ready = List.fold newly_ready_jobs ~init:ready ~f:Fheap.add in
    let now_in_progress, still_ready = start_jobs time still_in_progress ready in
    match Fheap.is_empty now_in_progress with
    | true -> time
    | false ->
      let new_time = fst (Fheap.top_exn now_in_progress) in
      step new_time now_in_progress still_ready new_graph
  ;;

  let solve input =
    let job_heap = Fheap.create ~cmp:[%compare: int * char] in
    let ready_heap = Fheap.create ~cmp:Char.compare in
    let ready_jobs, starting_graph = remove_dependencies [] (downward_graph input) in
    let ready = List.fold ready_jobs ~init:ready_heap ~f:Fheap.add in
    let in_progress, ready = start_jobs 0 job_heap ready in
    let new_time = fst (Fheap.top_exn in_progress) in
    step new_time in_progress ready starting_graph

end

let parts : (module Solution) list =
  [ (module Part01)
  ; (module Part02)
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
  [%expect{|
    CABDFE
    253 |}]
