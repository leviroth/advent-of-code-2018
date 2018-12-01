open! Core
open! Import

let date = 1

module Input = struct
  type input = int list
  let parse file =
    Sexp.load_sexps_conv_exn file Int.t_of_sexp
end

module Part01 = struct
  include Input
  let part = 1

  let solve input =
    List.fold input ~init:0 ~f:(+)
end

module Part02 = struct
  include Input
  let part = 2

  let solve input =
    let input_cycle = Sequence.cycle_list_exn input in
    Sequence.delayed_fold
      input_cycle
      ~init:(0, Int.Set.empty)
      ~f:(fun (current_frequency, seen_frequencies) change ~k ->
          match Set.mem seen_frequencies current_frequency with
          | true -> current_frequency
          | false ->
            let next_frequency = current_frequency + change in
            let next_seen = Set.add seen_frequencies current_frequency in
            k (next_frequency, next_seen))
      ~finish:fst
end

let parts : (module Solution) list =
  [ (module Part01)
  ; (module Part02)
  ]
