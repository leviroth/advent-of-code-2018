open! Core
open! Import

let date = 1

module Part1 = struct
  type input = int list

  let part = 1

  let parse file =
    Sexp.load_sexps_conv_exn file Int.t_of_sexp

  let solve input =
    List.fold input ~init:0 ~f:(+)
end

let parts : (module Solution) list =
  [ (module Part1) ]
