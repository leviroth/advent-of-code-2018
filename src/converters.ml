open! Core

module Int_list = struct
  type t = int list

  let load file =
    Sexp.load_sexps_conv_exn file Int.t_of_sexp
end
