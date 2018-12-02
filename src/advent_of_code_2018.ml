open! Core
open! Import

let days : (module Day) list =
  [ (module Day01) 
  ; (module Day02)
  ]

let command =
  Command.group
    ~summary:"Solve a selected puzzle"
    (List.map days ~f:make_day_command)
