open! Core
open! Import

module Solution = Solution

module Day01 = Solution.Day.Make (Day01)
module Day02 = Solution.Day.Make (Day02)
module Day03 = Solution.Day.Make (Day03)
module Day04 = Solution.Day.Make (Day04)
module Day05 = Solution.Day.Make (Day05)
module Day06 = Solution.Day.Make (Day06)
module Day07 = Solution.Day.Make (Day07)
module Day08 = Solution.Day.Make (Day08)
module Day09 = Solution.Day.Make (Day09)

let days : (module Solution.Day.S) list =
  [ (module Day01)
  ; (module Day02)
  ; (module Day03)
  ; (module Day04)
  ; (module Day05)
  ; (module Day06)
  ; (module Day07)
  ; (module Day08)
  ; (module Day09)
  ]

let command =
  Command.group
    ~summary:"Solve a selected puzzle"
    (List.map days ~f:(fun (module Day) ->
         let name = sprintf "%02d" Day.date in
         name, Day.command))
