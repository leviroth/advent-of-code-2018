open! Core
open Advent_of_code_2018

let test_specific_part test_case ~day ~part =
  let (module Day) = List.nth_exn days (day - 1) in
  let (module Part) = List.nth_exn Day.parts (part - 1) in
  Part.solve_input test_case
  |> printf "%s\n"

let test_whole_day_on_string (module Day : Solution.Day.S) test_case =
  List.iter Day.parts ~f:(fun (module Part) ->
      Part.solve_input test_case
      |> printf "%s\n")

let%expect_test "Day 01" =
  let part_1_test_cases =
    [ "+1\n-2\n+3\n+1"
    ; "+1\n+1\n+1"
    ; "+1\n+1\n-2"
    ; "-1\n-2\n-3"
    ]
  in
  List.iter part_1_test_cases ~f:(test_specific_part ~day:1 ~part:1);
  [%expect{|
    3
    3
    0
    -6 |}];
  let part_2_test_cases =
    [ "+1\n-2\n+3\n+1"
    ; "+1\n-1"
    ; "+3\n+3\n+4\n-2\n-4"
    ; "-6\n+3\n+8\n+5\n-6"
    ; "+7\n+7\n-2\n-7\n-4"
    ]
  in
  List.iter part_2_test_cases ~f:(test_specific_part ~day:1 ~part:2);
  [%expect{|
    2
    0
    10
    5
    14 |}]
;;

let%expect_test "Day 02" =
  let part_1_test_case =
    {|abcdef
    bababc
    abbcde
    abcccd
    aabcdd
    abcdee
    ababab|}
  in
  test_specific_part part_1_test_case ~day:2 ~part:1;
  [%expect{| 12 |}];
  let part_2_test_case =
    {|abcde
fghij
klmno
pqrst
fguij
axcye
wvxyz|}
  in
  test_specific_part part_2_test_case ~day:2 ~part:2;
  [%expect{| fgij |}];
;;

let%expect_test "Day 03" =
  let test_case =
    {|#1 @ 1,3: 4x4
#2 @ 3,1: 4x4
#3 @ 5,5: 2x2
|}
  in
  test_whole_day_on_string (module Day03) test_case;
  [%expect{|
    4
    3 |}]

let%expect_test "Day 04" =
  let test_case =
    {|[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up
|}
  in
  test_whole_day_on_string (module Day04) test_case;
  [%expect{|
    240
    4455 |}]

let%expect_test "Day 05" =
  let test_case = "dabAcCaCBAcCcaDA" in
  test_whole_day_on_string (module Day05) test_case;
  [%expect{|
    10
    4 |}]

let%expect_test "Day 06" =
  let test_case =
    {|1, 1
1, 6
8, 3
3, 4
5, 5
8, 9
|}
  in
  test_whole_day_on_string (module Day06) test_case;
  [%expect{|
    17
    72 |}]

let%expect_test "Day 07" =
  let test_case = 
    {|Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.
|}
  in
  test_whole_day_on_string (module Day07) test_case;
  [%expect{|
    CABDFE
    253 |}]

let%expect_test "Day 08" =
  let test_case = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2" in
  test_whole_day_on_string (module Day08) test_case;
  [%expect{|
    138
    66 |}]

let%expect_test "Day 09" =
  let test_cases =
    [ "9 players; last marble is worth 25 points"
    ; "10 players; last marble is worth 1618 points"
    ; "13 players; last marble is worth 7999 points"
    ; "17 players; last marble is worth 1104 points"
    ; "21 players; last marble is worth 6111 points"
    ; "30 players; last marble is worth 5807 points"
    ]
  in
  List.iter test_cases ~f:(test_whole_day_on_string (module Day09));
  [%expect{|
    32
    22563
    8317
    74765078
    146373
    1406506154
    2764
    20548882
    54718
    507583214
    37305
    320997431 |}]
