open! Import

let date = 9

module Game_params = struct
  type t =
    { players : int
    ; marbles : int
    }
  [@@deriving fields]

  let integer =
    let open Angstrom in
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

  let parser =
    let open Angstrom in
    lift2
      (fun players marbles -> { players; marbles })
      (integer <* string " players; ")
      (string "last marble is worth " *> integer <* string " points")
end

module Game_state : sig
  type t

  val create : Game_params.t -> t

  val play : t -> int
end = struct
  type t =
    { game_params : Game_params.t
    ; scores : int Int.Table.t
    ; mutable marble_to_place : int
    ; marbles : int Deque.t
    }

  let create game_params =
    let marbles = Deque.create () in
    Deque.enqueue_back marbles 0;
    { game_params
    ; scores = Int.Table.create ()
    ; marble_to_place = 1
    ; marbles
    }

  let normal_move t ~marble ~player:_ =
    Deque.dequeue_front_exn t.marbles
    |> Deque.enqueue_back t.marbles;
    Deque.enqueue_back t.marbles marble

  let special_move t ~marble ~player =
    for _ = 1 to 7 do
      Deque.dequeue_back_exn t.marbles
      |> Deque.enqueue_front t.marbles
    done;
    let back = Deque.dequeue_back_exn t.marbles in
    let score_to_add = back + marble in
    Int.Table.update t.scores player ~f:(Option.value_map ~default:score_to_add ~f:((+) score_to_add));
    Deque.dequeue_front_exn t.marbles
    |> Deque.enqueue_back t.marbles

  let current_player t =
    let { Game_params.players; _ } = t.game_params in
    ((t.marble_to_place + 1) mod players) + 1

  let move t =
    let move_function =
      match t.marble_to_place mod 23 with
      | 0 -> special_move
      | _ -> normal_move
    in
    move_function t ~marble:t.marble_to_place ~player:(current_player t);
    t.marble_to_place <- t.marble_to_place + 1

  let play t =
    for _ = 1 to t.game_params.marbles do
      move t
    done;
    Hashtbl.data t.scores
    |> List.max_elt ~compare
    |> Option.value_exn

end

module Common = struct
  module Input = Input.Make_parseable (Game_params)
  module Output = Int
end

module Part01 = struct
  include Common

  let part = 1

  let solve input =
    Game_state.create input
    |> Game_state.play
end

module Part02 = struct
  include Common

  let part = 2

  let solve (input : Game_params.t) =
    let game_params = { input with marbles = input.marbles * 100 } in
    Part01.solve game_params
end

let parts : (module Solution.Part.Basic) list =
  [ (module Part01)
  ; (module Part02)
  ]
