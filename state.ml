
(** Represents the type of the game state
    AF: {p1; p2} represents the game between player1 and player2 that have names
    [p1] and [p2] respectively. Player1 gets to go first. Curr_player is either 
    One or Two and represents who's turn it is
*)

type player_type = One | Two

type t = {p1 : Player.t;
          p2 : Player.t;
          curr_player : player_type;}

type state_status = 
  | Winner of Move.t * player_type
  | Normal of t * Move.t option (*new state, move that was recently executed*)
  | Bad_move

let init_state player1 player2 = 
  {p1 = Player.lookup_player player1; 
   p2 = Player.lookup_player player2; 
   curr_player = One;}

let names_to_foods names = List.map Food.lookup_food names

let init_custom n1 f1 n2 f2 = 
  assert (List.length f1 > 0 && List.length f2 > 0);
  let foods1 = names_to_foods f1 
  in let foods2 = names_to_foods f2 
  in {p1 = Player.make_player n1 foods1; 
      p2 = Player.make_player n2 foods2;
      curr_player = One;}

let turn s = s.curr_player 

let opposite = function
  | One -> Two 
  | Two -> One

let switch_curr state = 
  {state with curr_player = opposite state.curr_player}

let get_fst = function 
  | [] -> failwith "No foods"
  | h::t -> h

(**is the tuple representing the (player, opp) for the round *)
let get_roles state = match state.curr_player with 
  | One ->  (state.p1, state.p2)
  | Two -> (state.p2, state.p1)

let curr_food_helper state f = 
  let roles =  get_roles state 
  in Player.current_food (f roles) 

let get_curr_food state = curr_food_helper state fst

let get_opp_food state = curr_food_helper state snd

let get_name state = function 
  | One -> state.p1 |> Player.name
  | Two -> state.p2 |> Player.name

let get_curr_food state = let roles =  get_roles state in
  Player.current_food (fst roles) 

let get_food_options state = 
  let foods = match state.curr_player with
    | One -> state.p1 |> Player.live_food_names 
    | Two -> state.p2 |> Player.live_food_names
  in List.filter (fun f -> not (f = Food.name (get_curr_food state))) foods

let get_all_moves state = 
  match state.curr_player with
  | One -> state.p1 |> Player.current_food |> Food.moves_lst
  | Two -> state.p2 |> Player.current_food |> Food.moves_lst

let get_move_options state = 
  let food = get_curr_food state
  in get_all_moves state 
     |> List.filter (fun m -> (Food.move_usable food m))

let alive_total player = 
  (List.length (Player.live_food_names player), 
   List.length (Player.all_food_names player))

let get_curr_alive_total state = 
  let player = match state.curr_player with
    | One -> state.p1 
    | Two -> state.p2
  in alive_total player

let get_opp_alive_total state = 
  let opp = 
    match state.curr_player with
    | One -> state.p2 
    | Two -> state.p1 
  in alive_total opp

(*If [player] current food is expired, switches current to a random good food. 
  Else, just returns current food*)
(*let possible_switch player = if Food.is_expired (Player.current_food player) 
  then
    match Player.live_food_names player with
    | [] -> failwith "impossible"
    | h::t -> Player.switch_food player h; Player.current_food player
  else Player.current_food player*)

(**gets the winner of the game, if there is one*)
let opt_winner state = 
  if not (Player.is_alive state.p1) then Some Two
  else if not (Player.is_alive state.p2) then Some One
  else None

(**[need_switch state] checks if the current player needs to switch food
   If true, either food expired or no valid moves to use *)
let need_switch state = 
  let player = fst (get_roles state) 
  in Food.is_expired (Player.current_food player) 
     || get_move_options state = []

(*[exec_switch may_switch state food_name] tries to switch the curr food of curr
  player to [food_name]. [may_switch_plyr] might switch curr player*)
let exec_switch may_switch_plyr state food_name = 
  let roles =  get_roles state in
  try Player.switch_food (fst roles) food_name; 
    Normal (may_switch_plyr state, None)
  with Player.Expired_food | Food.Invalid_food -> Bad_move 

(**[exec_move state move_name] tries to have the current player use current food
   to execute the move with [move_name] *)
let exec_move state move_name = 
  let f2 = get_opp_food state (*possible_switch opp*) 
  in let f1 = get_curr_food state
  in try begin
    if need_switch state then Bad_move 
    else if not (Food.move_usable f1 move_name) then Bad_move
    else let move = (Food.get_move f1 move_name) 
      in Food.take_move f1 f2 move;
      match opt_winner state with 
      | None -> Normal ((switch_curr state), Some move)
      | Some p -> Winner (move, p)
  end
  with Move.Invalid_move -> Bad_move

let make_switch_dead = exec_switch (fun s -> s)
let make_switch =  exec_switch switch_curr
let make_move state str = 
  assert (not (need_switch state)); 
  exec_move state str 

(******************************************************************************
   Automated Players
 ******************************************************************************)
module type AutoPlayer = sig 
  (*Takes a move automatically in a two player State*)
  val auto_take_move : t -> state_status
end 

module DumbAuto : AutoPlayer = struct
  (*chance to switch food*)
  let switch_chance = 4

  let auto_food_switch state = 
    let food_opt = get_food_options state |> Constants.choose_random
    in match food_opt with 
    | None -> failwith "Should not call switch if Winner"
    | Some food -> make_switch state food

  let auto_make_move state = 
    let move_opt = get_move_options state |> Constants.choose_random
    in match move_opt with 
    | None -> auto_food_switch state
    | Some move -> make_move state move

  let auto_take_move state = 
    if need_switch state then auto_food_switch state
    else if Random.self_init(); 
      Random.int switch_chance = 0 && List.length (get_food_options state) > 0
    then auto_food_switch state 
    else auto_make_move state
end 

let auto_take_move = DumbAuto.auto_take_move

(*
module type OneAutoStateSig = sig 
  (**Represents a one player game against the computer. Only player 1 can 
     take a move or make a switch; player 2 must only call automated. *)
  include StateSig
  (**[auto_take_move state] takes an auto move. 
     Requires: the current player in the state is player two, the automated 
     player. *)
  val auto_take_move : t -> state_status
end

module OneAutoState : OneAutoStateSig = struct
  include State
  (*shadow to make sure only player one has control*)
  let make_switch state str = 
    assert (turn state = One);
    make_switch state str 

  let make_move state str = 
    assert (turn state = One);
    make_move state str

  let auto_take_move state = 
    assert (turn state = Two);
    AutoPlayer.auto_take_move state
end 

module type FullAutoStateSig = sig 

end

module FullAutoState : FullAutoStateSig = struct

end 
*)