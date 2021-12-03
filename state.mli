(*Represents a state with two players*)
type t 

(**Represents the State of the Game. *)
(*type player_type = One | Two*)
type player_type = One | Two

type state_status = 
  | Winner of Move.t * player_type
  | Normal of t * Move.t option (*new state, move that was recently executed*)
  | Bad_move

(**[init_state p1 p2] initializes a game where [p1] and [p2] are from the 
   Player Database. [p1] is the player that goes first.
   Raises: Invalid_player if invalid*)
val init_state: string -> string -> t

(**[init_state n1 f1 n2 f2] initializes a state where [n1] is the name of 
   player 1 with foods from the Food Database, and player 2 similarly represented
   by [n2], [f2]. Requires: [f1] and [f2] are not empty
   Raises: Invalid_food if Food is Invalid*)
val init_custom: string -> string list -> string -> string list -> t

(**[turn] is One if it is player one's turn. Two if it is player two's turn.*)
val turn: t -> player_type

(**[get_name state player] is the name of the [player] One or Two in State*)
val get_name : t -> player_type -> string 

(**[get_curr_food state] gets the current food of the current player *)
val get_curr_food: t -> Food.t

(**[get_opp_food state] gets the opponent's current food*)
val get_opp_food: t -> Food.t

(**[get_food_options] outputs list of food options to switch to 
   for current player *)
val get_food_options: t -> string list

(**outputs list of all moves, usable or not, for current food in current player*)
val get_all_moves : t -> string list 

(**outputs list of usable move options for current food in current player*)
val get_move_options: t -> string list

(**[get_curr_expire] represents the number of alive foods * total number of
   foods of the current player.*)
val get_curr_alive_total: t -> int * int

(**[get_opp_expire t] represents the number of alive foods, total foods of the
   opponent*)
val get_opp_alive_total: t -> int * int

(**[need_switch state] is whether the player needs to switch out food *)
val need_switch: t -> bool

(**[make_switch state food] tries to switch the current player's food to 
   this food, which counts as one turn. 
   | Winner of player_type if either player is dead
   | Normal of state * None if switch was executed
   | Bad_move if food is expired, try to switch to current food,
    or player does not know food
*)
val make_switch: t -> string -> state_status

(**[make_switch_dead state food] is like make_switch, but doesn't switch 
   current player in the [state], allowing the player to also make a move on
   their turn*)
val make_switch_dead: t -> string -> state_status

(**[make_move state food move] takes the current [state] and makes the current
   player use the [move] of the current food of the player. It returns 
   the status of the attempt to use the [move]. 
   Requires: [need_switch state] is false
   | Winner of t * Move.t * player_type if move kills a Player 
      (player_type is winner)
   | Normal of state * Some move if a turn was taken
   | Bad_move if a move has been used up, or is invalid *)
val make_move: t  -> string -> state_status

(**[auto_take_move state] takes an automatic move*)
val auto_take_move: t -> state_status