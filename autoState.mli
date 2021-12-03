open State
open StringDict

(**Represents a fully automated game for testing purposes*)
type t = State.t

type food_dict = 
  (string list * Category.category * int * int * int) StringDict.t

type player_dict = 
  string list StringDict.t

(*represents % win rate, average # of rounds until winner is declared*)
type stats = float * float

(**[init_state str1 str2] is a state with two players named [str1] and [str2] *)
val init_state : string -> string -> t

(**[make_food_state f1 f2] makes a state pitting [f1] against [f2], with players
   that have names [f1] and [f2], respectively*)
val make_food_state: string -> string -> t

(**[random_init ()] randomly initializes a state*)
val random_init : unit -> t

(**[play_until_win state] plays the state until someone wins, then returns the 
   winner's name and number of rounds it took*)
val play_until_win : t -> string * int 

(**[find_food_stats n plyr] runs [n] games with [plyr] vs. random player
   returns stats of [plry]*)
val find_player_stats : 'a StringDict.t -> int -> string -> stats

(**[find_food_stats n food] runs [n] games with [food] vs. random food
   returns stats of [food]*)
val find_food_stats : 'a StringDict.t -> int -> string -> stats

(**[all_player_outcomes ()] is dictionary mapping player name to stats of
   running many games, ordered from lowest % win rate to highest in Database*)
val all_player_stats : player_dict -> stats StringDict.t

(**[all_food_outcomes ()] is dictionary mapping food name to stats of
   running many games, ordered from lowest % win rate to highest in Database*)
val all_food_stats : food_dict -> stats StringDict.t

(**[print_stats dict] prints the outcomes of a given [dict] from lowest
   win rate to highest*)
val print_stats : stats StringDict.t -> unit

(**[all_move_worths()] is a dictionary mapping a move name to an estimate of a 
   move's worth, using the Database of moves*)
val all_move_worths : unit -> float StringDict.t

(**[print_move_worths()] prints rough estimates of all the move's worth
   from lowest worth to greatest*)
val print_move_worths : unit -> unit 