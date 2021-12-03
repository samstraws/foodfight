open State 

(**[small_barrier()] prints a small barrier*)
val small_barrier: unit -> unit

(**[welcome()] is a welcome message*)
val print_welcome: unit -> unit

(**[exit ()] is the exit message*)
val print_exit: unit -> unit

(**[print_play_dict] prints the players of Database*)
val print_play_dict : unit -> unit

(**[print_food_dict] prints the foods of Database*)
val print_food_dict : unit -> unit

(**[print_error str] prints the [str] as an error*)
val print_error : string -> unit

(**[message str] pretty prints a [str] to the terminal*)
val message: string -> unit

(**[prompt str] prints the [str] then prompts for user input*)
val prompt : string -> string

(**[simple_prompt str] does the same thing as prompt, different format
   for shorter messages*)
val simple_prompt : string -> string

(**[print_winner state] shows the winner of the [state]*)
val print_winner : State.player_type -> unit

(**[print_usable_foods state] prints the usable foods of the current player*)
val print_usable_foods : State.t -> unit

(**[print_all_moves state] prints the current player's moves*)
val print_all_moves: State.t -> unit

(**[start_round state] prints [state] info at beginning of turn*)
val start_round: State.t -> unit

(**[print_take_move str mv state] prints the outcome of taking a [mv]
   [str] is the player name. [state] is the state after [mv] has been taken, 
   but before the current player is switched*)
val print_take_move : string -> Move.t option -> State.t -> unit



