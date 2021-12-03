open Food

(*type food_name = string*)
type player_name = string

exception Expired_food
exception Invalid_player

(**Player represents the player's avatar, which has a name and foods that
   the player can use to fight. *)
type t 

(**[initialize_player name foods] creates a player with [name] and fighters 
   [foods]. Requires: there is at least one food in lst. 
   foods must be unique - no repeats.*)
val make_player : player_name -> Food.t list -> t

(**Raises: Invalid_player if not in database.*)
val lookup_player: string -> t

(**Raises: Invalid_player if not in database. Gets player_name corresponding to 
   char*)
val lookup_char: string -> string

(**[name player] is the [player]'s name*)
val name : t -> player_name 

(**[list_of_foods player] is list of the [player]'s live foods *)
(*val list_of_foods : t -> Food.t list*)

(** [live_food_names] returns a list of the alive food names in player *)
val live_food_names : t -> string list

(**[all_food_names t] is the list of all foods in [t]*)
val all_food_names : t -> string list

(**[get_food nm] is the food in the player with [nm]. 
   Raises: Invalid_food if player does not have food.*)
(*val get_food : string -> Food.t*)

(**[current_food player] is the food in play*)
val current_food: t -> Food.t

(**[switch_food player nm] switches current to move with [nm]
   Raises: Invalid_food if [nm] is not a food in player, or try to switch to 
   current food
   Raises: Expired_food if [nm] is expired*)
val switch_food: t -> string -> unit

(**[is_alive player] returns whether the player is alive or dead*)
val is_alive : t -> bool
