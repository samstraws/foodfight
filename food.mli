open Move
(**Represents a single food fighter that knows its health status, base stats,
   and moves it can use*)
type t


(** Raised when [food] is not in current food dicitionary*)
exception Invalid_food

(**[make_food nm move_lst cat hlth def off] is a food with name [nm] and other
   specified stats. It searches for move names from the [move_lst] in Constants
   Requires: [move_lst] contains valid move names, else raises [Invalid_move]*)
val make_food: string -> Move.t list(*string list*) -> Category.category -> 
  int -> int -> int -> t

(**[lookup_food name] looks up food with [name] in Database
   Raises: Invalid_food if food not found.*)
val lookup_food: string -> t 

(**[name food] is the name of the food*)
val name: t -> string

(**[moves_lst food] is the moves the [food] knows*)
val moves_lst: t -> string list

(**[get_move food name] move of [name] move in food. 
   Raises Invalid_move if [name] is not a valid move*)
val get_move: t -> string -> Move.t

(**[move_uses food name] is the number of times the [name] move can be used by 
   food. Raises Invalid_move if [name] is not a valid move*)
val move_uses: t -> string -> int

(**whether move uses > 0 *)
val move_usable: t -> string -> bool

(**[food_health food] is the current health of the [food]. 
   0 <= health <= max health of [food] *)
val health: t -> int

(**[max_health] is the max health of [food]. 
   0 <= health <= max health of [food]*)
val max_health: t -> int 

(**[category t] is the food's category*) 
val category: t -> Category.category

(**[offense t] is the food's offense*)
val offense : t -> int 

(**[defense t] is the food's defense*)
val defense : t -> int

(*val take_damage : t -> int ->  t*)

(** [is_expired t] is whether food is expired or not *)
val is_expired : t -> bool

(**[take_move f1 f2 move] takes in two foods, the attacker [f1] and defender
   [f2], and calculates the result of move specified by [move] on the foods and
   is the new foods.
   If not [Move.is_usable move], do nothing *)
val take_move : t -> t -> Move.t -> unit
