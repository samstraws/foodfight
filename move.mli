open Category
(**[move_info] is the type of move and fields corresponding to that move's 
     effects. Example: [Heal] move would have a [base_heal] status effect.*)
type move_info =
  | Attack of int
  | Heal of int
  | Drain of int

exception Invalid_move

(** Represents a possible move a Food can do in a turn.
      A move can damage the opponent, heal the current Food, 
      or change the stats of either the Food or its opponent. *)
type t

(**[make_move nm desc cat quant info] makes a move with named [nm] with 
   description [desc] and category [cat]. This move can be 
   used [quant] times and has some specific info [dm_info] *)
val make_move: string -> string -> category -> int -> Database.dm_info ->  t

(**[lookup_move nm] looks for the move with [nm] in Database.
   Riases: Invalid_move if move with [nm] does not exist*)
val lookup_move: string -> t

(** [name t] is the move's name *)
val name : t -> string

(** [descript t] is the move's description*) 
val descript : t -> string

(** [move_category t] is the move's category*) 
val category : t -> category

(** [times_usable t] number of times a move can be used, in total *)
val times_usable : t  -> int

(** [uses] is the number of times left the move can be used*)
val uses : t -> int

(**[m_type t] is the other relevant move info*)
val move_type : t -> move_info

(**[is_usable mv] is whether the [mv] can be used in fight*)
val is_usable : t -> bool

(**[decrement] the number of [uses]. Requires: [is_usable move] *)
val decrement : t -> unit

(** [effect_weight t Category.t] computes the weight on the damage 
    dealt by the move against the category*)
val effect_weight : t -> Category.category -> float
