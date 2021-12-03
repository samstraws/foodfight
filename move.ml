open Category

type move_info =
  | Attack of int
  | Heal of int
  | Drain of int

exception Invalid_move

(**AF: A single move is represented by its basic information and information 
   specific to the type of move it is.
   RI: quantity has to be greater or equal to zero. *)
type t = 
  {name : string;
   description : string;
   category : category;
   quantity : int;
   mutable uses: int;
   info: move_info}

(***********************MOVE CONSTRUCTORS****************************)
(**[make_base] is the [base_info] specified by input*)
let make_base nm desc cat quant i = 
  {name = nm;
   description = desc;
   category = cat;
   quantity = quant;
   uses = quant;
   info = i}

let make_move name desc cat quant info =  
  let m_info = match info with 
    | Database.DAttack dmg -> Attack dmg
    | DHeal hlth -> Heal hlth
    | DDrain drn -> Drain drn 
  in make_base name desc cat quant m_info

(**[lookup_move name] looks up the [name] in moves*)
let lookup_move name = 
  let open StringDict.StringDict in 
  match (find_opt name Database.moves) with 
  | Some (d, c, q, i) -> make_move name d c q i
  | None -> raise Invalid_move

(***********************ACCESS BASIC DATA****************************)
let name mv = mv.name

let descript mv = mv.description

let category mv = mv.category

let times_usable mv = mv.quantity

let uses mv = mv.uses

let move_type mv = mv.info

(***********************ACCESS MOVE INFO****************************)
let is_usable move = uses move > 0

let decrement move = 
  if is_usable move then move.uses <- (move.uses - 1)
  else failwith "Tried to decrement a move at 0 uses"

let effect_weight m cat = 
  effectiveness (category m) cat 
  |> Category.strength_mult