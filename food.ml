open StringDict 
(*open Move*)

(*
AF: [t] represents a Food that has various stats and knows its moves. Each move
the food knows (each potential action it can take in a given turn) is stored as 
a string, int pair in the dictionary under [moves]. The key is the move's name:
the move is stored in constants. The value is the number of uses left of the 
move: it is initialized with the move's [quantity]. 
RI: 0 <= health <= max_health. Each key in [moves] must be in constants. 
For each key move in [moves], the value [v] must satistfy: 0 <= [v] <= the move 
quantity, which is also stored in the move data. defence, offense >= 0.
[offense], [defense], [max_health] must be >= 0. 
*)

type t = {name : string; 
          mutable moves : Move.t list; (*(Move.t * int) StringDict.t;*)
          category : Category.category;  
          mutable health : int; 
          max_health : int;
          defense : int;
          offense : int;
         }

exception Invalid_food

let debug = false 

(**[rep_ok] asserts the RI*)
let rep_ok food = 
  if debug then begin 
    let geqz x = (x >= 0)
    in if (geqz food.health && food.health <= food.max_health)
       && (geqz food.max_health && geqz food.offense && geqz food.defense)
       && not List.(exists (fun m -> not (Move.is_usable m)) food.moves)
    then food
    else failwith "Violate RI"
  end
  else food

let get_mv_list nms = List.map (fun s -> Move.lookup_move s) nms 

let make_food nm mv_lst cat hlth def off = 
  rep_ok {name = nm; moves = mv_lst; category = cat; 
          health = hlth; max_health = hlth; defense = def; offense = off}

let lookup_food name = 
  match (StringDict.find_opt name Database.foods) with 
  | None -> raise Invalid_food (*if other dictionaries, look thru*)
  | Some (mv_nms, cat, hlth, def, off) -> 
    let mv_lst = get_mv_list mv_nms
    in  make_food name mv_lst cat hlth def off

let name food = food.name

let moves_lst food = (*food.moves |> StringDict.bindings |> List.split |> fst*)
  List.map (fun m -> Move.name m) food.moves

let get_move food name = 
  try List.find (fun m -> Move.name m = name) food.moves 
  with Not_found -> raise Move.Invalid_move

let move_uses food name = get_move food name |> Move.uses

let move_usable food name = get_move food name |> Move.is_usable

let health food = food.health

let max_health food = food.max_health

let category food = food.category

let offense food = food.offense

let defense food = food.defense

let is_expired food = (food.health <= 0)

(**[change_health food change] adds [change] to current [food].
   If [health] is greater than [max_health], then [food] health is set to max.
   If [change] would make health drop below 0 then [food] health is set to 0*)
let change_health food change = 
  let food = rep_ok food in
  let health_add = food.health + change
  in begin if health_add >= food.max_health 
    then food.health <- food.max_health
    else if health_add <= 0 then food.health <- 0
    else food.health <- health_add end

(** [take_damage food dmg] reduces health of the [food] by [dmg] *)
let take_damage food dmg = change_health food (~-dmg)

(** [add_health food health] adds health of the [food] by adding [health]*)
let add_health food health = change_health food health

(** [exec_attack f1 f2 move base_dmg] executes simple attack to food*)
let exec_attack f1 f2 move base_dmg = 
  let mult = Move.effect_weight move f2.category 
  in let total_dmg = 
       Constants.damage_calc mult base_dmg f1.offense f2.defense
  in take_damage f2 total_dmg (*deal damage*) 

let exec_drain f1 f2 move drain = 
  let mult = Move.effect_weight move f2.category 
  in let total_dmg = 
       Constants.damage_calc mult drain f1.offense f2.defense
  in let drained_hl = total_dmg / Constants.drain_div
  in take_damage f2 total_dmg; add_health f1 drained_hl

(**[take_move f1 f2 move] carries out the effects of the [move]*)
let take_move f1 f2 move =
  let f1 = rep_ok f1 in let f2 = rep_ok f2 in
  (*let move = get_move f1 move_nm*)
  (*decrement_move f1 move;*)
  if not (Move.is_usable move) then () (*do nothing*)
  else begin
    (Move.decrement move);
    match Move.move_type move with 
    | Attack base_dmg -> exec_attack f1 f2 move base_dmg
    | Heal hl -> (add_health f1 (Constants.vary hl))
    | Drain drn -> exec_drain f1 f2 move drn
  end
