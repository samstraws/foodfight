(*open Food*)
open StringDict 
(*RI: there are no foods with the same name in the food list, and player
  has at least one food. *)

type player_name = string


exception Expired_food
exception Invalid_player

type t = {name : player_name;
          foods : Food.t list; 
          mutable current : Food.t} (*TODO: fix food database*)

(**creates a list of food names*)
let food_to_str foods = List.map (fun f -> Food.name f) foods

let rep_ok player = 
  if List.length player.foods = 0 then failwith "No foods"
  else let rec name_acc acc = function 
      | [] -> true
      | h::t -> (not (List.mem h acc)) && name_acc (h::acc) t
    in if (food_to_str player.foods |> name_acc []) then player
    else failwith "Duplicate food name in player" 

let make_player name foods = 
  let get_fst = function 
    | [] -> failwith "No foods"
    | h::t -> h
  in let player = {name = name; foods = foods; current = get_fst foods} 
  in rep_ok player

let lookup_player name = 
  match (StringDict.find_opt name Database.players) with 
  | None -> raise Invalid_player 
  | Some (foods) -> 
    make_player name (List.map Food.lookup_food foods)

let lookup_char n = try StringDict.find n Database.play_num
  with _ -> raise Invalid_player

let name player = player.name

let live_foods player = List.filter (fun f -> Food.health f > 0) player.foods
(*in {player with foods = live_food_lst}.foods*) 

let live_food_names player = food_to_str (live_foods player)

let all_food_names player = food_to_str player.foods

let current_food player = player.current

(*TODO: make more efficient. Simple linear search, 
  but shouldn't be too many foods*)
(**Raises: Invalid_food if food is not in player*)
let str_to_food player name = 
  let rec search = function  
    | [] -> raise Food.Invalid_food
    | h::t -> if Food.name h = name then h else search t 
  in search player.foods

let switch_food player name = 
  let food = (str_to_food player name)
  in if Food.is_expired food then raise Expired_food
  else if player.current = food then raise Food.Invalid_food
  else player.current <- food 

let is_alive player = not (live_foods player = [])
