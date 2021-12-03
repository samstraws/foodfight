open State
open StringDict

type t = State.t
type food_dict = 
  (string list * Category.category * int * int * int) StringDict.t

type player_dict = 
  string list StringDict.t

type stats = float * float

let run_num = 100 

let init_state = State.init_state

(** initializes players with names of [f1] and [f2], with only one food each*)
let make_food_state f1 f2 = State.init_custom f1 [f1] f2 [f2]

let random_init () = 
  let p_lst = Database.players |> StringDict.bindings |> List.split |> fst
  in let choose_player lst = 
       match Constants.choose_random p_lst with 
       | None -> failwith "Database players are empty"
       | Some p -> p
  in State.init_state (choose_player p_lst) (choose_player p_lst)

(**[get_random dict] selects a random key from the [dict]*)
let get_random dict = 
  let keys = StringDict.bindings dict |> List.split |> fst
  in match Constants.choose_random keys with 
  | None -> failwith "empty player database"
  | Some p -> p

let play_until_win s =
  let rec loop n state =  
    match (State.auto_take_move state) with 
    | Winner (_, player_type) -> (State.get_name state player_type, n + 1)
    | Normal (nst, _) -> loop (n + 1) nst
    | Bad_move -> failwith "Should not be able to take bad moves"
  in loop 0 s

(******************************************************************************
   Get Stats
 *******************************************************************************)
(**[rand_player_state player] makes a random state based on the player*)
let rand_player_state d player = init_state player (get_random d)(*(random_player())*)

(**[rand_food_state food] makes a random food state based on [food]*)
let rand_food_state d food  = make_food_state food (get_random d)(*(random_food())*)

(**run n games with [player] vs. random opponent and returns a tuple with 
   two floats: % win rate and average # rounds*)
let run_games rand_init n player =
  let rec run_acc n acc player = begin
    if n <= 0 then acc 
    else let state = rand_init player
      in let (winner, rounds) = (play_until_win state)
      in if winner = player 
      then run_acc (n - 1) (1 + fst acc, rounds + snd acc) player
      else run_acc (n - 1) (fst acc, rounds + snd acc ) player
  end
  in let (wins, allrnds) = run_acc n (0, 0) player 
  in (float_of_int wins /. float_of_int n *. 100., 
      float_of_int allrnds /. float_of_int n)

let find_player_stats d = run_games (rand_player_state d)
let find_food_stats d = run_games (rand_food_state d)

let get_outcomes get_stat dict = 
  StringDict.(fold (fun k v acc -> add k (get_stat 100 k) acc) dict empty)

let all_player_stats d = get_outcomes (find_player_stats d) d

let all_food_stats d = get_outcomes (find_food_stats d) d

(**compares tuples by win rate*)
let compare_winr (_, (w1, _)) (_, (w2, _)) = compare w1 w2 

let print_stats dict = 
  StringDict.bindings dict
  |> List.sort compare_winr 
  |> List.iter 
    (fun (p, (w, r)) -> 
       print_endline (p ^ " win %: " ^ string_of_float w 
                      ^ " | avg # rounds: " ^ string_of_float r))  

let cmp_worth (_, w1) (_, w2) = compare w1 w2

(**exponential decay function to calculate how useful # times usable is*)
let exp_decay sp x = x *. (0.5 +. 1.4 ** (sp *. x *. -1.))  

let dmg_worth dmg uses = (float_of_int dmg) *. exp_decay 0.5 (float_of_int uses)

let hl_worth hl uses = (float_of_int hl) *. exp_decay 0.2 (float_of_int uses)

(**attempts to calculate the [move]'s worth based on various stats*)
let calc_worth move = 
  let uses = Move.times_usable move
  in let w = 
       match Move.move_type move with 
       | Move.Attack dmg -> dmg_worth dmg uses
       | Heal hl -> hl_worth hl uses
       | Drain drn -> dmg_worth drn uses 
                      +. hl_worth (drn/Constants.drain_div) uses
  in Float.trunc w

let all_move_worths () =
  let mvs = StringDict.bindings Database.moves |> List.split |> fst
            |> List.map Move.lookup_move
  in List.fold_left 
    (fun acc m -> StringDict.add (Move.name m) (calc_worth m) acc) 
    StringDict.empty mvs

let print_move_worths () = 
  StringDict.bindings (all_move_worths()) 
  |> List.sort cmp_worth
  |> List.iter (fun (m, w) -> 
      print_endline ("mv: " ^ m ^ " worth: " ^ string_of_float(w)))