(**  Test Plan
     Modules: 
     - Category: automatically test that relations work in both directions; i.e. 
        if spicy vs. sweet is strong, then sweet vs. spicy is weak
     - Move: auto test all methods using black box testing (only getters/setters)
     - Food: auto test all methods using a mix of black/glass box testing, with 
       focus on the 'take move' component
     - Player: auto test all methods  using black box testing (getters/setters)
     - State: auto test all methods + interactions with each other, but also 
       used manual testing for implementation in Main (hard to test all cases, 
       like switching then using a move in test file)
     - Command: auto test various inputs with glass-box testing (easy to tell 
       what should/should not be outputted)
     - Main + Display: manually play tested edge cases (such as quitting, 
       different game outcomes, etc.) and also had outsiders play-test
     - AutoState: no tests (outputs statistics about games, can't test)

     Correctness: we started by testing the system from the ground up; showing 
       that move, food, and player interact correctly. We then tested State and 
       Command on various isolated game situations before implementing Main, 
       then extensively manually playing Main to check for errors. 
     Note: Constants turns randomization on/off for testing purposes, allowing
       us to test for specific damage pts.
*)
open OUnit2
open Category
open StringDict
open Command

(******************************************************************************
   Category Tests
 ******************************************************************************)
(**[string_of_effect eff] is the string representation of [eff]*)
let string_of_effect = function 
  | Weak -> "Weak"
  | Neutral -> "Neutral"
  | Strong -> "Strong" 

let category_to_string c = 
  match c with
  | Sweet -> "Sweet"
  | Salty -> "Salty"
  | Savory -> "Savory"
  | Spicy -> "Spicy"
  | Sour -> "Sour"
  | Bitter -> "Bitter"
  | Bland -> "Bland"
  | Fresh -> "Fresh"

let category_test name cat1 cat2 expected = 
  name >:: (fun _ -> assert_equal expected 
               (effectiveness cat1 cat2) ~printer:string_of_effect)

let category_tests =  [
  category_test "Spicy vs. Spicy is Neutral" Spicy Spicy Neutral;
  category_test "Bitter vs. Bitter is Neutral" Bitter Bitter Neutral;
  category_test "Sour vs. Savory is Neutral" Sour Savory Neutral;
  category_test "Sweet vs. Bitter is Strong" Sweet Bitter Strong;
  category_test "Bitter vs. Sweet is Weak" Bitter Sweet Weak;
  category_test "Sour vs. Spicy is Strong" Sour Spicy Strong;
  category_test "Salty vs. Savory is Neutral" Salty Savory Neutral;
  category_test "Salty vs. Bitter is Weak" Salty Bitter Weak;
  category_test "Bitter vs. Salty is Strong" Bitter Salty Strong;
  category_test "Spicy vs. Sweet is Strong" Spicy Sweet Strong;
]

(******************************************************************************
   Move Tests
 ******************************************************************************)

let string_of_move_info = function 
  | Move.Attack dmg -> "Attack " ^ (string_of_int dmg)
  | Move.Heal hl -> "Heal " ^ (string_of_int hl)
  | Drain d -> "Drain " ^ (string_of_int d)

let invalid_move_test name f = (*invalid_test Invalid_move*)
  name >:: (fun _ -> f |> assert_raises Move.Invalid_move)

let move_test name move expected f p = 
  name >:: (fun _ -> assert_equal expected (f move) ~printer:p)

let move_name_test name move expected = move_test name move expected Move.name 
    (fun s -> s)

let move_info_test name move expected = move_test name move expected 
    Move.move_type string_of_move_info

let move_category_test name move expected = move_test name move expected 
    Move.category category_to_string

let move_descrip_test name move expected = move_test name move expected 
    Move.descript (fun s -> s)

let move_times_usable_test name move expected = move_test name move expected
    Move.times_usable string_of_int

let move_uses_test name move expected = move_test name move expected 
    Move.uses string_of_int

let move_usable_test name move expected = move_test name move expected 
    Move.is_usable string_of_bool

let move_effective_test name move cat expected = 
  name >:: (fun _ -> assert_equal expected (Move.effect_weight move cat)
               ~printer:string_of_float)

let rec decrement_n n move = 
  if n <= 0 then move
  else (Move.decrement move; decrement_n (n - 1) move)

let bean_blast() = Move.make_move "bean blast" 
    "The glorious smell of yesterday's leftovers" Savory 10 (DAttack 14)

let soy_sauce_smack() = Move.make_move "soy sauce smack" 
    "Unleash the saltiness" Salty 0 (DAttack 30)

let move_tests = [
  move_name_test "name: bean blast" (bean_blast()) "bean blast";
  move_info_test "info: bean blast is simple attack" (bean_blast()) (Attack 14);
  move_category_test "category: soy sauce smack is salty" (soy_sauce_smack()) 
    Salty;
  move_descrip_test "description for bean blast" (bean_blast())
    "The glorious smell of yesterday's leftovers";
  move_times_usable_test "times usable: bean blast is 10" (bean_blast()) 10;
  move_times_usable_test "times usable: soy_sauce_smack is 0" 
    (soy_sauce_smack()) 0;

  move_uses_test "use move 0 times" (bean_blast()) 10;
  move_uses_test "use move 1 time" (decrement_n 1 (bean_blast())) 9;
  move_uses_test "use move 2 times" (decrement_n 2 (bean_blast())) 8;
  move_usable_test "use move 10 times, not usable" 
    (decrement_n 10 (bean_blast())) false; 
  move_usable_test "use move 9 times, usable" (decrement_n 9 (bean_blast())) 
    true;

  (* Move effectiveness test*)
  move_effective_test "move effectiveness: spicy vs. cheesy cheese" 
    (Move.lookup_move "cheesy cheese") Spicy Constants.neutral_mult;
  move_effective_test "move effectiveness: sweet vs. kimchi" 
    (Move.lookup_move "kimchi") Sweet Constants.strong_mult;
  move_effective_test "move effectiveness: bitter vs. salty salt" 
    (Move.lookup_move "salty salt") Bitter Constants.weak_mult; 
]


(******************************************************************************
   Food Tests
 ******************************************************************************)
(** [pp_string s] pretty-prints string [s]. CODE FROM A2*)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. CODE FROM A2*)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
        if n = 100 then acc ^ "..."  (* stop printing long list *)
        else loop (n + 1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

(**[sorted l1] sorts [l1]*)
let sorted l1 = (List.sort compare l1) 

(**[food_name_test name food expected] checks that the [food]'s name matches
   [expected]*)
let food_name_test name food expected = 
  name >:: (fun _ -> assert_equal expected (Food.name food)
               ~printer:(fun s -> s))

(**[moves_lst_test name food expected_output] checks that the list of moves
   in [food] matches the [expected_output]*)
let moves_lst_test name food (expected_output : StringDict.key list) : test = 
  name >:: (fun _ -> 
      assert_equal (sorted expected_output) (sorted (Food.moves_lst food))
        ~printer:(pp_list pp_string))

let health_test name food expected = 
  name >:: (fun _ -> assert_equal expected (Food.health food) 
               ~printer:string_of_int)

let max_health_test name food expected = 
  name >:: (fun _ -> assert_equal expected (Food.max_health food) 
               ~printer:string_of_int)
let food_type_test name food expected = 
  name >:: (fun _ -> assert_equal expected (Food.category food) 
               ~printer:category_to_string)

let expired_test name food expected = 
  name >:: (fun _ -> assert_equal expected (Food.is_expired food)
               ~printer:string_of_bool)

let move_uses_test name (f1, f2) move expected = 
  name >:: (fun _ -> assert_equal expected (Food.move_uses f1 move) 
               ~printer:string_of_int)

let salty_salt() = Move.make_move "salty salt" "Just a pinch" Salty 7 
    (DAttack 20)

let test_food name = 
  match name with 
  | "empty food" -> Food.make_food "empty food" [] Salty 10 2 2
  | "zero food" -> Food.make_food "zero food" [] Bland 0 0 0
  | "savory food" -> Food.make_food "savory food" [(bean_blast());] Savory 10 2 2
  | "salty food"  -> Food.make_food "salty food" [salty_salt()] Salty 100 2 2
  | "tanky food" -> Food.make_food "tanky food" [] Salty 1000000 10 10
  | "dead food" -> Food.make_food "dead food" [salty_salt()] Salty 0 1 1
  | _ -> failwith "not in test foods"
(*
  let test_foods = 
    StringDict.(empty
                |> add "empty food" ([], Salty, 10, 2, 2)
                |> add "zero food" ([], Bland, 0, 0, 0)
                |> add "savory food" (["bean blast"], Savory, 10, 2, 2)
                |> add "salty food" (["salty salt"], Salty, 100, 2, 2)
                |> add "tanky food" ([], Salty, 1000000, 10, 10)
                |> add "dead food" (["salty salt"], Salty, 0, 1, 1)
               )
  in match StringDict.find name test_foods with 
    (moves, cat, hlth, quant, dmg) -> Food.make_food name moves cat hlth quant 
    dmg
*)
let test_take_move f1 f2 name = 
  Food.take_move f1 f2 (Food.get_move f1 name)

(**Result of when [food1] from test dict
   tries to use [move] [n] times on [food2] from test dict. *)
let take_move_n_times n food1 food2 move = 
  let rec move_helper acc f1 f2 = 
    match acc with 
    | 0 -> (f1, f2)
    | x -> begin 
        test_take_move f1 f2 move;
        move_helper (x - 1) f1 f2
      end
  in move_helper n (test_food food1) (test_food food2)

let food_tests = 
  let open Food in 
  [
    food_name_test "empty food name" (test_food "empty food") "empty food";
    food_name_test "Non-empty food name" (lookup_food "mole") "mole";

    moves_lst_test "empty food" (test_food "empty food") [];
    moves_lst_test "burrito, moves: cheesy cheese, bean blast, burrito wrap" 
      (lookup_food "burrito") ["cheesy cheese";"bean blast";"burrito wrap"];
    moves_lst_test "key lime pie, moves: lemon zest, whipped cream, sugar rush"
      (lookup_food "key lime pie")["lemon zest"; "whipped cream"; "sugar rush"];

    move_uses_test "savory food has not used bean blast, 10 uses"
      ((test_food "savory food"), (test_food "empty food")) "bean blast" 10;
    move_uses_test "savory food uses 1 bean blast on empty food, decrement by 1" 
      (take_move_n_times 1 "savory food" "empty food" "bean blast") "bean blast"
      9;

    invalid_move_test "empty food tries to use 'salty salt'"
      (fun () -> move_uses (test_food "empty food") "salty salt");
    (*invalid_move_test "make food with 'bad move' move"
      (fun () -> make_food "bad food" ["bad move"] Bland 10 10 10);*)
    invalid_move_test "move_uses with 'bad move' move"
      (fun () -> move_uses (test_food "empty food") "bad move");
    invalid_move_test "take_move with 'bad move' move" 
      (fun () -> test_take_move (test_food "empty food") 
          (test_food "savory food") "bad move");
    invalid_move_test "take_move, empty_food tries to use 'bean blast' move" 
      (fun () -> test_take_move (test_food "empty food") 
          (test_food "savory food") "bean blast");

    health_test "empty food health" (test_food "empty food") 10;
    health_test "nonempty food health" (test_food "salty food") 100; 

    food_type_test "empty food type" (test_food "empty food") Salty;
    food_type_test "Nonempty food type - salty" (lookup_food "pizza") Salty;
    food_type_test "Nonempty food type - sweet" 
      (lookup_food "mango smoothie") Sweet;
    food_type_test "Nonempty food type - sour" 
      (lookup_food "key lime pie") Sour;
    food_type_test "Nonempty food type - bitter" 
      (lookup_food "kale salad") Bitter;
    food_type_test "Nonempty food type - fresh" (lookup_food "salad") Fresh;

    expired_test "empty food type" (test_food "empty food") false;
    expired_test "Nonempty food type" (lookup_food "salad") false;
    expired_test "savory food attacks empty with bean blast once, expired" 
      (take_move_n_times 1 "savory food" "empty food" "bean blast" |> snd) true;
  ]

let bb_attack = 
  let bb = Move.lookup_move "bean blast" 
  in match Move.move_type bb with 
  | Move.Attack d -> d 
  | _ -> failwith "bean blast no longer simple attack"

let sav_vs_salt = Category.(effectiveness Savory Salty
                            |>  strength_mult)

let n_expired =
  if 100 mod bb_attack = 0 then 100 / bb_attack
  else 1 + 100 / bb_attack

let food_vary_tests = if Constants.variance_on then [] 
  else [
    health_test "expired food has health 0" 
      (take_move_n_times 1 "savory food" "empty food" "bean blast" |> snd) 0;
    health_test "savory food vs salty, savory uses bean blast" 
      (take_move_n_times 1 "savory food" "salty food" "bean blast" |> snd) 
      Constants.(100 - damage_calc sav_vs_salt bb_attack 2 2);
    health_test "savory food vs salty, savory uses bean blast 2 times"
      (take_move_n_times 2 "savory food" "salty food" "bean blast" |> snd)
      Constants.(100 - 2 * damage_calc sav_vs_salt bb_attack 2 2);
    health_test "use bean blast until expired, health is 0"
      (take_move_n_times n_expired "savory food" "salty food" "bean blast" 
       |> snd) 0;
    max_health_test "no move, current health = max health"
      (take_move_n_times 0 "savory food" "empty food" "bean blast" |> snd) 
      ( Food.health (test_food "empty food"));
    max_health_test "expired food, max health is still the same" 
      (test_food "zero food") 0;
    max_health_test "savory food vs salty, savory uses bean blast 4 times 
    max health is the same" 
      (take_move_n_times 4 "savory food" "salty food" "bean blast" |> snd) 100;
  ]
(******************************************************************************
   Player Tests
 ******************************************************************************)

let string_of_life bool = if bool then "Alive" else "Dead"

let player_test f p name player expected = name >:: (fun _ -> 
    assert_equal expected (f player) ~printer: p)

let player_name_test= player_test Player.name (fun s -> s)

let player_live_test = player_test Player.is_alive string_of_life

let player_food_list_test = player_test Player.live_food_names (pp_list pp_string)

let player_all_foods_test = player_test Player.all_food_names (pp_list pp_string)

let player_current_test name player expected =
  name >:: (fun _ -> assert_equal expected (Player.current_food player 
                                            |> Food.name) ~printer:(fun s -> s))


(*let player_switch_test name player food_name expected = 
  (switch_food player Food.name;
  player_current_test name player expected*)

let invalid_food_test name f = 
  name >:: (fun _ -> f |> assert_raises Food.Invalid_food)

let switch_to_expired_test name player food_name = 
  name >:: (fun _ -> assert_raises Player.Expired_food 
               (fun () -> Player.switch_food player food_name))


let player_switch_test name f1 switch= 
  let test_lst = List.map (fun str -> test_food str) f1 
  in let player = Player.make_player "" test_lst
  in Player.switch_food player switch; 
  name >:: (fun _ -> assert_equal switch (Player.current_food player 
                                          |> Food.name)) 

let dead_food = test_food "dead food"
let salty_food = test_food "salty food"
let savory_food = test_food "savory food"
let test_player = Player.make_player "test" [dead_food]
let player_two = Player.make_player "player two" [dead_food; salty_food; 
                                                  savory_food]

let player_tests = [
  player_name_test "player: test " test_player "test";
  player_name_test "player: player two" player_two "player two";

  (*player_food_list_test "cheese gang food list" (lookup_player "cheese gang")
    ["pizza"; "cheese cake"; "burrito"];*)
  player_food_list_test "test player food list" test_player [];
  player_food_list_test "test one expired, two live" 
    player_two ["salty food"; "savory food"];
  player_all_foods_test "player two should have all three foods"
    player_two ["dead food"; "salty food"; "savory food"];
  player_all_foods_test "test player has dead food" test_player ["dead food"];

  player_switch_test "switch out foods, dead -> savory" 
    ["dead food"; "savory food"] "savory food";
  player_switch_test "switch out foods, savory -> salty"
    ["savory food"; "salty food"] "salty food";
  invalid_food_test "switch to same food raises invalid food"
    (fun () -> player_switch_test "switch to same food, savory -> savory"
        ["savory food"; "salty food"] "savory food");
  switch_to_expired_test "try to switch from savory to dead"
    (Player.make_player "" [savory_food; dead_food]) "dead food";

  player_current_test "current should be first food" player_two "dead food";
  player_current_test "current should be only food" test_player "dead food";
  player_current_test "current of health hammer" 
    (Player.lookup_player "health hammer") 
    (StringDict.find "health hammer" Database.players |> List.hd);

  switch_to_expired_test "try to switch player two back to dead food" 
    player_two "dead food";

  player_live_test "player: health hammer" (Player.lookup_player "health hammer") 
    true;
  player_live_test "player: test" test_player false;
  player_live_test "player two" player_two true; 
]

(******************************************************************************
   State Tests
 ******************************************************************************)

(*let init_state_test name p1 p2 expected = name >:: (fun _ -> 
    assert_equal expected (init_state p1 p2))*)

let player_str = function 
  | State.One -> "Player One"
  | Two -> "Player Two"

let state_str = function 
  | State.Winner ( _, player) -> "Winner: " ^ (player_str player)
  | Normal (_,_) -> "Normal"
  | Bad_move -> "Bad move"

(*TODO: try to test? can't figure out how*)
(*let need_switch_test name st expected = 
  name >:: (fun _ -> assert_equal expected (need_switch st))*)

let turn_test name st expected = 
  name >:: (fun _ -> assert_equal expected (State.turn st) ~printer: player_str)

let invalid_player_test name f = (*invalid_test Invalid_move*)
  name >:: (fun _ -> f |> assert_raises Player.Invalid_player)

let name_test name state plyr expected = 
  name >:: (fun _ -> assert_equal expected (State.get_name state plyr)
               ~printer: (fun s -> s))

(**Returns the normal state from the state status, if it is normal*)
let get_state = function 
  | State.Normal (state, _) -> state
  | _ -> failwith "abnormal state"

(**[select_random_food state] selects a random food from the alive foods
   of the current player in [state]*)
let select_random_food state = 
  let options = State.get_food_options state
  in match Constants.choose_random options with 
  | None -> failwith "No foods in state"
  | Some f -> f

(**[switch_food n state] switches the food [n] times in the given state*)
let rec switch_food n state = 
  if n <= 0 then state
  else 
    let next = State.make_switch state (select_random_food state) |> get_state 
    in switch_food (n - 1) next

let switch_test name state n expected = 
  let final = switch_food n state 
  in turn_test name final expected

let curr_food_test name state expected = 
  name >:: (fun _ -> assert_equal expected (State.get_curr_food state 
                                            |> Food.name)
               ~printer: (fun s -> s))

let normal_switch_food_test name state switch expected = 
  let state = State.make_switch state switch |> get_state
  in name >:: (fun _ -> assert_equal expected 
                  (State.get_opp_food state |> Food.name) ~printer: (fun s -> s))

let bad_move_switch_test name state food = 
  name >:: (fun _ -> assert_equal State.Bad_move (State.make_switch state food))

let str_lst_comp_test f name state expected = 
  name >:: (fun _ -> assert_equal (List.sort compare expected) 
               (f state |> List.sort compare) 
               ~printer:(pp_list (fun s -> s)))

let food_options_test = str_lst_comp_test State.get_food_options
let all_moves_test = str_lst_comp_test State.get_all_moves
let move_options_test = str_lst_comp_test State.get_move_options

let make_move_test name state move expected = 
  name >:: (fun _ -> assert_equal expected (State.make_move state move)
               ~printer:state_str)

let test_state() = 
  (State.init_custom "Freddy Fazbear's" ["pizza"; "salad"; "pretzels"]
     "Jacksonville Jaguars" ["key lime pie"; "mole"; "mango smoothie"])

(*TODO: FINISH TESTING STATE*)
let state_tests = let open State in [
    turn_test "initialize state, player one's turn" 
      (State.init_state "health hammer" "sugar demon") One;
    turn_test "make custom player" (test_state()) One;
    turn_test "auto take move" (test_state() |> auto_take_move |> get_state) Two;

    name_test "custom name Freedy" (test_state()) One "Freddy Fazbear's";
    name_test "custom name Jacksonville" (test_state()) Two "Jacksonville Jaguars";

    curr_food_test "Freddy's food is pizza" (test_state()) "pizza";
    normal_switch_food_test "switch food from options, should be curr food" 
      (test_state()) "salad" "salad";
    bad_move_switch_test "switch first food to first food, should be bad move"
      (test_state()) "pizza";

    food_options_test "Freddy's food options" 
      (test_state()) ["salad"; "pretzels"];
    all_moves_test "All pizza's moves" (test_state()) 
      (Food.lookup_food "pizza" |> Food.moves_lst);
    move_options_test "equal to moves" (test_state())
      (get_all_moves (test_state()));

    invalid_player_test "initialize state raises P1 Invalid player"
      (fun () -> init_state "bad player 1" "health hammer");
    invalid_player_test "initialize state raises P2 Invalid player"
      (fun () -> init_state "health hammer" "bad player 1");
    invalid_player_test "initialize state raises P1 and P2 Invalid player"
      (fun () -> init_state "bad player 1" "sketchy player");
    (*invalid_food_test "initialize custom raises Invalid food"
      (fun () -> init_custom "Nikha" ["grean been"] "Taerim" []);*)
    switch_test "switch out once, curr player two" 
      (State.init_state "health hammer" "sugar demon") 1 Two;
    switch_test "switch out ten times, curr player one"
      (State.init_state "health hammer" "sugar demon") 10 One;
    switch_test "switch out 101 times, curr player two"
      (test_state()) 101 Two;

  ]

(******************************************************************************
   Command Tests
 ******************************************************************************)

let parse_food_test name str expected = 
  name >:: (fun _ -> assert_equal expected (parse_food str))

let parse_move_test name str expected = 
  name >:: (fun _ -> assert_equal expected (parse_move str))

let parse_player_test name str expected = 
  name >:: (fun _ -> assert_equal expected (parse_player str))

let parse_yes_no_test name str expected = 
  name >:: (fun _ -> assert_equal expected (parse_yes_no str))

let command_tests = [
  parse_food_test "testing quit" "quit" Quit;
  parse_food_test "food lower with normal spacing" "mole" (Output "mole");
  parse_food_test "food lower extra spaces on edges" "     kale salad     "
    (Output "kale salad");
  parse_food_test "food upper normal spacing" "MOLE" (Output "mole");
  parse_food_test "food mixed upper lower normal spacing" "MoLe" (Output "mole");
  parse_food_test "food mixed upper lower multiple words" "KaLE SaLAd" 
    (Output "kale salad");
  parse_food_test "food mixed upper lower extra spaces" 
    "       KaLE SaLAD         " (Output "kale salad");
  parse_food_test "Non existant food" "Aloo" Bad_input;
  parse_food_test "Mispelled food" "Cale Salad" Bad_input;

  parse_move_test "testing quit" "quit" Quit;
  parse_move_test "testing back" "back" Back;
  parse_move_test "move lower with normal spacing" "vinegar" (Output "vinegar");
  parse_move_test "move lower extra spaces on edges" "       vinegar       " 
    (Output "vinegar");
  parse_move_test "move lower extra spaces on edges multi word" 
    "       orange juice       " (Output "orange juice");
  parse_move_test "move upper normal spacing" "LETTUCE WRAP" 
    (Output "lettuce wrap");
  parse_move_test "move mixed upper lower normal spacing" "VinEGaR" 
    (Output "vinegar");
  parse_move_test "move mixed upper lower normal spacing multi words" 
    "LEttUcE WrAP" (Output "lettuce wrap");
  parse_move_test "move mixed upper lower extra spaces" 
    "       LeTTucE WraP         " (Output "lettuce wrap");
  parse_move_test "invalid move" "knuckle sandwhich" Bad_input;
  parse_move_test "mispelled move" "vinegarr" Bad_input;

  parse_player_test "testing quit" "quit" Quit;
  parse_player_test "player lower with normal spacing" "b" (Output "sugar demon");
  parse_player_test "player lower extra spaces on edges" "     i     "
    (Output "cheese gang");
  parse_player_test "player upper normal spacing" "D" 
    (Output "health hammer");
  parse_player_test "player mixed upper lower normal spacing" "b" 
    (Output "sugar demon");
  parse_player_test "player mixed upper lower single word" "A" 
    (Output "vegiefruitarian") ;
  parse_player_test "player mixed upper lower extra spaces" 
    "       D         " (Output "health hammer");
  parse_player_test "Non existant player" "Aloo" Bad_input;

  parse_yes_no_test "Empty str is redo" "" Redo;
  parse_yes_no_test "weird is redo" "weird" Redo;
  parse_yes_no_test "Yes is Yes" "Yes" Yes;
  parse_yes_no_test "yes is Yes" "yes" Yes;
  parse_yes_no_test "no is No" "no" No;
]

let tests = 
  "test suite for Command" >::: List.flatten [
    category_tests;
    move_tests;
    food_tests;
    food_vary_tests;
    player_tests;
    command_tests;
    state_tests;
  ]

let _ = run_test_tt_main tests; 
  if Constants.stats_on
  then begin 
    (*TODO: fix when AutoState is modified*)
    (*print_endline "Player outcomes: ";
      AutoState.(all_player_stats() |> print_stats);
      print_endline "\n\nFood outcomes: ";
      AutoState.(all_food_stats() |> print_stats);
      print_endline "\n\nMove estimated worth: ";
      AutoState.print_move_worths();*)
  end