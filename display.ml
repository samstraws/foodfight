open Command
open State
(******************************************************************************
   Useful General Methods
 ******************************************************************************)
let print_list lst = 
  print_endline ((List.fold_left (fun s acc -> acc ^ "; " ^ s) "" lst))

(**[print_dict] prints a [StringDict]*)
let print_dict d = 
  let lst = StringDict.StringDict.bindings d in 
  let str = 
    List.fold_left (fun acc (k, v) ->"\n" ^ k ^ ". " ^ v ^ acc) "" (List.rev lst)
  in print_endline str

let print_play_dict () = print_dict Database.play_num

(**[repeat_str n str] is [str] n times. If [n] <= 0, then return empty str*)
let repeat_str n str = 
  let rec str_acc x acc str = 
    if x <= 0 then acc 
    else str_acc (x - 1) (str ^ acc) str
  in str_acc n "" str

(******************************************************************************
   Constants
 ******************************************************************************)
let barrier () = print_endline (repeat_str 20 "\n" ^ 
                                "╠" ^ (repeat_str 80 "═") ^ "╣")
let spacer_1() = (repeat_str 15 "░") ^ " "
let spacer_2() = "░░░ "

let small_barrier () = print_endline(repeat_str 15 "-")
let print_welcome () = 
  print_newline();
  ANSITerminal.(print_string [white; on_blue]
                  (repeat_str 22 " " 
                   ^ "Welcome to the Food Fight!!"
                   ^ repeat_str 22 " "));
  print_newline();
  print_newline();
  print_endline (
    "███████╗░█████╗░░█████╗░██████╗░  ███████╗██╗░██████╗░██╗░░██╗████████╗\n" ^
    "██╔════╝██╔══██╗██╔══██╗██╔══██╗  ██╔════╝██║██╔════╝░██║░░██║╚══██╔══╝\n" ^
    "█████╗░░██║░░██║██║░░██║██║░░██║  █████╗░░██║██║░░██╗░███████║░░░██║░░░\n" ^
    "██╔══╝░░██║░░██║██║░░██║██║░░██║  ██╔══╝░░██║██║░░╚██╗██╔══██║░░░██║░░░\n" ^
    "██║░░░░░╚█████╔╝╚█████╔╝██████╔╝  ██║░░░░░██║╚██████╔╝██║░░██║░░░██║░░░\n" ^
    "╚═╝░░░░░░╚════╝░░╚════╝░╚═════╝░  ╚═╝░░░░░╚═╝░╚═════╝░╚═╝░░╚═╝░░░╚═╝░░░")

let print_exit () = 
  print_endline ("▀▀█▀▀ ░█─░█ ─█▀▀█ ░█▄─░█ ░█─▄▀ ░█▀▀▀█ 　 ░█▀▀▀ ░█▀▀▀█ ░█▀▀█ 　 ░█▀▀█ ░█─── ─█▀▀█ ░█──░█ ▀█▀ ░█▄─░█ ░█▀▀█ \n" ^
                 "─░█── ░█▀▀█ ░█▄▄█ ░█░█░█ ░█▀▄─ ─▀▀▀▄▄ 　 ░█▀▀▀ ░█──░█ ░█▄▄▀ 　 ░█▄▄█ ░█─── ░█▄▄█ ░█▄▄▄█ ░█─ ░█░█░█ ░█─▄▄ \n" ^
                 "─░█── ░█─░█ ░█─░█ ░█──▀█ ░█─░█ ░█▄▄▄█ 　 ░█─── ░█▄▄▄█ ░█─░█ 　 ░█─── ░█▄▄█ ░█─░█ ──░█── ▄█▄ ░█──▀█ ░█▄▄█ \n" ^
                 "                                                                                                         \n"^

                 "░█▀▀█ ░█──░█ ░█▀▀▀ █ \n" ^
                 "░█▀▀▄ ░█▄▄▄█ ░█▀▀▀ ▀ \n" ^
                 "░█▄▄█ ──░█── ░█▄▄▄ ▄")

(**[bar ()] is a bar for formatting*)
let bar () = "\t|| "

(******************************************************************************
   Displaying messages
 ******************************************************************************)

let print_error str = 
  ANSITerminal.(print_string [red] (">>>" ^ str ^ "\n"))

let message str = 
  ANSITerminal.(print_string [cyan] ("\n      ~" ^ str ^ "~      \n"))

let prompt str = 
  ANSITerminal.(print_string [yellow] ("\n" ^ str ^ "\n"));
  print_string ("> ");
  read_line()

let simple_prompt str = 
  ANSITerminal.(print_string [yellow] (str ^ ": "));
  (*let out = read_line()
    in print_newline(); out*)
  read_line()
(******************************************************************************
   Game State Displays
 ******************************************************************************)
(*str of a turn*)
let turn_str = function
  | State.One -> "Player One"
  | State.Two -> "Player Two" 

let print_winner = barrier(); 
  function 
  | State.One -> 
    print_endline 
      ("░█▀▀█ ░█─── ─█▀▀█ ░█──░█ ░█▀▀▀ ░█▀▀█ 　 ▄█─ 　 ░█──░█ ░█▀▀▀█ ░█▄─░█ \n"^
       "░█▄▄█ ░█─── ░█▄▄█ ░█▄▄▄█ ░█▀▀▀ ░█▄▄▀ 　 ─█─ 　 ░█░█░█ ░█──░█ ░█░█░█ \n"^
       "░█─── ░█▄▄█ ░█─░█ ──░█── ░█▄▄▄ ░█─░█ 　 ▄█▄ 　 ░█▄▀▄█ ░█▄▄▄█ ░█──▀█\n"^
       "                                                                     ");
    print_exit ()
  | Two -> 
    print_endline 
      ("░█▀▀█ ░█─── ─█▀▀█ ░█──░█ ░█▀▀▀ ░█▀▀█ 　 █▀█ 　 ░█──░█ ░█▀▀▀█ ░█▄─░█ \n"^
       "░█▄▄█ ░█─── ░█▄▄█ ░█▄▄▄█ ░█▀▀▀ ░█▄▄▀ 　 ─▄▀ 　 ░█░█░█ ░█──░█ ░█░█░█ \n"^
       "░█─── ░█▄▄█ ░█─░█ ──░█── ░█▄▄▄ ░█─░█ 　 █▄▄ 　 ░█▄▀▄█ ░█▄▄▄█ ░█──▀█\n"^
       "                                                                     ");
    print_exit ()


(******************************************************************************
   Move Display
 ******************************************************************************)
(*[print_move_info info] is the extra [info] of the move*)
let print_move_info info = 
  let open ANSITerminal in 
  match info with 
  | Move.Attack dmg ->
    print_string [default]("Dmg: " ^ string_of_int dmg)
  | Heal hl -> 
    print_string [green] ("Heal: " ^ string_of_int hl)
  | Drain drn -> begin
      print_string [default] ("Dmg: " ^ string_of_int drn);
      print_string [green] (" + Drain: " 
                            ^ string_of_int (drn/Constants.drain_div))
    end

(**[print_cat c] prints the category*)
let print_category c = 
  let open ANSITerminal in 
  (*print_string [default] "Type: ";*)
  match c with 
  | Category.Spicy -> print_string [on_red] "Spicy"
  | Sour -> print_string [black; on_yellow] "Sour "
  | Savory -> print_string [on_blue] "Savory"
  | Bland -> print_string [black; on_white] "Bland"
  | Salty -> print_string [black; on_cyan] "Salty"
  | Sweet -> print_string [on_magenta] "Sweet"
  | Fresh -> print_string [black; on_green] "Fresh"
  | Bitter -> print_string [blue; on_white] "Bitter" 

(**[print_move food name] prints info about a move of [name] in [food].
   Requires: [name] is a valid move in food*)
let print_move food name = 
  let move = Food.get_move food name 
  in let uses = Food.move_uses food name
  in let extra_tab = if Move.name move |> String.length <= 7 then 
         "\t" else ""
  in print_string 
    ("▶ Move: " ^ Move.name move ^ extra_tab ^ bar());
  print_category (Move.category move);
  print_string (bar() ^ "Uses: " ^ (string_of_int uses) ^ "/" 
                ^ string_of_int (Move.times_usable move) ^ bar());
  print_move_info (Move.move_type move); 
  print_endline ""


(*prints info about the current moves in curr food of curr player *)
let print_all_moves state = 
  print_endline "Your Moves: ";
  let food = State.get_curr_food state in 
  let move_names = State.get_all_moves state
  in let rec print_moves = function 
      | [] -> ()
      | h::t -> print_move food h; print_moves t
  in print_moves move_names

(**[get_effect move opp] gets the [Category.effect] of the 
   [move] on the [opp] food*)
let get_effect move opp = 
  Category.effectiveness (Move.category move) (Food.category opp)

(**[print_effect eff] prints flavor-text for the [eff] of a move*)
let print_effect = function 
  | Category.Weak -> message "It wasn't very effective..."
  | Category.Neutral -> ()
  | Category.Strong -> message "It was super effective!"

(******************************************************************************
   Food Display
 ******************************************************************************)
(**[print_health_bar health max] prints a health bar. 
   Case 1: [health] >= 2/3 [max] -> green health bar
   Case 2: 2/3 [max] > current health >= 1/3 [max] -> yellow health bar
   Case 3: 1/3 [max] > current health -> red health bar
   Requires: health >= 0*)
let print_health_bar health max = 
  let fill = (float_of_int health) /. (float_of_int max) *. 25.
  in let round_up fl = floor(fl +. 0.5) |> int_of_float
  in let diff = 25 - (round_up fill)
  in let health_bar = "║" ^ (repeat_str (round_up fill) "█") 
                      ^ (repeat_str diff "░") ^ "║ " 
  in if round_up fill > 15 then ANSITerminal.(print_string [green] health_bar)
  else if round_up fill > 7 then ANSITerminal.(print_string [yellow] health_bar)
  else ANSITerminal.(print_string [red] health_bar)

(**represents [alive] players with a filled circle and 
   dead players with an unfilled circle*)
let player_alive_dead_str alive total = 
  (repeat_str alive "●") ^ (repeat_str (total - alive) "○")

(**[print_food_info food sep] prints info about a [food] relevant to a turn
   each line is separated by [sep]*)
let print_food_info food sep = 
  print_string (Food.name food);
  print_string (sep ^ "Type: ");
  print_category (Food.category food);
  print_string sep;
  let (health, max) = Food.health food, Food.max_health food 
  in print_string ("Health: " ^ (string_of_int health) ^ "/" 
                   ^ (string_of_int max) ^ " "); 
  print_health_bar health max;
  print_string sep;
  print_newline()

(**[print_plain_food_info food] all the basic info about a food*)
let print_plain_food_info food = 
  print_string (repeat_str 2 "═"); 
  print_string (" " ^ Food.name food ^ " | ");
  print_category (Food.category food);
  print_string (" | ");
  let (health, max) = (Food.health food, Food.max_health food)
  in print_string ("Health: " ^ (string_of_int health) ^ "/" 
                   ^ (string_of_int max) ^ " "); 
  print_endline (repeat_str 20 "═"); 
  List.iter (print_move food) (Food.moves_lst food)

let print_food_dict () = 
  StringDict.StringDict.to_seq Database.foods
  |> Seq.iter (fun (k, _) -> Food.lookup_food k |> print_plain_food_info;
                print_newline())

(**prints the foods from a [lst]*)
let print_food_list lst = List.iter (fun f -> print_plain_food_info f) lst

(**prints opponent's food, curr player's food info*)
let print_opp_player_foods state = 
  print_string(spacer_1() ^ "Opponent: ");
  print_food_info (State.get_opp_food state) ("\n" ^ spacer_1());
  print_newline();
  print_string(spacer_2() ^ "Your Food:");
  print_food_info (State.get_curr_food state) ("\n" ^ spacer_2())

(**[usable_foods state] prints the usable foods of the current player*)
let print_usable_foods state =
  print_string "Usable Foods: ";
  print_list (State.get_food_options state)

let start_round state = 
  barrier();
  let (plyr, opp) = 
    match State.turn state with 
    | One -> (State.One, State.Two)
    | Two -> (Two, One)
  in ANSITerminal.(print_string [Blink; Bold; Inverse] 
                     (repeat_str 30 "░" ^ "It's " ^ (turn_str plyr) ^ "'s turn" 
                      ^ repeat_str 30 "░"); print_newline(););

  let (alive, total) = State.get_opp_alive_total state
  in print_endline (spacer_1());
  print_endline (spacer_1() ^ "Opponent: "  ^ (State.get_name state opp)
                 ^ " " ^ (player_alive_dead_str alive total));
  print_string (spacer_1() ^ "Food: ");
  print_food_info (State.get_opp_food state) ("\n" ^ spacer_1());
  print_newline(); small_barrier ();
  let (alive, total) = State.get_curr_alive_total state
  in print_endline (spacer_2() ^ "You: " ^ (State.get_name state plyr)
                    ^ " " ^ player_alive_dead_str alive total);
  print_string(spacer_2() ^ "Food: ");
  print_food_info (State.get_curr_food state) ("\n" ^ spacer_2());
  print_newline();
  print_usable_foods state

let print_take_move who move_opt state = 
  match move_opt with 
  | None -> message (who ^ " switched out a food!")
  | Some move -> 
    message (who ^ " used " ^ Move.name move ^ "! " 
             ^ Move.descript move ^ "!");
    print_opp_player_foods state;
    match Move.move_type move with 
    | Attack dmg -> 
      State.get_opp_food state |> get_effect move |> print_effect
    | Heal hl -> message ("Healed " ^ (string_of_int hl) ^ " points!")
    | Drain dmg -> message ("Gained back some health!");
      State.get_opp_food state |> get_effect move |> print_effect