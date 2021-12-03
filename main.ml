open Command
open State
open Display

(**[declare_winner] prints the outcome of the round and exits the game
   [str] indicates who took the recent [mv], and [plyr] is who won the game
   [state] is the state right after move is taken by winner - has not switched 
   current player*)
let declare_winner (mv, plyr) str state= 
  print_take_move str (Some mv) state; 
  ignore(prompt("Hit enter to continue"));
  print_winner plyr; 
  exit 0

(**[read_help parse t str] tries to [parse] the given [str]
   of type string representation [t].
   Returns: None if user wants to go back, Some [str] if [str] is parsed *)
let rec read_help parse t str = 
  match parse str with 
  | Back -> None 
  | Quit -> print_exit (); exit 0 
  | Output "switch" -> Some "switch"
  | Output str -> Some str 
  | Bad_input -> 
    print_error ("Sorry, this is an invalid " ^ t);
    let input = prompt ("Please enter a valid "^ t ^" (or type 'quit' to quit)")
    in read_help parse t input

let read_move_opt str = read_help parse_move "move" str

let rec read_food_opt str = read_help parse_food "food" str

(*[read_player str] tries to get the player from the given [str]. 
  If bad output, reprompts for the player. *)
let rec read_player str = 
  match (read_help parse_player "player" str) with 
  | None -> failwith "Parse player should never reach this state"
  | Some p -> p

(**[read_food_switch switch_f state] is None if the user does not want to switch
   or Some [switch_f state] if the user sucessfully switches foods.
   This is the actual execution of food switch using function [switch_f]
   Case 1: go back 
   Case 2: try to switch to current food, recur again
   Case 3: Unsucessful switch
   Case 4: successful switch*)
let rec read_food_switch switch_f state = 
  print_usable_foods state;
  let str =  prompt "Enter a valid food to switch to: " 
  in match read_food_opt str with 
  | None -> None (*case 1*)
  | Some food -> begin
      match (switch_f state food) with 
      | State.Winner _ -> failwith "Should never win on switch"
      | Bad_move -> (*case 3*)
        print_error "Sorry, you can't use that food.";
        read_food_switch switch_f state
      | Normal (nst, _) -> (*case 4*)
        message ("You sent out " ^ food ^ "!"); 
        Some nst 
    end

(**[normal_food_switch state] asks the user if they want to switch their food, 
   then calls the food switch loop*)
let rec normal_food_switch state = 
  if List.length (State.get_food_options state) <= 0 
  then (message "Can't switch out with only one food"; None) 
  else read_food_switch State.make_switch state

(**[read_take_move state] prompts the user for a move they wish to use. 
   If the user wants to go back, returns None. If bad move, reprompt. 
   Else, uses the move*)
let rec read_take_move state = 
  print_all_moves state;
  let str = prompt "What move do you want to use? (Enter a move name or \"switch\" to change foods): " 
  in match read_move_opt str with 
  | None -> None  
  | Some "switch" -> normal_food_switch state
  | Some move -> begin
      match State.make_move state move with 
      | Winner (mv, p) -> declare_winner (mv, p) "You" state
      | Bad_move -> begin
          print_error "Sorry, you can't use that move.";
          read_take_move state 
        end
      | Normal (nst, mv) -> 
        print_take_move "You" mv state;
        Some nst 
    end
(******************************************************************************
   Execute Game
 ******************************************************************************)
(**[choice_loop loop state] asks for user input in a game, then may call [loop] 
   based on outcome.
   Case 1: current food is expired, forces the player to switch,
   then asks them to take a move
   Case 2: Take move/Make food switch
   Case 3: Go back - loop over unchanged state
*)
let choice_loop loop state = 
  ignore(prompt("Hit enter to continue"));
  start_round state;
  if State.need_switch state then begin (*case 1*)
    message "Your current food has expired, you need to switch.";
    match read_food_switch State.make_switch_dead state with 
    | None -> read_food_switch State.make_switch_dead state
    | Some nst -> begin
        match read_take_move nst with 
        | Some nst' -> loop nst'
        | None -> read_take_move nst
      end 
  end
  else
    begin 
      match read_take_move state with  
      | Some nst -> loop nst (*case 2*)
      | None -> (*case 3*)
        message "Went back."; 
        loop state 
    end

(**[loop state] loops over [state] with two human players 
   until someone quits or wins*)
let rec loop state = choice_loop loop state

(**[cp_loop state] loops over [state] with one human player and one automated 
   player*)
let rec cp_loop state = 
  if State.turn state = Two then begin
    ignore(prompt("Computer's Move: hit enter to continue"));
    match auto_take_move state with 
    | Winner (mv, plyr) -> declare_winner (mv, plyr) "Computer" state
    | Normal (nst, mv) -> 
      print_take_move "Computer" mv nst; 
      cp_loop nst 
    | Bad_move -> failwith "Computer should not take bad move"
  end
  else choice_loop cp_loop state

(******************************************************************************
   Player Selection
 ******************************************************************************)
(**[custom_foods acc plyr n] prompts for a foods list in a customized player*)
let rec custom_foods acc plyr n = 
  let rec custom_food_loop plyr n = 
    match read_food_opt (simple_prompt ("food " ^ string_of_int n)) with 
    | None -> print_error "Invalid food, try again. "; custom_food_loop plyr n 
    | Some f -> f
  in if n > Constants.max_foods then acc 
  else custom_foods ((custom_food_loop plyr n)::acc) plyr (n + 1)


(**[custom_player_info turn_str] prompts for custom info to make a player
   returns name * string list of foods*)
let custom_player_info turn_str =  
  let name = simple_prompt (turn_str ^ " name")
  in let foods = custom_foods [] turn_str 1 
  in (name, foods)

(*TODO: only let the user select certain options*)
(**[customize] creates a customized state*)
let customize () = 
  print_food_dict();
  let (n1, foods1) = custom_player_info "Player One"
  in small_barrier();
  let (n2, foods2) = custom_player_info "Player Two"
  in State.init_custom n1 foods1 n2 foods2 

(**[select_players] creates a state with preformed players*)
let select_players () = 
  print_endline ("Your potential fighters: ");
  print_play_dict();
  let p1 = read_player (prompt "Player One, please choose a character (type quit to exit)")
  in let p2 = read_player (prompt "Player Two, please choose a character (type quit to exit)")
  in State.init_state p1 p2 

(**[ask_custom_or_select] asks the player if they would like to customize*)
let rec ask_custom_or_select () = 
  let str = (prompt "Would you like to customize your players?\n(yes for custom, no to select from list)") 
  in match (parse_yes_no str) with 
  | Yes -> customize ()
  | No -> select_players ()
  | Redo -> print_error "Please type yes or no"; 
    ask_custom_or_select ()

(**[ask_one_or_two()] is true if the user wants one player game,
   false if two player*)
let rec ask_one_or_two () = 
  let str = (prompt "Single player mode or two player mode? (1 for single, 2 for two)")
  in match str with 
  | "1" -> true
  | "2" -> false
  | _ -> print_error "Please enter 1 or 2"; ask_one_or_two() 

(**[main ()] initializes the loop by reading in two players*)
let main () = 
  print_welcome ();
  if ask_one_or_two() then
    let i_state = select_players()
    in let _ = cp_loop i_state in ()
  else let i_state = ask_custom_or_select ()
    in let _ = loop i_state in ()

let () = main ()

