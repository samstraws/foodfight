
type game_command =
  | Back
  | Output of string (*would be the name of move, food, or player*)
  | Bad_input
  | Quit

type conditional = 
  | Yes
  | No 
  | Redo 

(** [user_input str] takes the user input of type string and trim it.
    It also converts all letters to lowercase *)
let user_input str = String.(trim str |> lowercase_ascii)

let parse_move move = 
  let input = user_input move in 
  match input with
  | "back" -> Back
  | "quit" -> Quit 
  | "switch" -> Output "switch"
  | str -> try
      ignore (Move.lookup_move str); 
      Output str
    with Move.Invalid_move -> Bad_input

let parse_food food = 
  match user_input food with
  | "quit" -> Quit
  | "back" -> Back
  | str -> try 
      ignore(Food.lookup_food str);
      Output str
    with Food.Invalid_food -> Bad_input

let parse_player player = 
  match user_input player with
  | "quit" -> Quit
  | s -> try
      let str = Player.lookup_char s in 
      ignore (Player.lookup_player str);
      Output str
    with Player.Invalid_player| _ -> Bad_input

let parse_yes_no str = 
  match user_input str with 
  | "yes" -> Yes
  | "no" -> No
  | _ -> Redo
