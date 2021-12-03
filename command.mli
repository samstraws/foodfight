(**Parsing of Player Commands *)

type game_command =
  | Back
  | Output of string
  | Bad_input
  | Quit

type conditional = 
  | Yes
  | No 
  | Redo 

(**[parse_move str] is a game command:
   | Back if user wants to go back
   | Quit if user wants to quit
   | Output "switch" if user wants to switch food
   | Output str of move with name [str] if [str] found in move Database
   | Bad_input if [str] not found
*)
val parse_move: string -> game_command

(**[parse_food str] is a game command: 
   | Quit if user wants to quit
   | Back if user wants to go back
   | Output str of food with name [str] if [str] found in food Database
   | Bad_input if [str] not found
*)
val parse_food: string -> game_command

(**[parse_player str] is a game command:
   | Quit if user wants to quit
   | Output str if [str] found in player Database
   | Bad_input if [str] not found
   Guaranteed to never return [Back]
*)
val parse_player: string -> game_command

(**[parse_yes_no str] is the corresponding conditional, yes or no.
   If bad input, then is [Redo]. if quit, the [Quit]*)
val parse_yes_no : string -> conditional