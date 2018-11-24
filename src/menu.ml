open Main
open Leaderboard

(** [logo] is the ASCII text art representing of Go*)
let logo = "         _              _       
        /\\ \\           /\\ \\     
       /  \\ \\         /  \\ \\    
      / /\\ \\_\\       / /\\ \\ \\   
     / / /\\/_/      / / /\\ \\ \\  
    / / / ______   / / /  \\ \\_\\ 
   / / / /\\_____\\ / / /   / / / 
  / / /  \\/____ // / /   / / /  
 / / /_____/ / // / /___/ / /   
/ / /______\\/ // / /____\\/ /    
\\/___________/ \\/_________/     "

(** [highlight] represents the modes which will be highlighted in the menu
    Left means that the human player mode will be highlighted in the menu, and 
    Right means that the Computer player mode will be highlighted*)
type highlight = Left | Right | Middle
type ai_mode = Easy | Experimental

let play mode sets difficulty = 
  match mode with 
  | Left -> main false sets difficulty
  | Middle -> main true sets difficulty
  | Right -> print_leaderboard (convert_leaderboard (Unix.getcwd ())) 0

(** [menu highlight] prints out the menu of the game, and [highlight] determines
    which mode should be highlighted*)
let menu highlight =
  ANSITerminal.erase Screen;
  ANSITerminal.print_string [ANSITerminal.Bold; ANSITerminal.yellow] logo;
  ANSITerminal.print_string [ANSITerminal.cyan] "\n\nWelcome to Go!\n";
  ANSITerminal.print_string [ANSITerminal.Bold] "Select a mode:\n";
  match highlight with 
  | Left -> ANSITerminal.print_string [ANSITerminal.Bold; ANSITerminal.on_magenta] "Human Player";
    ANSITerminal.print_string [] "    ";
    ANSITerminal.print_string [ANSITerminal.Bold] "Computer Player";
    ANSITerminal.print_string [] "    ";
    ANSITerminal.print_string [ANSITerminal.Bold] "Leaderboard";
    print_endline ""
  | Middle -> ANSITerminal.print_string [ANSITerminal.Bold] "Human Player";
    ANSITerminal.print_string [] "    ";
    ANSITerminal.print_string [ANSITerminal.Bold; ANSITerminal.on_magenta] "Computer Player";
    ANSITerminal.print_string [] "    ";
    ANSITerminal.print_string [ANSITerminal.Bold] "Leaderboard";
    print_endline ""
  | Right -> ANSITerminal.print_string [ANSITerminal.Bold] "Human Player";
    ANSITerminal.print_string [] "    ";
    ANSITerminal.print_string [ANSITerminal.Bold] "Computer Player";
    ANSITerminal.print_string [] "    ";
    ANSITerminal.print_string [ANSITerminal.Bold; ANSITerminal.on_magenta] "Leaderboard";
    print_endline ""

let sets_select highlight_sets =
  ANSITerminal.erase Screen;
  ANSITerminal.print_string [ANSITerminal.Bold] "\nSelect the number of sets:\n";
  match highlight_sets with 
  | One -> ANSITerminal.print_string [ANSITerminal.Bold; ANSITerminal.on_magenta] "Best of 1";
    ANSITerminal.print_string [] "    ";
    ANSITerminal.print_string [ANSITerminal.Bold] "Best of 3";
    ANSITerminal.print_string [] "    ";
    ANSITerminal.print_string [ANSITerminal.Bold] "Best of 5";
    print_endline ""
  | Three -> ANSITerminal.print_string [ANSITerminal.Bold] "Best of 1";
    ANSITerminal.print_string [] "    ";
    ANSITerminal.print_string [ANSITerminal.Bold; ANSITerminal.on_magenta] "Best of 3";
    ANSITerminal.print_string [] "    ";
    ANSITerminal.print_string [ANSITerminal.Bold] "Best of 5";
    print_endline ""
  | Five -> ANSITerminal.print_string [ANSITerminal.Bold] "Best of 1";
    ANSITerminal.print_string [] "    ";
    ANSITerminal.print_string [ANSITerminal.Bold] "Best of 3";
    ANSITerminal.print_string [] "    ";
    ANSITerminal.print_string [ANSITerminal.Bold; ANSITerminal.on_magenta] "Best of 5";
    print_endline ""

let ai_select highlight_ai = 
  ANSITerminal.erase Screen;
  ANSITerminal.print_string [ANSITerminal.Bold] "\nSelect the difficulty: \n";
  match highlight_ai with 
  | Easy -> ANSITerminal.print_string [ANSITerminal.Bold; ANSITerminal.on_magenta] "Easy";
    ANSITerminal.print_string [] "    ";
    ANSITerminal.print_string [ANSITerminal.Bold] "Experimental";
    print_endline ""
  | Experimental -> ANSITerminal.print_string [ANSITerminal.Bold] "Easy";
    ANSITerminal.print_string [] "    ";
    ANSITerminal.print_string [ANSITerminal.Bold; ANSITerminal.on_magenta] "Experimental";
    print_endline ""

let rec handle_sets_input highlight sets difficulty = match get1char () with 
  | '\027' -> handle_sets_input highlight sets difficulty
  | '[' -> handle_sets_input highlight sets difficulty
  | 'D' -> if (sets = Five) then (sets_select Three; handle_sets_input highlight Three difficulty)
    else sets_select One; handle_sets_input highlight One difficulty
  | 'C' -> if (sets = One) then (sets_select Three; handle_sets_input highlight Three difficulty)
    else sets_select Five; handle_sets_input highlight Five difficulty
  | '\n' -> play highlight sets difficulty
  | _ -> handle_sets_input highlight sets difficulty

let rec handle_ai_input difficulty highlight sets= match get1char () with 
  | '\027' -> handle_ai_input difficulty highlight sets
  | '[' -> handle_ai_input difficulty highlight sets
  | 'C' -> ai_select Experimental; handle_ai_input Experimental highlight sets
  | 'D' -> ai_select Easy; handle_ai_input Easy highlight sets
  | '\n' -> sets_select One; handle_sets_input Middle sets (difficulty = Experimental)
  | _ -> handle_ai_input difficulty highlight sets

(** [handle_menu_input highlight] handles the input portion of the menu. [highlight 
    determines which mode should be selected]*)
let rec handle_menu_input highlight = match get1char () with
  | '\027' -> handle_menu_input highlight
  | '[' -> handle_menu_input highlight
  | 'D' -> (match highlight with 
      | Left -> menu Left; handle_menu_input Left
      | Middle -> menu Left; handle_menu_input Left
      | Right -> menu Middle; handle_menu_input Middle)
  | 'C' -> (match highlight with 
      | Left -> menu Middle; handle_menu_input Middle
      | Middle -> menu Right; handle_menu_input Right
      | Right -> menu Right; handle_menu_input Right)
  | '\n' -> (match highlight with 
      | Right -> play highlight One false;
      | Middle -> ai_select Easy; handle_ai_input Easy highlight One
      | _ -> sets_select One; handle_sets_input highlight One false;)
  | _ -> handle_menu_input highlight

let () = ANSITerminal.erase Screen; menu Left; handle_menu_input Left
