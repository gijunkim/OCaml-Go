open Graphic
open Game
open Pervasives
open Ai
open Leaderboard

(** [get1char ()] is a character which the user has typed into the terminal
    Requires: A POSIX-compliant environment
    From https://stackoverflow.com/a/13410456 *)
let get1char () =
  let termio = Unix.tcgetattr Unix.stdin in
  let () =
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
      { termio with Unix.c_icanon = false; Unix.c_echo = false } in
  let res = input_char stdin in
  Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio;
  res

(** [input] is the type representing what kind of move a user is performing on
    the board*)
type input = Up | Down | Left | Right | Return | Pass | Quit

(**[handle_input board_size] evaluates to an input, based on what arrow key
   a user inputs. Uses [board_size] to generate bounds for movement  *)
let rec handle_input (board_size:int) : input = 
  let (width, height) = ANSITerminal.size () in 
  let y_board = (height - board_size*2 - 1) in
  let x_board = (1 + ((board_size) - 1) * 4) in

  match get1char () with
  (*Arrow Controls*)
  | 'Q' | 'q' -> Quit
  | 'P' | 'p' -> Pass
  | '\n' -> Return
  | '\027' -> handle_input board_size
  | '[' -> handle_input board_size
  | 'A' -> begin match (ANSITerminal.pos_cursor ()) with
      | exception _ -> handle_input board_size
      | (_, b) -> if (b = y_board) then handle_input board_size else Up end
  | 'B' -> begin match (ANSITerminal.pos_cursor ()) with
      | exception _ -> handle_input board_size
      | (_, b) -> if (b = height-3) then handle_input board_size else Down end
  | 'D' -> begin match (ANSITerminal.pos_cursor ()) with
      | exception _ -> handle_input board_size
      | (a, _) -> if (a = 3) then handle_input board_size else Left end
  | 'C' -> begin match (ANSITerminal.pos_cursor ()) with
      | exception _ -> handle_input board_size
      | (a, _) -> if (a = x_board) then handle_input board_size else Right end
  | _ -> handle_input board_size

(**[move_pos input] moves the terminal cursor along the board according to
   [input] *)
let rec move_pos (input : input) : unit = match input with
  | Up -> ANSITerminal.move_cursor 0 (-2)  
  | Down ->  ANSITerminal.move_cursor 0 2
  | Left -> ANSITerminal.move_cursor (-4) 0
  | Right -> ANSITerminal.move_cursor (4) 0
  | _ -> ()

(**[movement func board_size] is the option possibly containg the board position
   where a stone should be placed. Uses [func] to transform the terminal cursor
   position to a position on the board. Evaluates to None if a turn should be
   passed *)
let rec movement func board_size = match handle_input board_size with
  | Return -> Some (func (ANSITerminal.pos_cursor ()))
  | Quit ->  ANSITerminal.move_bol (); ANSITerminal.erase Screen; exit 0
  | Pass -> None
  | x -> move_pos x; movement func board_size

(**[sc s] prints the winner according to [s] *)
let sc score =
  match score with
  |(a,b)-> if(a>b) then print_endline 
        ("\nBlack wins "^(string_of_float a)^"-"^string_of_float b) else if(b>a) 
    then print_endline ("\nWhite wins "^((string_of_float b)^"-"
                                         ^string_of_float a)) 
    else  print_endline ("\nTIE"^(string_of_float b)^"-"^string_of_float a)

(**[select_color color] is the opposite color of [color] *)
let select_color color =  match color with
  | B -> W
  | W -> B
  | Blank -> Blank

(** [randomly_select lst] is the option possibly containing an element in [lst],
    chosen randomly. Evaluates to None if an element outside of [lst] is 
    randomly chosen*)
let randomly_select lst = let size = List.length lst in 
  let index = Random.int (size + 1) in List.nth_opt lst index

let score_difference stone (b, w) = match stone with
  | B -> b -. w
  | W -> w -. b
  | Blank -> 0.0

(**[computer_select_move board] is the option possibly storing the coordinates
   of the computer's selected position on [board]. Evaluates to None if the
   computer player decides to pass*)
let computer_select_move board configs  = match randomly_select board with
  | Some (a, b) -> Some a
  | None -> None

(** [black_white_print color] prints the text associated with [color]*)
let black_white_print color =
  match color with
  | B -> ANSITerminal.print_string 
           [ANSITerminal.black; ANSITerminal.Bold] "\nBlack's "
  | _ -> ANSITerminal.print_string 
           [ANSITerminal.white; ANSITerminal.Bold] "\nWhite's "

type highlight_sets = One | Three | Five

let print_score b w = ANSITerminal.print_string 
    [ANSITerminal.black; ANSITerminal.Bold] "\nBlack ";print_string
    (string_of_int (b)^" : "^string_of_int w); 
  ANSITerminal.print_string [ANSITerminal.white; ANSITerminal.Bold] " White\n"

let count_score3 b w =
  if(b>=2||w>=2) then true else false

let count_score5 b w =
  if(b>=3||w>=3) then true else false

(**[main computer] runs the game. [computer] is the boolean representing
   whether or not the game is being run in computer mode *)
let rec main computer game_mode experimental =
  print_endline "\nPlease enter a valid size of the board:\n";
  print_string  "> ";
  match read_line () with 

  | exception _ -> main computer game_mode experimental
  | board_size -> 
    match initialize (int_of_string board_size) with 
    | exception _ -> main computer game_mode experimental
    | _ -> let board = initialize (int_of_string board_size) in 
      ANSITerminal.erase ANSITerminal.Screen; 
      let rec print_placed color data computer_move invalid b_score w_score= 

        if ((game_mode = Three && count_score3 b_score w_score) || 
            (game_mode = Five && count_score5 b_score w_score)) then (
          exit 0); 

        (if (color = B) then black_white_print B else black_white_print W);
        print_string "Turn: "; 
        (if (invalid = true) then ANSITerminal.print_string 
             [ANSITerminal.red; ANSITerminal.Bold] "Invalid Move!" 
         else print_string "");

        print_string "\n[Arrow Keys] to move cursor | [Enter] to place | [p] to pass | [q] to quit\n\n";
        printb2 data.board;

        let height = 24 + (int_of_string board_size) + 4 in
        if (data.passes >= 2)
        then (let (bl, wh) = score data.board in 
              (if (game_mode = One) 
               then (sc (score data.board); 
                     if computer then 
                       ((out_leaderboard (Unix.getcwd ()) 
                           (wh-.bl)); exit 0) else exit 0;) 
               else (
                 ANSITerminal.erase Screen; match (bl,wh) with
                 | (a,b) -> if (a>b) then print_score (b_score+1) (w_score) 
                   else print_score (b_score) (w_score+1);sc (score data.board); 
                   if computer then 
                     out_leaderboard (Unix.getcwd ()) (wh-.bl);
                   match (score data.board) with 
                   | (a,b) -> if (a>b) then 
                       print_string (print_board 
                                       (print_placed B 
                                          {board=board;configs=[];passes=0} 
                                          computer false (b_score+1) (w_score)))
                     else if (a<b) then 
                       print_string (print_board 
                                       (print_placed B 
                                          {board=board;configs=[];passes=0} 
                                          computer false (b_score) (w_score+1)))
                     else print_string (print_board 
                                          (print_placed B 
                                             {board=board;configs=[];passes=0} 
                                             computer false (b_score) (w_score))))
              ))
        else

          print_string "\n\n\n  "; 

        let y_start = (height - (int_of_string board_size)*2 - 1) in
        ANSITerminal.set_cursor (1) y_start;
        let pos_to_coord (a, b) = ((b - y_start)/2, (a - 1)/4) in

        if computer_move then if experimental then let end_time = 
                                                     if data.configs = [] then 0.0 else Ai.time_to_calculate in
            let data = Ai.find_next_move data color end_time in
            ANSITerminal.erase ANSITerminal.Screen;
            print_placed (select_color color) data false false b_score w_score else 
            begin match computer_select_move data.board data.configs with
              | None -> ANSITerminal.move_bol (); ANSITerminal.erase Screen; 
                print_placed (select_color color) (move Blank (0,0) data) 
                  false false b_score w_score
              | Some coords -> ANSITerminal.move_bol (); begin match 
                    ((move color coords data)) with
                |exception InvalidPlacement -> print_endline "\n Invalid move"; 
                  ANSITerminal.erase ANSITerminal.Screen; 
                  print_placed color data true true b_score w_score
                | exception Ko -> print_endline "\n Ko! Invalid move"; 
                  ANSITerminal.erase ANSITerminal.Screen; 
                  print_placed color data true true b_score w_score
                |exception Not_found -> print_endline "\n Invalid move"; 
                  ANSITerminal.erase ANSITerminal.Screen;
                  print_placed color data true true b_score w_score
                | a-> let data = (move color coords data) in
                  ANSITerminal.erase ANSITerminal.Screen; 
                  print_placed (select_color color) data false false
                    b_score w_score end end
        else
          match movement pos_to_coord (int_of_string board_size) with 
          | Some coordinates -> ANSITerminal.move_bol (); 
            begin match ((move color coordinates data)) with
              |exception InvalidPlacement -> 
                ANSITerminal.erase ANSITerminal.Screen; 
                print_placed color data false true b_score w_score
              | exception Ko ->  
                ANSITerminal.erase ANSITerminal.Screen; 
                print_endline "\nKo! Invalid move!"; 
                print_placed color data false true b_score w_score
              |exception Not_found ->
                ANSITerminal.erase ANSITerminal.Screen;
                print_endline "\nInvalid move!"; 
                print_placed color data false true b_score w_score
              |a-> let data = (move color coordinates data) in
                ANSITerminal.erase ANSITerminal.Screen; 
                print_placed (select_color color) data computer 
                  false b_score w_score end

          | None -> ANSITerminal.move_bol (); ANSITerminal.erase Screen; 
            print_placed (select_color color) (move Blank (0,0) data) 
              computer false b_score w_score

      in ( 
        ANSITerminal.resize (80+(int_of_string) board_size) 
          (24+4+(int_of_string) board_size);
        ANSITerminal.erase ANSITerminal.Screen;
        let height = 24+(int_of_string board_size+4) in
        let  y_start = (height - (int_of_string board_size)*2 - 1) in 
        ANSITerminal.set_cursor 0 y_start; 
        ANSITerminal.move_bol ();
        print_string (print_board  (print_placed B 
                                      {board=board;configs=[];passes=0}  computer false 0 0))) 

