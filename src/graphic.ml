open ANSITerminal
open Game

exception InvalidSize

(** [vert a s] is the row of |'s for the correct [s] of the board [b] *)
let rec vert acc size = match acc with
  |a when a = size -> "|"
  |b -> "|   "^ vert (acc+1) size

(** [print_board b] prints the go board [b] in type string *)
let print_board board = 

  let y = List.hd (List.rev board) in
  let x = match y with 
    |((a,b),c)->b 
  in

  let rec printb board = match board with
    |[]->""
    |h::t-> match h with
      |((w,y),z) when w =x&&y=x -> if z=Blank then " " 
        else if z = W then "O" else "X"
      |((c,d),e) when d = x -> (if (e = Blank) then " " 
                                else if e = W then "O"
                                else "X") ^"\n"^ vert 0 x^"\n"^ (printb t)
      |((f,g),h)->(if h = Blank then " " 
                   else if h = W then "O" 
                   else "X")^" ― "^(printb t)
  in printb (board)

(** [printb2_helper a] prints the character, with proper color, that corresponds 
    to [a]*)
let printb2_helper a = match a with
  |'O'-> print_string [ANSITerminal.white;ANSITerminal.on_yellow; 
          ANSITerminal.Bold] "O"
  |'X'-> print_string [ANSITerminal.black;ANSITerminal.on_yellow; 
          ANSITerminal.Bold] "O"
  | '|' -> print_string [ANSITerminal.black;ANSITerminal.on_yellow] "|"
  |' '->print_string [ANSITerminal.black;ANSITerminal.on_yellow] " "
  |'0'->print_string [ANSITerminal.white;ANSITerminal.on_yellow] "0"
  |'1'->print_string [ANSITerminal.white;ANSITerminal.on_yellow] "1"
  |'2'->print_string [ANSITerminal.white;ANSITerminal.on_yellow] "2"
  |'3'->print_string [ANSITerminal.white;ANSITerminal.on_yellow] "3"
  |'4'->print_string [ANSITerminal.white;ANSITerminal.on_yellow] "4"
  |'5'->print_string [ANSITerminal.white;ANSITerminal.on_yellow] "5"
  |'6'->print_string [ANSITerminal.white;ANSITerminal.on_yellow] "6"
  |'7'->print_string [ANSITerminal.white;ANSITerminal.on_yellow] "7"
  |'8'->print_string [ANSITerminal.white;ANSITerminal.on_yellow] "8"
  |'9'->print_string [ANSITerminal.white;ANSITerminal.on_yellow] "9"
  |'\128'->print_string [ANSITerminal.black;ANSITerminal.on_yellow] "―"
  |'\226'->print_string [ANSITerminal.black;ANSITerminal.on_yellow] ""
  |'\149'->print_string [ANSITerminal.black;ANSITerminal.on_yellow] ""
  | b -> print_char b

(**[printb2 b] prints [b] with the proper colors of the stones *)
let printb2 board= 
  String.iter printb2_helper (print_board (board))