
(* [read_f channel acc] reads a file's in-channel [channel] and 
   returns the lines in a string list [acc] *)
let rec read_f channel acc = 
  match (input_line channel) with
  | exception End_of_file -> acc
  | s -> read_f channel (s::acc)

(* [convert_s s] converts a line [s] from the leaderboard file into an element
   in the leaderboard data type; 
   Requires: [s] should be in the format "name ;; score" *)
let convert_s s = 
  let lst = Str.split (Str.regexp " ;; ") s
  in match lst with 
  | name::score::[] -> (name, float_of_string score)
  | _ -> ("", 0.)

(* [lst_2_lb lst acc] converts the parsed-in string list [lst] and converts it
   to a leaderboard data type in [acc] *)
let rec lst_2_lb lst acc =
  match lst with 
  | s::t -> let converted = convert_s s in if converted <> ("", 0.) 
    then lst_2_lb t (converted::acc) else lst_2_lb t acc
  | [] -> acc

(* [convert_leaderboard d] takes the directory [d] that the file "woo.txt"
   resides in and then converts the file's contents to a leaderboard data type;
   Requires: [d] should be a directory containing the file "woo.txt", which
   should have a valid leaderboard output *)
let convert_leaderboard d = 
  let dir = match Unix.opendir d with
    | exception Unix.Unix_error (_, _, _) -> raise Not_found
    | d -> d
  in 
  let rec helper dir =
    match Unix.readdir dir with 
    | exception End_of_file -> Unix.closedir dir; []
    | entry -> 
      if Str.string_match (Str.regexp "woo.txt$") entry 0 then 
        let file_description = Unix.openfile (d ^ "/" ^ entry) [O_RDONLY] 0o640 in 
        let channel = Unix.in_channel_of_descr file_description in lst_2_lb
          (read_f channel []) []
      else helper dir
  in helper dir

(** [print_leaderboard lb count] prints a leaderboard [lb] to the terminal;
    Requires: [lb] is a valid leaderboard *)
let rec print_leaderboard lb count = 
  if count = 0 then ANSITerminal.erase Screen;
  match lb with 
  | (name, score)::t -> ANSITerminal.print_string [] 
                          (string_of_int (count + 1) ^ ") Name: " ^ name ^ ", Score: " 
                           ^ string_of_float score ^ "\n"); print_leaderboard t (count + 1)
  | [] -> ANSITerminal.print_string [] ""

(* [insert_lb name score lb acc kill] inserts an individual game's [name] 
   and [score] into a valid leaderboard [lb] in the proper location (high to low),
   which accumulates in [acc]; 
   Requires: [lb] is a valid leaderboard *)
let rec insert_lb (name:string) (score:float) lb acc kill = 
  match lb with 
  | (n, s)::t -> 
    if score < s || kill then insert_lb name score t ((n,s)::acc) kill
    else 
      insert_lb name score t ((n, s)::(name, score)::acc) true
  | [] -> if kill then List.rev(acc) else List.rev((name, score)::acc)

(* [lb_2_file lb d] converts a leaderboard [lb] to the output file "woo.txt" 
   in the directory [d]; 
   Requires: [d] has the file "woo.txt"*)
let lb_2_file lb d = 
  let file_description = Unix.openfile (d ^ "/" ^ "woo.txt") [O_WRONLY] 0o640 in 
  let channel = Unix.out_channel_of_descr file_description in  
  let rec helper lb = 
    (match lb with 
     | (n, s)::t -> output_string channel (n^" ;; "^string_of_float s^"\n"); helper t
     | [] -> close_out channel)
  in helper lb

(** [out_leaderboard d score] creates a new leaderboard given the [score]
    and the player's name, and then outputs the new leaderboard to a file in directory
    [d] *)
let out_leaderboard d score = 
  let lb = convert_leaderboard d in 
  ANSITerminal.print_string [] "Name: "; 
  match read_line () with 
  | exception _ -> exit 0
  | name -> if lb = [] then lb_2_file [(name, score)] d else 
      let new_lb = (insert_lb name score lb [] false) in lb_2_file new_lb d