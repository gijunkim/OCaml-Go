type stone = Blank | B | W

type board = ((int * int) * stone) list

type data = {board: board; configs: board list; passes: int}

type status = IN_PROGRESS | OVER

exception InvalidPlacement
exception Ko

(**[cap] is the maximum number of previous board configurations to be checked 
   for the enforcement of the Ko rule*)
let cap = 3

(**[get stone (x,y) b] is stone at (x,y) on b *)
let get_stone (x, y) board = List.assoc (x, y) board

(**[size b] is size of b *)
let size board = (List.length board) |> float_of_int |> sqrt |> int_of_float

(**[out_of_bounds (x,y) b] is true if (x,y) is out of bounds on b *)
let out_of_bounds (x, y) board = let size = size board in
  x < 0 || y < 0 || size <= x || size <= y

(**[uniq_insert x lst] is the list containing [x] and every element in [lst],
   such that each element in the list appears only once *)
let uniq_insert x lst = if List.mem x lst then lst else x::lst

(**[uniq_combine lst1 lst2] is the list containing all the elements of [lst1] 
   and [lst2], such that each element appears in the list only once *)
let uniq_combine lst1 lst2 = let rec helper lst acc = match lst with
    | [] -> acc
    | h::t -> helper t (uniq_insert h acc)
  in helper lst1 lst2

(** [initialize x] makes is a board of [x] by [x] spaces; 
    Requires: size >=1 *)
let initialize (size:int) : board = 
  let rec init board size (acc1:int) (acc2:int) = match (acc1,acc2) with
    |(a,b) when a =size&&b=size -> [(size,size),Blank]
    |(c,d) when d = size -> ((c,d),Blank)::init board size (acc1+1) 0
    |(e,f)->((e,f),Blank)::init board size (acc1) (acc2+1)
  in init [] (size - 1) 0 0

(**[ignore] is a ref storing the list of positions visited during the execution
   of reaches *)
let ignore = ref []

(** [neighbors (x,y) b] is a list of the neighbors of [(x,y)] on [b] *)
let neighbors (x, y) board = let size = size board in
  (if not (List.mem (x + 1, y) !ignore) && x + 1 < size 
   then [(x + 1), (y)] else []) 
  @ (if not (List.mem (x - 1, y) !ignore) && 0 <= x - 1 
     then [(x - 1, y)] else [])
  @ (if not (List.mem (x, y + 1) !ignore) && y + 1 < size 
     then [(x), (y + 1)] else []) 
  @ ((if not (List.mem (x, y - 1) !ignore) && 0 <= y - 1 
      then [(x, y - 1)] else []))

(**[reaches_helper stone pos board] is the list of colors that [stone] at
   [pos] in [board] reaches, according to the Logical Rules of Go *)
let rec reaches_helper stone pos board : stone list =
  let neighbor_stones = neighbors pos board in 
  let rec helper lst acc = match lst with
    | [] -> acc
    | h::t -> let h_stone = get_stone h board in 
      if h_stone != stone then helper t (uniq_insert h_stone acc) else
        helper t (uniq_combine (reaches_helper stone h board) acc)
  in ignore := uniq_insert pos !ignore; helper neighbor_stones []

(** [reaches s p b] is the list of stones reach from [s] at [p] on [b] *)
let reaches stone pos board = ignore := []; reaches_helper stone pos board

(**fold f i b is List.fold_left f i b*)
let fold f init board = List.fold_left f init board

(**map f b is List.map f b*)
let map f board = List.map f board

(**[opposite_color b] is the opposite color of b or Blank if b is Blank *)
let opposite_color s = match s with
  | B -> W
  | W -> B
  | Blank -> Blank

(**[add_config config_lst config cap] is the list where [config] is stored with the
   first [cap] - 1 elements in [config_lst] *)
let rec add_config config_lst config cap = 
  let len = List.length config_lst in 
  if len = cap then 
    let removed = match (List.rev config_lst) with 
      | h::t -> List.rev t
      | [] -> [] 
    in config :: removed
  else config :: config_lst

(**[store config_lst config] is the list where [config] is stored with the
   first cap - 1 elements in [config_lst]*)
let store config_lst config = 
  add_config config_lst config cap

(**[prev_config config board] is the boolean representing whether or not the
   current configuration in [board] matches [config] *)
let rec prev_config config board = 
  if config = [] then false else
    match board with 
    | ((x, y), stone)::t -> (List.assoc (x,y) config = stone) && prev_config 
                              config t
    | [] -> true

(**[prev_configs config_lst board] is the boolean representing whether or not 
   the current configuration in [board] matches a configuration in [config_lst]*)
let rec prev_configs config_lst board = 
  if config_lst = [] then false else 
    match config_lst with
    | h::t -> prev_config h board || prev_configs t board
    | [] -> false

(** [change_pos s p b] is the new position of [s] that was at [p] on [b]; 
    Requires: [p] is not out of bounds *)
let change_pos stone pos board = map (fun (position, s) -> 
    if position = pos then (position, stone) else (position, s)) board

(**[clear s b] is the board if [s] is cleared *)
let clear stone board : board = 
  let reaches_empty b (p, s) = s != stone || s = Blank 
                               || List.mem Blank (reaches s p b) in
  map (fun (pos, s) -> if reaches_empty board (pos, s) then (pos, s) 
        else (pos, Blank)) board

(**[handle_suicide pos board stone] is the boolean representing whether or
   a potential suicide is actually a valid move or not*)
let handle_suicide pos board stone = let possible_board = 
                                       change_pos stone pos board 
                                       |> clear (opposite_color stone) |> 
                                       clear stone in
  let reaches_lst = reaches stone pos possible_board in
  List.mem Blank reaches_lst && get_stone pos possible_board != Blank

(** [is_valid_pos pos board stone config_lst] is the boolean representing 
    whether or not a stone placement at [pos] if a valid placement
    Precondition: x, y is within bounds of board *)
let is_valid_pos pos board stone config_lst =
  (* Case 1: already has stone on that position *)
  let color = get_stone pos board in
  match color with 
  | B | W -> false
  (* Case 2: not suicidal move, can't be liberty or eye *)
  | Blank -> let reaches_lst = reaches stone pos board in 
    (List.mem Blank reaches_lst || handle_suicide pos board stone)

(** [place s p b] is the board after [s] is placed at [p] on [b]. 
    Raises: Invalid Placement if [p] is not valid on [b].*)
let place stone position board config_lst = 
  if is_valid_pos position board stone config_lst then change_pos stone 
      position board
  else raise InvalidPlacement

(** [territory (p,s) b] is the stone for which territory [s] belongs to at [p] 
    on [b]*)
let territory (pos, s) board : stone =
  let reached = reaches s pos board in match reached with
  | [B] | [Blank; B] | [B; Blank] -> B
  | [W] | [Blank; W] | [W; Blank] -> W
  | _ -> Blank

(**[move s p b config_lst] is the board after [s] is placed at [p] on [b], where
   [config_lst] is the list of previous board configurations. 
   Raises: Invalid Placement is a move violates the game rules
   if [p] is not valid on [b].*)
let move stone position {board; configs=config_lst; passes} = 
  if stone = Blank then {board = board; configs = config_lst; 
                         passes = passes + 1} 
  else
    let new_board = 
      (place stone position board config_lst |> clear (opposite_color stone) 
       |> clear stone) 
    in 
    if not(prev_configs config_lst new_board) 
    then {board = new_board; configs = store config_lst new_board; passes = 0 } 
    else raise Ko

(**[num_of stone board] is the number of stones of color [stone] in [board] *)
let num_of stone board = fold (fun acc (_, s) -> if s = stone then acc + 1 
                                else acc)
    0 board

(**[num_in_territory stone board] is the number of blank spaces in the territory
   of color [stone] in [board] *)
let num_in_territory stone board = fold (fun acc (pos, s) -> 
    if s = Blank && territory (pos, s) board = stone then acc + 1 else acc)
    0 board

(**[score b] is the score (black, white) of [b] *)
let score board : (float * float) =
  (float_of_int(num_of B board) +. float_of_int(num_in_territory B board)
  , float_of_int(num_of W board) +. float_of_int(num_in_territory W board) 
    +. 5.5)  

(** [status game] is the current status of the game*)
let status game = if game.passes = 2 then OVER else IN_PROGRESS

(** [winner game] is the winner of the game *)
let winner game = let (b, w) = score game.board in
  if b > w then B else if b < w then W else Blank

(**[equals b1 b2] is the boolean representing if the boards [b1] and [b2] 
   are equal. Two boards are equal if they have the same stones in the exact same
   positions*)
let equals b1 b2 = 
  let rec helper lst1 lst2 = match (lst1, lst2) with
    | ([], []) -> true
    | ([], h::t) -> false
    | (h::t, []) -> false
    | ((_, s1)::t1, (_, s2)::t2) -> if s1 <> s2 then false else helper t1 t2 in
  helper b1 b2 