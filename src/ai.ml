open Game
(*This is an implementation of Monte Carlo Tree Search, according to 
  https://www.baeldung.com/java-monte-carlo-tree-search *)

(**[state] is the record stone information relating to each node *)
type state = {game:data; player:stone; visit_count : int; win_score : float}

(**[gameTree] is the type representing the nodes in a game tree *)
type gameTree = Leaf | Node of ((gameTree * state * (gameTree list))) ref

(**[tree] is the ref storing the game tree for the current instance of the game *)
let tree = ref Leaf

(**[root_of_tree] is the ref storing the root node of the tree *)
let root_of_tree = ref Leaf

(**[win_score] is a score added to a node if it's in a branch leading to a win *)
let win_score = 10.0

(**[time_to_calculate] is the time in seconds the algorithm is allowed to run 
    before it must return a move*)
let time_to_calculate = 5.0

(** [randomly_select lst] is the option possibly containing an element in [lst],
    chosen randomly. Evaluates to None if an element outside of [lst] is 
    randomly chosen*)
let randomly_select lst = let size = List.length lst in 
  let index = Random.int (size + 1) in List.nth_opt lst index


(** [randomly_select_in lst] a randomly chosen element of [lst] **)
let randomly_select_in lst = let size = List.length lst in 
  let index = Random.int (size) in List.nth lst index

(**[get_all_possible_states state] is a list of all possible states after one 
    move is made, starting at [state] *)
let get_all_possible_states {game; player; visit_count; win_score} = 
  let rec helper acc = function
    | [] -> acc
    | (pos, s)::t -> begin match Game.move player pos game with
        | exception _ -> helper acc t
        | data -> helper ({game = data; player = (Game.opposite_color player); 
                           visit_count = 0; win_score = 0.0;}::acc) t end
  in {game = Game.move Blank (0,0) game; player = (Game.opposite_color player); 
      visit_count = 0; win_score = 0.0}::(helper [] game.board)

(**[random_play state] is the state resulting from making a random move,
     starting at [state]*)
let rec random_play state = 
  let board = state.game.board in 
  let (position, stone) = match randomly_select board with 
    | None -> ((0, 0), Blank)
    | Some (pos, _) -> (pos, state.player) in
  match Game.move stone position state.game with 
  | exception _ -> random_play state
  | game -> {state with game = game; player =(Game.opposite_color state.player)}

(**[uct_value total_visit visit_count win_score] is the UCT score calculated 
   from [total_visit], [visit_count], and [win_score] *)
let uct_value total_visit visit_count win_score = if visit_count = 0 then 
    Pervasives.infinity
  else win_score /. (float_of_int visit_count) +. 
       1.41 *. Pervasives.sqrt (Pervasives.log (float_of_int total_visit) /. 
                                (float_of_int visit_count))

(**[uct_value_of_node total_visit node] is UCT value of [node], given 
   [total_visit] *)
let uct_value_of_node total_visit = function
  | Leaf -> Pervasives.infinity
  | Node {contents = (_, state, _)} -> uct_value total_visit state.visit_count 
                                         state.win_score

(**[compare_node total_visit x y] is 0 if [x] as the same UCT value as [y], 
    returns a negative integer  if [x] has a greater UCT value than [y], and 
    returns a positive integer otherwise*)
let compare_node total_visit x y = Pervasives.compare 
    (uct_value_of_node total_visit y) (uct_value_of_node total_visit x)

(**[find_best_node_with_UCT node] is the child of [node] with the greatest 
    UCT value *)
let find_best_node_with_UCT = function
  | Leaf -> failwith "Don't call find_best_node_with_UCT on Leaf!"
  | Node x -> let (_, state, lst) = !x in match List.sort 
                                                  (compare_node state.visit_count) lst |> List.hd with
    | exception _ -> Node x
    | y -> y

(**[select_promising_node node] is a node with no current children, selected by
   chosing branches with high UCT values, starting at [node]*)
let select_promising_node node = 
  let rec helper node = match node with
    | Leaf -> failwith "Don't call select_promising_node on Leaf!"
    | Node x -> let (_, _, lst) = !x in if lst = [] then node else 
        find_best_node_with_UCT node |> helper in
  helper node

(**[expand_node node] generates children for [node] if [node] has no children *)
let expand_node = function
  | Leaf -> failwith "Don't call expand_node on Leaf!"
  | Node x -> let (parent, s, _) = !x in 
    let possible_states = get_all_possible_states s in
    let rec node_creator acc = function
      | [] -> acc
      | h::t -> node_creator ((Node (ref (Node x, h, [])))::acc) t
    in
    x := (parent, s, node_creator [] possible_states)

(**[back_propogation node_to_explore player] back propogates scores after exploring
   [node_to_explore]. [player] is used to determine whether exploration resulted
   in a win or not*)
let back_propogation node_to_explore player = 
  let rec explore = function
    | Leaf -> ()
    | Node x -> let (parent, s, lst) = !x in x := (parent, 
                                                   {s with visit_count = s.visit_count + 1; win_score = if player = s.player 
                                                                                              then s.win_score +. win_score else s.win_score}, lst); explore parent
  in
  explore node_to_explore

(**[compare_win_node total_visit x y] is 0 if [x] as the same win score as [y],
   returns a negative integer if [x] has a greater win score than [y], 
   and returns a positive integer otherwise*)
let compare_win_node x y = match (x, y) with 
  | (Leaf, _) -> failwith "Don't compare leaves!"
  | (_, Leaf) -> failwith "Don't compare leaves!"
  | (Node {contents = (_, s1, _)}, Node {contents = (_, s2, _)}) -> 
    Pervasives.compare s2.win_score s1.win_score 

(**[child_with_best_score node] is the child of [node] with the highest win score *)
let child_with_best_score = function
  | Leaf -> failwith "Don't call child_with_best_score on Leaf!"
  | Node {contents = (_, _, lst)} -> List.sort compare_win_node lst |> List.hd

(**[simulate_random_playout opponent] is the winner of a randomly simulated game.
   [opponent] is used to determine if the game resulted in a win for the AI player*)
let simulate_random_playout opponent = function
  | Leaf -> failwith "Don't call simulate_random_playout on Leaf!"
  | Node x -> let rec helper state' = if Game.status state'.game = OVER then 
                  state' else helper (random_play state') in  
    let (parent, state, lst) = !x in
    if Game.status state.game = OVER && Game.winner state.game = opponent then 
      ((x := (parent, {state with win_score = Pervasives.neg_infinity}, lst)); 
       opponent) else  Game.winner (helper state).game

(**[find_corresponding_node player lst game] is the node in [lst] corresponding
   to [game]. Uses [player] to create the node if no such node exists *)
let find_corresponding_node player lst game = 
  let rec helper = function
    | [] -> Node (ref (Leaf, {game = game; player = player; visit_count = 0; 
                              win_score = 0.0}, []))
    | Leaf :: t -> helper t
    | Node x :: t -> let (_, s, _) = !x in if 
        (Game.equals s.game.board game.board) then Node x else helper t
  in
  helper lst

(**[find_next_move game player end_time] is the move generated after [end_time]
   seconds. Uses [player] so that the AI knows what stone to play *)
let find_next_move game player end_time =
  let opponent = Game.opposite_color player in
  let root = match !tree with 
    | Leaf -> tree := Node (ref (Leaf, {game = game; player = player; 
                                        visit_count = 0; win_score = 0.0}, [])); 
      root_of_tree := !tree; !tree
    | Node x -> let (_, _, lst) = !x in find_corresponding_node player lst game 
  in
  let start_time = Sys.time () in
  let rec helper () = 
    let promising = select_promising_node root in 
    match promising with 
    | Leaf -> failwith "Weird bug?"
    | Node x -> let (parent, state, lst) = !x in if Game.status state.game = 
                                                    IN_PROGRESS then
        expand_node promising;
      let explore = match promising with 
        | Leaf -> failwith "Weird bug 2?"
        | Node x -> let (p, s, lst) = !x in if lst = [] then promising else 
            randomly_select_in lst  
      in 
      let playout_result = simulate_random_playout opponent explore in
      back_propogation explore playout_result;
      if (Sys.time ()) -. start_time < end_time then helper ()
  in helper ();
  let winner = child_with_best_score root in
  match winner with 
  | Leaf -> failwith "That would be weird"
  | Node x -> tree := Node x; let (_, state, _) = !x in state.game

