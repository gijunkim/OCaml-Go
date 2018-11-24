open OUnit2
open Game

module GameTesting = struct 

  let check_ko fx = 
    match fx () with 
    | exception Ko -> true
    | _ -> false

  let check_suicide fx = 
    match fx () with 
    | exception InvalidPlacement -> true
    | _ -> false

  (* Assumes valid moves *)
  let rec seq_moves board configs moves = 
    match moves with 
    | ((x,y), stone)::t -> 
      let x = move stone (x, y) board configs in 
      seq_moves x.board x.configs t
    | [] -> {board = board; configs = configs}

  (* These are the boards/configuration lists being tested *)
  let board_3 = initialize 3;;
  let board_center = (seq_moves (initialize 3) [] 
                        [((0,1), B); ((1,2), B); ((2, 1), B); 
                         ((1, 1),W); ((1,0), B)]).board;;
  let board_side = (seq_moves (initialize 3) [] 
                      [((0,0), B); ((1,1), B); ((0, 1), W); ((2, 0), B)]).board;;
  let board_side_2 = (seq_moves (initialize 3) [] 
                        [((0,0), B); ((1,1), B); ((0,1), W); ((0,2), W); 
                         ((1,2), B)]).board;;
  let board_corner = (seq_moves (initialize 3) [] 
                        [((0,0), B); ((0,1), B); ((1, 0), W)]).board;;
  let board_corner_2 = (seq_moves (initialize 3) [] 
                          [((0,0), W); ((1,1), W); ((2, 2), B); ((0,1), B); 
                           ((0,2), B); ((1,0), B); ((1,2), B); ((2,0), B); 
                           ((2,1), B)]).board;;
  let board_side = (seq_moves (initialize 3) [] 
                      [((0,0), B); ((1,1), B); ((0, 1), W); ((2, 0), B)]).board;;
  let board_sui_side = (seq_moves (initialize 3) [] 
                          [((0,0), B); ((1,1), B); ((0, 2), B)]).board;;
  let board_eye = (seq_moves (initialize 3) [] 
                     [((0,2), B); ((1,1), B); ((1,0), B); ((0, 1), W)]).board;;
  let board_m = (seq_moves (initialize 3) [] 
                   [((0,0), W); ((0,1), W); ((1, 0), B); ((1, 1), B); ((0, 2), B)]).board;;
  let filled_board = (seq_moves board_3 [] 
                        [((0,1), B); ((1,2), B); ((2, 1), B); ((1,0), B)]).board;;
  let filled_board_2 = (seq_moves board_3 [] 
                          [((0,1), B); ((1,2), B)]).board;;
  let filled_board_9 = (seq_moves (initialize 9) [] 
                          [((0,1), B); ((1,2), W); ((2, 1), B); 
                           ((1,0), W); ((5,6), B); ((1,7), W); ((3,5), B)]).board;;
  let board_score = (seq_moves board_3 [] [((0,0), B); 
                                           ((0,1), W); ((2,0), B); ((1,1), W)]).board;;
  let board_4_score = (seq_moves (initialize 4) [] 
                         [((0,0), B); ((0,1), W); ((2,0), B); 
                          ((1,1), W); ((1, 0), W); ((3, 3), B)]).board;;
  let board_9_score = (seq_moves (initialize 9) [] 
                         [((0,0), B); ((3,1), W); ((2, 2), B); ((1,1), W); 
                          ((8,0), B); ((5,1), W); ((6,0), B); ((6,7), W);
                          ((7,7), B); ((2,4), W); ((6,8), B); ((3,6), W)]).board;;
  let ko_data = seq_moves (initialize 5) [] 
      [((2, 1), B); ((1, 2), B); ((3, 2), B); ((2, 2), W); ((1, 3), W); 
       ((3, 3), W); ((2, 4), W); ((2, 3), B)];;
  let ko_0_data = seq_moves (initialize 4) [] [((0,3),W); ((1,2),W); 
                                               ((2,3),W); ((2,2),B); ((3,3),B);
                                               ((1,3),B)];;
  let ko_2_data = seq_moves (initialize 4) [] [((0,3),W); ((1,2),W); ((2,3),W); 
                                               ((2,2),B); ((3,3),B); ((1,3),B);
                                               ((0,0),W); ((1,0),B); ((2, 3), W)];;
  let ko_3_data = seq_moves (initialize 3) [] [((0,2),W); ((1,2),W); ((2,1),W); 
                                               ((1,0),B); ((2,0),B); ((1,1),B); 
                                               ((2,2),B)];;
  let ko_4_data = seq_moves (initialize 5) [] [((1, 4), B); ((3,4), B); ((4,4), B); 
                                               ((2,2), W); ((3,2), W); ((3,3), W); 
                                               ((1,3), W); ((2,4), W); ((2,3), B)];;
  let ko_5_data = seq_moves ko_data.board ko_data.configs [((0, 0), W)];;
  let ko_6_data = seq_moves ko_5_data.board ko_5_data.configs [((0, 1), B)];;
  let ko_7_data = seq_moves ko_6_data.board ko_6_data.configs [((1, 0), W)];;  
  let ko_8_data = seq_moves ko_7_data.board ko_7_data.configs [((1, 1), B)];; 

  let create_game_test name expected real = 
    name >:: (fun _ -> assert_equal expected real)
  let game_tests = 
    [
      (* Tests for initialize *)
      create_game_test "initialize size 2" 
        [((0,0), Blank); ((0,1), Blank); ((1,0), Blank); ((1,1), Blank)]
        (initialize 2);
      create_game_test "initialize size 1" 
        [((0,0), Blank)]
        (initialize 1);
      create_game_test "initialize size 3" 
        [((0, 0), Blank); ((0, 1), Blank); ((0, 2), Blank); ((1, 0), Blank);
         ((1, 1), Blank); ((1, 2), Blank); ((2, 0), Blank); ((2, 1), Blank);
         ((2, 2), Blank)]
        (initialize 3);
      create_game_test "initialize size 4" 
        [((0, 0), Blank); ((0, 1), Blank); ((0, 2), Blank); ((1, 0), Blank);
         ((1, 1), Blank); ((1, 2), Blank); ((2, 0), Blank); ((2, 1), Blank);
         ((2, 2), Blank)]
        (initialize 3);
      create_game_test "initialize size 5"
        [((0, 0), Blank); ((0, 1), Blank); ((0, 2), Blank); ((0, 3), Blank);
         ((0, 4), Blank); ((1, 0), Blank); ((1, 1), Blank); ((1, 2), Blank);
         ((1, 3), Blank); ((1, 4), Blank); ((2, 0), Blank); ((2, 1), Blank);
         ((2, 2), Blank); ((2, 3), Blank); ((2, 4), Blank); ((3, 0), Blank);
         ((3, 1), Blank); ((3, 2), Blank); ((3, 3), Blank); ((3, 4), Blank);
         ((4, 0), Blank); ((4, 1), Blank); ((4, 2), Blank); ((4, 3), Blank);
         ((4, 4), Blank)] (initialize 5);
      create_game_test "initialize size 9"
        [((0, 0), Blank); ((0, 1), Blank); ((0, 2), Blank); ((0, 3), Blank);
         ((0, 4), Blank); ((0, 5), Blank); ((0, 6), Blank); ((0, 7), Blank);
         ((0, 8), Blank); ((1, 0), Blank); ((1, 1), Blank); ((1, 2), Blank);
         ((1, 3), Blank); ((1, 4), Blank); ((1, 5), Blank); ((1, 6), Blank);
         ((1, 7), Blank); ((1, 8), Blank); ((2, 0), Blank); ((2, 1), Blank);
         ((2, 2), Blank); ((2, 3), Blank); ((2, 4), Blank); ((2, 5), Blank);
         ((2, 6), Blank); ((2, 7), Blank); ((2, 8), Blank); ((3, 0), Blank);
         ((3, 1), Blank); ((3, 2), Blank); ((3, 3), Blank); ((3, 4), Blank);
         ((3, 5), Blank); ((3, 6), Blank); ((3, 7), Blank); ((3, 8), Blank);
         ((4, 0), Blank); ((4, 1), Blank); ((4, 2), Blank); ((4, 3), Blank);
         ((4, 4), Blank); ((4, 5), Blank); ((4, 6), Blank); ((4, 7), Blank);
         ((4, 8), Blank); ((5, 0), Blank); ((5, 1), Blank); ((5, 2), Blank);
         ((5, 3), Blank); ((5, 4), Blank); ((5, 5), Blank); ((5, 6), Blank);
         ((5, 7), Blank); ((5, 8), Blank); ((6, 0), Blank); ((6, 1), Blank);
         ((6, 2), Blank); ((6, 3), Blank); ((6, 4), Blank); ((6, 5), Blank);
         ((6, 6), Blank); ((6, 7), Blank); ((6, 8), Blank); ((7, 0), Blank); 
         ((7, 1), Blank); ((7, 2), Blank); ((7, 3), Blank); ((7, 4), Blank); 
         ((7, 5), Blank); ((7, 6), Blank); ((7, 7), Blank); ((7, 8), Blank); 
         ((8, 0), Blank); ((8, 1), Blank); ((8, 2), Blank); ((8, 3), Blank); 
         ((8, 4), Blank); ((8, 5), Blank); ((8, 6), Blank); ((8, 7), Blank);
         ((8, 8), Blank)] (initialize 9);

      (* Tests for score *)
      create_game_test "score, size 1, naive case"
        (0.0, 5.5) (score (initialize 1));
      create_game_test "score, nothing to test handicap" 
        (0.0,5.5) (score board_3);
      create_game_test "score, size 3" 
        (2.0,7.5) (score board_score);
      create_game_test "score, size 4, captures" 
        (2.0,9.5) (score board_4_score);
      create_game_test "score, size 9, no captures" 
        (6.0,11.5) (score board_9_score);

      (* Tests for move: suicides and regular moves *)
      create_game_test "move naive case size 3" 
        [((0, 0), Blank); ((0, 1), B); ((0, 2), Blank); ((1, 0), Blank);
         ((1, 1), Blank); ((1, 2), Blank); ((2, 0), Blank); ((2, 1), Blank);
         ((2, 2), Blank)]
        ((move B (0,1) board_3 [])).board;
      create_game_test "move naive case size 9" 
        [((0, 0), Blank); ((0, 1), B); ((0, 2), Blank); ((0, 3), Blank);
         ((0, 4), Blank); ((0, 5), Blank); ((0, 6), Blank); ((0, 7), Blank);
         ((0, 8), Blank); ((1, 0), W); ((1, 1), Blank); ((1, 2), W); ((1, 3), Blank);
         ((1, 4), Blank); ((1, 5), Blank); ((1, 6), Blank); ((1, 7), W);
         ((1, 8), Blank); ((2, 0), Blank); ((2, 1), B); ((2, 2), Blank);
         ((2, 3), Blank); ((2, 4), Blank); ((2, 5), Blank); ((2, 6), Blank);
         ((2, 7), Blank); ((2, 8), Blank); ((3, 0), Blank); ((3, 1), Blank);
         ((3, 2), Blank); ((3, 3), Blank); ((3, 4), Blank); ((3, 5), B);
         ((3, 6), Blank); ((3, 7), Blank); ((3, 8), Blank); ((4, 0), Blank);
         ((4, 1), Blank); ((4, 2), Blank); ((4, 3), Blank); ((4, 4), Blank);
         ((4, 5), Blank); ((4, 6), Blank); ((4, 7), Blank); ((4, 8), B);
         ((5, 0), Blank); ((5, 1), Blank); ((5, 2), Blank); ((5, 3), Blank);
         ((5, 4), Blank); ((5, 5), Blank); ((5, 6), B); ((5, 7), Blank);
         ((5, 8), Blank); ((6, 0), Blank); ((6, 1), Blank);
         ((6, 2), Blank); ((6, 3), Blank); ((6, 4), Blank); ((6, 5), Blank);
         ((6, 6), Blank); ((6, 7), Blank); ((6, 8), Blank); ((7, 0), Blank); 
         ((7, 1), Blank); ((7, 2), Blank); ((7, 3), Blank); ((7, 4), Blank); 
         ((7, 5), Blank); ((7, 6), Blank); ((7, 7), Blank); ((7, 8), Blank); 
         ((8, 0), Blank); ((8, 1), Blank); ((8, 2), Blank); ((8, 3), Blank); 
         ((8, 4), Blank); ((8, 5), Blank); ((8, 6), Blank); ((8, 7), Blank);
         ((8, 8), Blank)]
        ((move B (4, 8) filled_board_9 [])).board;
      create_game_test "move capture center" 
        [((0, 0), Blank); ((0, 1), B); ((0, 2), Blank); ((1, 0), B); ((1, 1), Blank);
         ((1, 2), B); ((2, 0), Blank); ((2, 1), B); ((2, 2), Blank)]
        board_center;
      create_game_test "move capture side" 
        [((0, 0), B); ((0, 1), W); ((0, 2), Blank); ((1, 0), Blank); ((1, 1), B);
         ((1, 2), Blank); ((2, 0), B); ((2, 1), Blank); ((2, 2), Blank)]
        board_side;
      create_game_test "move capture corner" 
        [((0, 0), B); ((0, 1), B); ((0, 2), Blank); ((1, 0), W); ((1, 1), Blank);
         ((1, 2), Blank); ((2, 0), Blank); ((2, 1), Blank); ((2, 2), Blank)]
        board_corner;
      create_game_test "move, capture multiple center"
        [((0, 0), Blank); ((0, 1), Blank); ((0, 2), B); ((1, 0), B); ((1, 1), B);
         ((1, 2), Blank); ((2, 0), Blank); ((2, 1), Blank); ((2, 2), Blank)]
        board_m;
      create_game_test "move, capture multiple side"
        [((0, 0), B); ((0, 1), Blank); ((0, 2), Blank); ((1, 0), Blank); ((1, 1), B);
         ((1, 2), B); ((2, 0), Blank); ((2, 1), Blank); ((2, 2), Blank)]
        board_side_2;
      create_game_test "move, capture multiple corner"
        [((0, 0), Blank); ((0, 1), B); ((0, 2), B); ((1, 0), B); ((1, 1), Blank);
         ((1, 2), B); ((2, 0), B); ((2, 1), B); ((2, 2), B)]
        board_corner_2;
      create_game_test "move, suicide, edge" 
        true (check_suicide (fun () -> (move W (0,1) board_sui_side [])));
      create_game_test "move, suicide, cover own eye" 
        true (check_suicide (fun () -> (move W (0,0) board_eye [])));
      create_game_test "move, suicide, center" 
        true (check_suicide (fun () -> (move W (1,1) filled_board [])));
      create_game_test "move, suicide, touching corner" 
        true (check_suicide (fun () -> (move W (0,2) filled_board_2 [])));

      (* Tests for move: Ko rule *)
      create_game_test "move, but testing ko rule in the center"
        true (check_ko (fun () -> (move W (2,2) ko_data.board ko_data.configs)));
      create_game_test "move, but testing ko rule with a side"
        true (check_ko (fun () -> (move W (2,3) ko_0_data.board ko_0_data.configs)));
      create_game_test "move, but testing ko rule in the corner"
        true (check_ko (fun () -> (move W (2,1) ko_3_data.board ko_3_data.configs)));
      create_game_test "move, but testing ko rule in the center again"
        true (check_ko (fun () -> (move W (2,4) ko_4_data.board ko_4_data.configs)));
      create_game_test "move, but testing ko rules with 1 move after capture"
        false (check_ko (fun () -> (move W (2,2) ko_5_data.board ko_5_data.configs)));
      create_game_test "move, but testing ko rules with 2 moves after capture"
        false (check_ko (fun () -> (move W (2,2) ko_6_data.board ko_6_data.configs)));
      create_game_test "move, but testing ko rules with 3 moves after capture"
        false (check_ko (fun () -> (move W (2,2) ko_7_data.board ko_7_data.configs)));
      create_game_test "move, but testing ko rules with 4 moves after capture"
        false (check_ko (fun () -> (move W (2,2) ko_8_data.board ko_8_data.configs)));
      create_game_test "move, but testing ko rules with random other pieces in board"
        true (check_ko (fun () -> (move B (1,3) ko_2_data.board ko_2_data.configs)));
    ] 
end;;

let suite = "game test suite" >::: List.flatten 
              [
                GameTesting.game_tests;
              ]

let _ = run_test_tt_main suite