(* 
 TEST PLAN:
  Most of the testing is done visually by game play mainly for two reasons - 
  (1) The Graphics module is used which can't be OUnit tested 
  (2) The actual Tetris game has a randomized factor which doesn't allow 
      testing to know where and which pieces are generated.

  Module tested through OUnit: Pieces, State
  Module tested through Gameplay: Visuals, Main, State

  When OUnit testing was used, glass-box testing was used for the Pieces module
  because there are many functions that are implementation dependent. 
  For example, rotating pieces can be done in various
  ways especially with deciding how to rotate at boundaries. Getting the colors
  of pieces are also specific to what colors are chosen. 

  Black-box testing was used for the State module where the basics of the
  commands were tested, which doesn't require knowledge of the actual 
  implementation as the pieces are represented by coordinates where a drop
  corresponds to a decrement in the y-axis value. 

  The Visuals, Main, and State are tested by gameplay because Main is correct
  if the game starts and Visuals is correct if the game is visually displayed
  correctly. Much of State must also be done through gameplay due to 
  randomization of pieces and the amount of pieces needed for certain
  functions in State to be triggered.  

  This testing approach demonstrates correctness because we've done OUnit
  testing for fundamental operations that are not affected by the randomization
  component of the game. Gameplay testing was also done constantly with each
  change/addition of a function. We also tried to replicate special scenarios
  such as clearing lines in the middle and executing commands right after 
  updates. We've even done some field testing by having outside testers play
  the game.
 *)

open OUnit2
open State
open Pieces

let pp_tuple (k, v) = "\"(" ^ string_of_int k ^ ", " ^ string_of_int v ^ ")\""

let pp_string s = "\""^s^"\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1::(h2::t as t') ->
        if n=100 then acc ^ "..."  (* stop printing long list *)
        else loop (n+1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

let pp_int i = string_of_int

let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

let st = State.init_state 10 24

let pos = State.get_position st


let update_tests = [
  begin
    let st = State.init_state 10 24 in
    let n_st = State.update NoUserCommand st in
    let a, b = State.get_position st in
    let x, y = State.get_position n_st in
    "No User Command Update" >:: (fun _ ->
        assert_equal (a, b-1) 
          (x, y) ~printer:pp_tuple )
  end;
  begin
    let st = State.(init_state 10 24 |> update NoUserCommand) in
    let n_st = State.update Drop st in
    let a, b = State.get_position st in
    let x, y = State.get_position n_st in
    "Drop Update" >:: (fun _ -> 
        assert_equal (a, b-1)
          (x, y) ~printer:pp_tuple ) 
  end;
  begin
    let st = State.(init_state 10 24 |> update NoUserCommand
                    |> update Right) in
    let n_st = State.update Left st in
    let a, b = State.get_position st in
    let x, y = State.get_position n_st in
    "Left Update" >:: (fun _ -> 
        assert_equal (a - 1, b)
          (x, y) ~printer:pp_tuple )
  end;
  begin
    let st = State.(init_state 10 24 |> update NoUserCommand
                    |> update Left) in
    let n_st = State.update Right st in
    let a, b = State.get_position st in
    let x, y = State.get_position n_st in
    "Right Update" >:: (fun _ -> 
        assert_equal (a+1, b)
          (x, y) ~printer:pp_tuple )
  end;
  begin
    let st = State.(init_state 10 24 |> update NoUserCommand
                    |> update Drop) in
    let n_st = State.update RotateCW st in
    let a, b = State.get_position st in
    let x, y = State.get_position n_st in
    "Rotote CW Update" >:: (fun _ -> 
        assert_equal (a, b)
          (x, y) ~printer:pp_tuple )
  end;
  begin
    let st = State.(init_state 10 24 |> update NoUserCommand
                    |> update Drop) in
    let n_st = State.update RotateCC st in
    let a, b = State.get_position st in
    let x, y = State.get_position n_st in
    "Rotote CC Update" >:: (fun _ -> 
        assert_equal (a, b)
          (x, y) ~printer:pp_tuple )
  end;
  begin
    let st = State.(init_state 10 24 |> update NoUserCommand
                    |> update Drop) in
    let n_st = State.update Store st in
    let x, y = State.get_position n_st in
    "Hold Update" >:: (fun _ -> 
        assert_equal y (24-4) 
          ~printer:string_of_int )
  end;
  begin
    let st = State.(init_state 10 24 |> update NoUserCommand
                    |> update Drop) in
    let n_st = State.update HardDrop st in
    let a, b = State.get_position st in
    let x, y = State.get_position n_st in
    "Hard Drop Update" >:: (fun _ -> 
        assert_equal (x, y)
          (a, 0) 
          ~printer:pp_tuple )
  end;
]

let pieces_tests = [
  "Construct I" >:: (fun _ ->
      assert_equal 
        {name = I; color = Graphics.rgb 255 77 77; 
         shape = [(0, 0); (1, 0); (2, 0); (3, 0)]} 
        (construct_piece 0));
  "Construct L" >:: (fun _ ->
      assert_equal
        {name = L; color = Graphics.rgb 255 187 51;
         shape = [(0, 0); (0, 1); (1, 0); (2, 0)]}
        (construct_piece 1));
  "Construct J" >:: (fun _ ->
      assert_equal
        {name = J; color = Graphics.rgb 255 255 102; 
         shape= [(0,0); (1,0); (2,0); (2,1)]}
        (construct_piece 2));
  "Construct O" >:: (fun _ ->
      assert_equal
        {name = O; color = Graphics.rgb 85 128 0; 
         shape = [(0,0); (1,0); (0,1); (1,1)]}
        (construct_piece 3));
  "Construct S" >:: (fun _ ->
      assert_equal
        {name = S; color = Graphics.rgb 102 127 255; 
         shape= [(0,0); (1,0); (1,1); (2,1)]}
        (construct_piece 4));
  "Construct T" >:: (fun _ ->
      assert_equal
        {name = T; color = Graphics.rgb 170 0 255; 
         shape = [(0,0); (1,0); (2,0); (1,1)]}
        (construct_piece 5));
  "Construct Z" >:: (fun _ ->
      assert_equal
        {name = Z; color = Graphics.rgb 255 153 238; 
         shape = [(0,1); (1,1); (1,0); (2,0)]}
        (construct_piece 6));
  "Shape Init I" >:: (fun _ ->
      assert_equal [(0,0); (1,0); (2,0); (3,0)] (get_shape_init I) 
        ~printer:(pp_list pp_tuple));
  "Shape Init L" >:: (fun _ ->
      assert_equal [(0,0); (0,1); (1,0); (2,0)] (get_shape_init L)
        ~printer:(pp_list pp_tuple));
  "Shape Init J" >:: (fun _ ->
      assert_equal [(0,0); (1,0); (2,0); (2,1)] (get_shape_init J)
        ~printer:(pp_list pp_tuple));
  "Shape Init O" >:: (fun _ ->
      assert_equal [(0,0); (1,0); (0,1); (1,1)] (get_shape_init O)
        ~printer:(pp_list pp_tuple));
  "Shape Init S" >:: (fun _ ->
      assert_equal [(0,0); (1,0); (1,1); (2,1)] (get_shape_init S)
        ~printer:(pp_list pp_tuple));
  "Shape Init T" >:: (fun _ ->
      assert_equal [(0,0); (1,0); (2,0); (1,1)] (get_shape_init T)
        ~printer:(pp_list pp_tuple));
  "Shape Init Z" >:: (fun _ ->
      assert_equal [(0,1); (1,1); (1,0); (2,0)] (get_shape_init Z)
        ~printer:(pp_list pp_tuple));
  "Shape Init E" >:: (fun _ ->
      assert_equal [(0,0)] (get_shape_init E)
        ~printer:(pp_list pp_tuple));
  "Empty Piece Construct" >:: (fun _ ->
      assert_equal {name = E; color = Graphics.rgb 0 34 51; shape = []}
        (empty_piece ()));
]

let rotate_tests = [
  begin
    let piece = {name = I; color = Graphics.black; 
                 shape = [(0,0); (1,0); (2,0); (3,0)]} in
    let piece' = {name = I; color = Graphics.black; 
                  shape = [(0,0); (0, 1); (0, 2); (0, 3)]} in
    let rotated = rotate piece true (3, 10) in
    "Normal rotate I CC shape" >:: (fun _ ->
        assert_equal piece'.shape rotated.shape
          ~cmp:cmp_set_like_lists)
  end;
  begin
    let piece = {name = I; color = Graphics.black;
                 shape = [(0,0); (1,0); (2,0); (3,0)]} in
    let piece' = {name = I; color = Graphics.black; 
                  shape = [(0,0); (0, 1); (0, 2); (0, 3)]} in
    let rotated = rotate piece true (3, 10) in
    "Normal rotate I CC name" >:: (fun _ ->
        assert_equal piece'.name rotated.name)
  end;
  begin
    let piece = {name = I; color = Graphics.black; 
                 shape = [(0,0); (1,0); (2,0); (3,0)]} in
    let piece' = {name = I; color = Graphics.black; 
                  shape = [(0,0); (0, 1); (0, 2); (0, 3)]} in
    let rotated = rotate piece true (3, 10) in
    "Normal rotate I CC color" >:: (fun _ ->
        assert_equal piece'.color rotated.color)
  end;
  begin
    let piece = {name = L; color = Graphics.red; 
                 shape = [(0,0); (0,1); (1,0); (2,0)]} in
    let piece' = {name = L; color = Graphics.red; 
                  shape = [(1,0); (0,0); (1, 1);(1, 2)]} in
    let rotated = rotate piece true (3, 10) in
    "Normal rotate L CC shape" >:: (fun _ ->
        assert_equal piece'.shape rotated.shape
          ~cmp:cmp_set_like_lists)
  end;
  begin
    let piece = {name = L; color = Graphics.red; 
                 shape = [(0,0); (0,1); (1,0); (2,0)]} in
    let piece' = {name = L; color = Graphics.red; 
                  shape = [(1,0); (0,0); (1, 1);(1, 2)]} in
    let rotated = rotate piece true (3, 10) in
    "Normal rotate L CC name" >:: (fun _ ->
        assert_equal piece'.name rotated.name)
  end;
  begin
    let piece = {name = L; color = Graphics.red; 
                 shape = [(0,0); (0,1); (1,0); (2,0)]} in
    let piece' = {name = L; color = Graphics.red; 
                  shape = [(1,0); (0,0); (1, 1);(1, 2)]} in
    let rotated = rotate piece true (3, 10) in
    "Normal rotate L CC color" >:: (fun _ ->
        assert_equal piece'.color rotated.color)
  end;
  begin
    let piece = {name = J; color = Graphics.black; 
                 shape = [(0,0); (1,0); (2,0); (2,1)]} in
    let piece' = {name = J; color = Graphics.black; 
                  shape = [(1,0); (1,1); (1, 2);(0, 2)]} in
    let rotated = rotate piece true (3, 10) in
    "Normal rotate J CC shape" >:: (fun _ ->
        assert_equal piece'.shape rotated.shape
          ~cmp:cmp_set_like_lists)
  end;
  begin
    let piece = {name = J; color = Graphics.black; 
                 shape = [(0,0); (1,0); (2,0); (2,1)]} in
    let piece' = {name = J; color = Graphics.black; 
                  shape = [(1,0); (1,1); (1, 2);(0, 2)]} in
    let rotated = rotate piece true (3, 10) in
    "Normal rotate J CC name" >:: (fun _ ->
        assert_equal piece'.name rotated.name)
  end;
  begin
    let piece = {name = J; color = Graphics.black; 
                 shape = [(0,0); (1,0); (2,0); (2,1)]} in
    let piece' = {name = J; color = Graphics.black; 
                  shape = [(1,0); (1,1); (1, 2);(0, 2)]} in
    let rotated = rotate piece true (3, 10) in
    "Normal rotate J CC color" >:: (fun _ ->
        assert_equal piece'.color rotated.color)
  end;
  begin
    let piece = {name = O; color = Graphics.yellow; 
                 shape = [(0,0); (1,0); (0,1); (1,1)]} in
    let piece' = {name = O; color = Graphics.yellow; 
                  shape = [(1,0); (1,1); (0, 0);(0, 1)]} in
    let rotated = rotate piece true (3, 10) in
    "Normal rotate O CC shape" >:: (fun _ ->
        assert_equal piece'.shape rotated.shape
          ~cmp:cmp_set_like_lists)
  end;
  begin
    let piece = {name = O; color = Graphics.yellow; 
                 shape = [(0,0); (1,0); (0,1); (1,1)]} in
    let piece' = {name = O; color = Graphics.yellow; 
                  shape = [(1,0); (1,1); (0, 0);(0, 1)]} in
    let rotated = rotate piece true (3, 10) in
    "Normal rotate O CC name" >:: (fun _ ->
        assert_equal piece'.name rotated.name)
  end;
  begin
    let piece = {name = O; color = Graphics.yellow; 
                 shape = [(0,0); (1,0); (0,1); (1,1)]} in
    let piece' = {name = O; color = Graphics.yellow; 
                  shape = [(1,0); (1,1); (0, 0);(0, 1)]} in
    let rotated = rotate piece true (3, 10) in
    "Normal rotate O CC color" >:: (fun _ ->
        assert_equal piece'.color rotated.color)
  end;
  begin
    let piece = construct_piece 4 in
    let piece' = {name = S; color = Graphics.rgb 102 127 255; 
                  shape = [(0, 2); (0, 1); (1, 1); (1, 0)]} in
    let rotated = rotate piece true (3, 10) in
    "Nomral rotate S CC shape" >:: (fun _ ->
        assert_equal piece'.shape rotated.shape
          ~cmp:cmp_set_like_lists)
  end;
  begin
    let piece = construct_piece 4 in
    let piece' = {name = S; color = Graphics.rgb 102 127 255; 
                  shape = [(0, 2); (0, 1); (1, 1); (1, 0)]} in
    let rotated = rotate piece true (3, 10) in
    "Nomral rotate S CC name" >:: (fun _ ->
        assert_equal piece'.name rotated.name)
  end;
  begin
    let piece = construct_piece 4 in
    let piece' = {name = S; color = Graphics.rgb 102 127 255; 
                  shape = [(0, 2); (0, 1); (1, 1); (1, 0)]} in
    let rotated = rotate piece true (3, 10) in
    "Nomral rotate S CC color" >:: (fun _ ->
        assert_equal piece'.color rotated.color)
  end;
  begin
    let piece = construct_piece 5 in
    let piece' = {name = T; color = Graphics.rgb 170 0 255; 
                  shape = [(0, 1); (1, 2); (1, 1); (1, 0)]} in
    let rotated = rotate piece true (3, 10) in
    "Nomral rotate T CC shape" >:: (fun _ ->
        assert_equal piece'.shape rotated.shape
          ~cmp:cmp_set_like_lists)
  end;
  begin
    let piece = construct_piece 5 in
    let piece' = {name = T; color = Graphics.rgb 170 0 255; 
                  shape = [(0, 1); (1, 2); (1, 1); (1, 0)]} in
    let rotated = rotate piece true (3, 10) in
    "Nomral rotate T CC name" >:: (fun _ ->
        assert_equal piece'.name rotated.name)
  end;
  begin
    let piece = construct_piece 5 in
    let piece' = {name = T; color = Graphics.rgb 170 0 255; 
                  shape = [(0, 1); (1, 2); (1, 1); (1, 0)]} in
    let rotated = rotate piece true (3, 10) in
    "Nomral rotate T CC color" >:: (fun _ ->
        assert_equal piece'.color rotated.color)
  end;
  begin
    let piece = construct_piece 6 in
    let piece' = {name = Z; color = Graphics.rgb 255 153 238; 
                  shape = [(0, 0); (0, 1); (1, 1); (1, 2)]} in
    let rotated = rotate piece true (3, 10) in
    "Nomral rotate Z CC shape" >:: (fun _ ->
        assert_equal piece'.shape rotated.shape
          ~cmp:cmp_set_like_lists)
  end;
  begin
    let piece = construct_piece 6 in
    let piece' = {name = Z; color = Graphics.rgb 255 153 238; 
                  shape = [(0, 0); (0, 1); (1, 1); (1, 2)]} in
    let rotated = rotate piece true (3, 10) in
    "Nomral rotate Z CC name" >:: (fun _ ->
        assert_equal piece'.name rotated.name)
  end;
  begin
    let piece = construct_piece 6 in
    let piece' = {name = Z; color = Graphics.rgb 255 153 238; 
                  shape = [(0, 0); (0, 1); (1, 1); (1, 2)]} in
    let rotated = rotate piece true (3, 10) in
    "Nomral rotate Z CC color" >:: (fun _ ->
        assert_equal piece'.color rotated.color)
  end;
]

let getter_tests = [
  begin
    let st = State.init_state 10 24 in
    let color = State.get_piece_color st in
    let n_st = State.update NoUserCommand st in
    let color' = State.get_piece_color n_st in
    "Color Getter" >:: (fun _ ->
        assert_equal color color')
  end;
  begin
    let st = State.(init_state 10 24 ) in
    let piece_color = State.get_piece_color st in
    let n_st = State.update Store st in
    let piece_color' = State.get_hold_piece_color n_st in
    "Hold Color Getter" >:: (fun _ ->
        assert_equal piece_color piece_color' )
  end;
  begin
    let st = State.(init_state 10 24 ) in
    let piece_pos = Pieces.get_shape_init 
        (fst (State.get_piece_info st)).name in
    let n_st = State.update Store st in
    let piece_pos' = State.get_hold_piece n_st in
    "Hold Pos Getter" >:: (fun _ ->
        assert_equal piece_pos piece_pos'
          ~cmp:cmp_set_like_lists )
  end;
  begin
    let st = State.(init_state 10 24) in
    let info = State.get_next_piece_info st in
    let n_st = State.(update HardDrop st |> update NoUserCommand) in
    let info' = State.get_piece_info n_st in
    "Next Piece Getter" >:: (fun _ ->
        assert_equal info info')
  end;
  begin
    let st = State.(init_state 10 24) in
    let pos = State.get_drop_coords st in
    let n_st = State.update HardDrop st in
    let pos' = State.get_piece_pos n_st in
    "Drop Coords and Curr Coords" >:: (fun _ ->
        assert_equal pos pos'
          ~cmp:cmp_set_like_lists)
  end;
  begin
    let st = State.(init_state 10 24) in
    let coins = State.get_coins st in
    "Coin Getter" >:: (fun _ ->  
        assert_equal coins 0
          ~printer:string_of_int)
  end;
  begin
    let st = State.(init_state 10 24) in
    let lvl = State.get_level st in
    "Level Getter" >:: (fun _ ->
        assert_equal lvl 1 
          ~printer:string_of_int)
  end;
  begin
    let st = State.(init_state 10 24) in
    let cmd = State.get_prev_command st in
    "Init Command" >:: (fun _ ->
        assert_equal cmd NoUserCommand)
  end;
  begin
    let st = State.(init_state 10 24  |> update NoUserCommand) in
    let cmd = State.get_prev_command st in
    "NoUserCommand Prev Cmd" >:: (fun _ ->
        assert_equal cmd Drop)
  end;
  begin
    let st = State.(init_state 10 24  |> update Drop) in
    let cmd = State.get_prev_command st in
    "Drop Prev Cmd" >:: (fun _ ->
        assert_equal cmd Drop)
  end;
  begin
    let st = State.(init_state 10 24  |> update RotateCC) in
    let cmd = State.get_prev_command st in
    "CC Prev Cmd" >:: (fun _ ->
        assert_equal cmd RotateCC)
  end;
  begin
    let st = State.(init_state 10 24  |> update RotateCW) in
    let cmd = State.get_prev_command st in
    "CW Prev Cmd" >:: (fun _ ->
        assert_equal cmd RotateCW)
  end;
  begin
    let st = State.(init_state 10 24  |> update Left) in
    let cmd = State.get_prev_command st in
    "Left Prev Cmd" >:: (fun _ ->
        assert_equal cmd Left)
  end;
  begin
    let st = State.(init_state 10 24  |> update Right) in
    let cmd = State.get_prev_command st in
    "Right Prev Cmd" >:: (fun _ ->
        assert_equal cmd Right)
  end;
]

let dropping_tests = [
  begin
    let st = State.init_state 10 24 in
    let n_st = State.update NoUserCommand st in
    let pos = State.get_drop_coords st in
    let prev_pos = State.get_drop_coords n_st in
    "Hard Drop Position" >:: (fun _ ->
        assert_equal pos prev_pos ~cmp:cmp_set_like_lists)
  end;
  begin
    let st = State.(init_state 10 24 |> update NoUserCommand)in
    let n_st = State.update NoUserCommand st in
    let pos = State.get_drop_coords st in
    let prev_pos = State.get_drop_coords n_st in
    "Hard Drop Position" >:: (fun _ ->
        assert_equal pos prev_pos ~cmp:cmp_set_like_lists)
  end;
]

let fill_board_tests = [
  begin
    let st = State.(init_state 10 24 |> update HardDrop) in
    let n_st = State.update NoUserCommand st in
    (* let a, b = State.get_position st in
       let x, y = State.get_position n_st in *)
    "Fill Board One Piece" >:: (fun _ ->
        assert_equal ((State.get_colored_line n_st) > (-1))
          true
          ~printer:string_of_bool)
  end;
]

let suite = "state test suite" >::: List.flatten [
    update_tests;
    getter_tests;
    pieces_tests;
    rotate_tests;
    dropping_tests;
    fill_board_tests;
  ]

let _ = run_test_tt_main suite
