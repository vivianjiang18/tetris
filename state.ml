open Pieces

type command = Left | Right | RotateCW | RotateCC 
             | Drop | NoUserCommand | Store | HardDrop

type t = {
  piece_info : (Pieces.piece * (int * int));(*based off of (0,0)*)
  coinboard : (int array) array;
  prev_coinboard : (int array) array;
  board : (int array) array;
  score : int;
  level : int;
  height : int;
  width : int;
  prev_position : (int*int);
  prev_piece_coords : Pieces.coordinates;
  clear_lines : int list;
  last_colored_line : int;
  stored_piece_info : Pieces.piece;
  next_piece_info : (Pieces.piece * (int * int));
  sleep_time : float;
  have_swapped : bool;
  prev_command : command;
  coins_collected : int;
}

(** [random_position bw bh] is a randomly selected start position that can
    be spawned in a width range of [bw] and a height of [bh] *)
let random_position bw bh = 
  (Random.int (bw), bh - 4)

(** [random_piece ()] is a randomly selected piece. *)
let random_piece () = 
  Pieces.construct_piece (Random.int 7)

(** [generate_piece bw bh] is a randomly generated piece with an initial
    x-position in the range of (0, bw) and at a height of [bh] *)
let generate_piece bw bh = 
  let piece = random_piece () in
  let len = Pieces.get_len piece.name in
  (piece, random_position (bw-len)bh)

(** [random_coinboard gameboard] makes a random board of the same size as 
    [gameboard] with coins of values 10, 25, and 50 placed randomly on 
    empty spaces.*)
let random_coinboard gameboard : (int array) array = 
  let x_length = Array.length gameboard in
  let y_length = Array.length (gameboard.(0)) in
  let coinboard = (Array.make_matrix (Array.length gameboard) 
                     (Array.length gameboard.(0)) 0) in
  for x = 0 to (x_length - 1) do begin
    for y = 0 to (y_length - 1) do begin
      if (gameboard.(x).(y) = 0) then 
        let randomint = Random.int 200 in 
        if randomint < 1 then coinboard.(x).(y) <- 10 
        else if randomint < 2 then coinboard.(x).(y) <- 25
        else if randomint < 3 then coinboard.(x).(y) <- 50
        else ();
    end done;
  end done;
  coinboard

let init_state bw bh = 
  { 
    piece_info = generate_piece bw bh;
    board = Array.make_matrix bh bw 0;
    prev_coinboard = Array.make_matrix bh bw 0;
    coinboard = random_coinboard (Array.make_matrix bh bw 0);
    score = 0;
    level = 1;
    height = bh;
    width = bw;
    prev_position = (-1, -1); (*off the board*)
    prev_piece_coords = [(-11, -20);(-11, -20);(-11, -20);(-11, -20)];
    clear_lines = [];
    last_colored_line = (-1);
    stored_piece_info = empty_piece (); 
    next_piece_info = generate_piece bw bh;
    sleep_time = 0.4;
    have_swapped = false;
    prev_command = NoUserCommand;
    coins_collected = 0;
  }

let rec is_game_over t acc = 
  if acc = 9 then (t.board.(20).(9) <> 0) 
  else if (t.board.(20).(acc) <> 0) then true
  else is_game_over t (acc+1)

let get_sleep_time state =
  state.sleep_time 

let get_level state = 
  state.level

let get_piece_info state = 
  state.piece_info

let get_next_piece_info state = 
  state.next_piece_info

let get_stored_piece state = 
  state.stored_piece_info

let get_prev_command state = state.prev_command

let get_position state = snd state.piece_info

(** [translate_list position lst acc] is the translated coordinates of the 
    piece shape found in [lst] given the position of the piece on the board 
    from. *)
let translate_list position = 
  List.map (fun x -> ((fst x + fst position), (snd x + snd position))) 

(** [valid_move lst state] is true if a piece does not overlap with filled 
    cells or go out of the bounds of the board, false otherwise.*)
let rec valid_move lst state =
  match lst with
  | [] -> true
  | (x,y)::t -> begin
      if (y >= 23 && x >= 0 && x <= 9) then valid_move t state
      else if (x < 0 || x > 9 || y < 0 || (state.board).(y).(x) = 1 
               || (state.board).(y).(x) = 2) then false 
      else valid_move t state
    end

(** [valid state] is true if the state is valid, false otherwise. *)
let valid state = 
  let list = translate_list (snd state.piece_info) (fst state.piece_info).shape 
  in valid_move list state

(** [fold_helper ary1 ary2] is a maxtrix of all rows in [ary2] that do not
    contain only ones. *)
let fold_helper (arry_accu: (int array) array) (ary:(int array)) = 
  if (Array.fold_left (fun a x-> a && (x=1)) true ary) = false then 
    Array.append arry_accu [|ary|] else arry_accu

let get_rows_to_clear state = 
  let lst = ref [] in 
  for x = 0 to 19 do begin
    if (Array.fold_left (fun a x-> a && (x=1)) true state.board.(x)) = true then 
      lst := (x::!lst) else () end
  done;
  List.rev !lst

(**[last_colored_row st] is the row number of the last row in the board that 
   contained a placed shape. *)
let last_colored_row state = 
  let lst = ref [] in 
  for x = 0 to 23 do begin
    if (Array.fold_left (fun a x-> a || (x=1) || (x=2)) 
          false state.board.(x)) = true then 
      lst := (x::!lst) else () end
  done;
  try (List.hd !lst) with _ -> -1 

(**[clear_lines st] is a matrix of all rows in [st] that did not need to 
   be cleared. Calls [fold_helper ary1 ary 2] *)
let clear_lines st : (int array) array =
  let unfilled_lines = Array.fold_left fold_helper 
      (Array.make_matrix 1 10 3) st.board in
  if ((Array.length unfilled_lines) = 1) then st.board else
    let miniboard = 
      Array.sub unfilled_lines 1 ((Array.length unfilled_lines) -1) in
    let size = 24 - Array.length(miniboard) in
    if size=0 then miniboard else
      Array.append miniboard (Array.make_matrix size 10 0)

(** [shorten_board st] is the board in [st] with an added row at the bottom 
    due to increasing levels. *)
let shorten_board st = 
  if last_colored_row st = st.height - 1 then 
    failwith "game over due to shortened board" 
  else begin 
    let new_line = Array.make_matrix 1 10 2 in 
    let big_board = Array.append new_line st.board in 
    {st with prev_coinboard = st.coinboard; 
             coinboard = random_coinboard (Array.sub big_board 0 24);
             board = Array.sub big_board 0 24 }
  end 

(** [fill_cell lst board] modifies the [board] by setting the number 
    at coordinates denoted in [lst].
    Raises: "fill_cell out of range" when index is out of bounds. *)
let rec fill_cell lst board =
  match lst with
  | [] -> ()
  | (x,y)::t -> 
    try (board.(y).(x) <- 1; fill_cell t board) with 
      _ -> failwith 
             ("fill_cell out of range: "^string_of_int x^string_of_int y) 

(** [fill_board state] is the state with new placed piece and randomized new 
    piece and piece position. *)
let fill_board state = 
  fill_cell (translate_list (snd state.piece_info) (fst state.piece_info).shape)
    state.board;
  let lines_to_clear = get_rows_to_clear state in
  let new_score = (List.length lines_to_clear) * 100 * state.level + state.score 
  in let new_level = if new_score > state.level*1000 && state.level < 20 
       then state.level+1 else state.level in 
  let cleared_state = {state with board = clear_lines state} in
  let new_board = if new_level > 10 && new_level > state.level then
      (shorten_board cleared_state).board else cleared_state.board in
  let old_coins = state.coinboard in 
  {state with prev_position = (-1, -1); (*off the board*)
              prev_piece_coords = [(-11, -20);(-11, -20);(-11, -20);(-11, -20)];
              piece_info = state.next_piece_info;
              clear_lines = lines_to_clear; level = new_level;
              score = new_score; last_colored_line = last_colored_row state;
              board = new_board; prev_coinboard = old_coins;
              coinboard = random_coinboard new_board;
              next_piece_info = generate_piece state.width state.height;
              sleep_time = if new_level <= 10 then 
                  0.4 -. float_of_int (new_level) *. 0.02 else state.sleep_time;
  }

let get_clear_lines st = st.clear_lines

let get_colored_line st = st.last_colored_line

let get_queue_piece state = 
  Pieces.get_shape_init (fst state.next_piece_info).name

let get_hold_piece state = 
  Pieces.get_shape_init (state.stored_piece_info).name

let get_hold_piece_name st = 
  (st.stored_piece_info).name

let get_hold_piece_color state = 
  (state.stored_piece_info).color

let get_coins st = st.coins_collected

let coin_pos st is_current = 
  let coinboard = if is_current then st.coinboard else st.prev_coinboard in
  let rec get_coords x y lst = 
    if x>=0 && x<=9 && y>=0 && y<=19 then begin
      let value = coinboard.(y).(x) in
      if value <> 0 && x=9 then get_coords 0 (y+1) ((x,y, value)::lst)
      else if value <> 0 then get_coords (x+1) y ((x, y, value)::lst)
      else if x = 9 then get_coords 0 (y+1) lst
      else get_coords (x+1) y lst
    end else lst in
  get_coords 0 0 []

(**[updated_coins st] updates coins on th board *)
let update_coins st = 
  if not (valid st) then st else
    let coinboard = st.coinboard in
    let coords_to_check = translate_list (snd st.piece_info) 
        (fst st.piece_info).shape in 
    let coins_to_add = (List.fold_left (fun acc coord -> 
        let condition = fst coord >= 0 && snd coord >= 0 &&
                        snd coord < Array.length coinboard &&
                        fst coord < Array.length coinboard.(0) in
        if condition && coinboard.(snd coord).(fst coord) <> 0 then
          let c = coinboard.(snd coord).(fst coord) in
          (coinboard.(snd coord).(fst coord) <- 0); acc + c
        else if condition then acc
        else acc) st.coins_collected coords_to_check) in
    {st with coinboard = coinboard; coins_collected = coins_to_add}

(** [state_translation st trans_x trans_y] is a new state in which the falling
    piece's x-coordinate is translated by amount trans_x and y-coordinate is
    translated by amount trans_y from the position in [st]. 
    The previous positions are set to the positions of [st]. *)
let state_translation st trans_x trans_y is_trial = 
  let x = (get_position st) |> fst in
  let y  = (get_position st) |> snd in
  let cmd = begin
    if trans_x = (-1) then Left
    else if trans_x = 1 then Right
    else Drop
  end in
  if not is_trial then 
    update_coins {st with prev_piece_coords = (fst st.piece_info).shape; 
                          prev_position = (snd st.piece_info); 
                          piece_info = ((fst st.piece_info),
                                        (x + trans_x, y + trans_y));
                          prev_coinboard = st.coinboard;
                          clear_lines = [];
                          last_colored_line = st.last_colored_line;
                          prev_command = cmd}
  else {st with prev_piece_coords = (fst st.piece_info).shape; 
                prev_position = (snd st.piece_info); 
                piece_info = ((fst st.piece_info),
                              (x + trans_x, y + trans_y));
                prev_coinboard = st.coinboard;
                clear_lines = [];
                last_colored_line = st.last_colored_line;
                prev_command = cmd}

(**[try_drop acc st] drops piece until it is no longer valid*)
let rec try_drop acc state = 
  if (valid state) then try_drop (acc+1) (state_translation state 0 (-1) true)
  else (acc-1)

let get_piece_pos state = 
  translate_list (snd state.piece_info) (fst state.piece_info).shape

(**[get_prev_position st] is a list of integer coordinate where the piece of 
   [st] was previously located.*)
let get_prev_position state =
  translate_list state.prev_position state.prev_piece_coords

let get_drop_coords state = 
  let st = state_translation state 0 (-(try_drop 0 state)) true in
  get_piece_pos st

let get_drop_coords_p state = 
  let pz = {name = (fst state.piece_info).name; 
            color = (fst state.piece_info).color;
            shape = state.prev_piece_coords} in
  let st' = {state with piece_info = (pz, state.prev_position)} in
  let prev_st = state_translation st' 0 (-(try_drop 0 st')) true in
  let curr_st = state_translation state 0 (-(try_drop 0 state)) true in
  let curr_pos = get_piece_pos curr_st in
  let prev_pos = get_piece_pos prev_st in
  List.filter (fun x -> not (List.mem x curr_pos)) prev_pos

let get_piece_color state = 
  (fst state.piece_info).color

let coords_to_erase state =
  let prev_position = get_prev_position state in 
  let curr_position = 
    translate_list (snd state.piece_info) (fst state.piece_info).shape in 
  List.filter (fun x -> not (List.mem x curr_position)) prev_position

let get_score state = 
  state.score

(** [state_rotation st is_cc] is a new state in which the falling piece is
    rotated counter-clockwise if [is_cc] is [true], and clockwise if [is_cc]
    is [false]. Sets previous fields to current fields of [st]. *)
let state_rotation st is_cc = 
  update_coins {st with prev_piece_coords = (fst st.piece_info).shape; 
                        prev_position = (snd st.piece_info); 
                        piece_info = ((rotate (fst st.piece_info) 
                                         is_cc (snd st.piece_info)),
                                      snd st.piece_info);
                        clear_lines = [];
                        last_colored_line = -1}


(**[state_store st] is a new state in which the piece from 
   [st.stored_piece_info] is swapped with the current piece. If the swap creates
   an invalid state then [st] is returned. *)
let state_store st = 
  if not st.have_swapped then begin
    if (st.stored_piece_info = empty_piece ()) then 
      let trial_state = {
        st with piece_info = st.next_piece_info;
                next_piece_info = generate_piece st.width st.height;
                stored_piece_info = (fst st.piece_info);
                prev_position = (snd st.piece_info);
                have_swapped = true; clear_lines = [];
      } in if valid trial_state then trial_state else st
    else (
      let trial_state = {
        st with piece_info = (st.stored_piece_info), 
                             (snd (generate_piece st.width st.height)); 
                stored_piece_info = fst st.piece_info;
                prev_position = (snd st.piece_info);
                prev_piece_coords = (fst st.piece_info).shape;
                have_swapped = true
      } in if valid trial_state then {trial_state with clear_lines = []} 
      else {st with clear_lines = []}) end
  else {st with clear_lines = []}

(** [score_drop st] is the new state with an incremented score based on soft
    dropping a piece.  *)
let score_drop st = 
  {st with score = st.score + 1}

(** [update cmd st] is the new state after applying command [cmd] to 
    state [st]. *)
let update cmd st =
  let trial_st = match cmd with
    | Store -> state_store st
    | Left -> state_translation st (-1) 0 false
    | Right -> state_translation st 1 0 false
    | RotateCW -> state_rotation st false
    | RotateCC -> state_rotation st true
    | Drop -> state_translation st 0 (-1) false
    | HardDrop -> begin
        state_translation st 0 (-(try_drop 0 st)) false
      end
    | NoUserCommand -> state_translation st 0 (-1) false in 
  match cmd with
  | Drop -> if valid trial_st then score_drop {trial_st with clear_lines = []}
    else {(fill_board st) with have_swapped = false; prev_command = cmd}
  | NoUserCommand -> if valid trial_st then {trial_st with clear_lines = []} 
    else {(fill_board st) with have_swapped = false; prev_command = cmd}
  | HardDrop -> {trial_st with score = trial_st.score + 40}
  | _ -> if valid trial_st then 
      ({trial_st with prev_command = cmd; clear_lines = []}) 
    else {st with clear_lines = []}

let get_gray_blocks st =
  if st.level <= 10 then 0
  else (st.level - 10)
