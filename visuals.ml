open Graphics

type position = {x: int; y: int; t_size:int }
let score_pos = {x = 350; y = 300; t_size = 20}
let coins_pos = {x = 350; y = 260; t_size = 20}
let game_over_pos = {x = 225; y = 400; t_size = 150}
let welcome_pos = {x = 190; y = 400; t_size = 300}
let key_prompt_pos = {x = 200; y = 250; t_size = 70}
let hold_pos = {x = 315; y = 480; t_size = 20}
let queue_pos = {x = 315; y = 0; t_size = 20}
let level_pos = {x = 350; y = 280; t_size = 20}
let ten_pos = {x = 200; y = 400; t_size = 50}

let user_name = ref ""
let score_sc = ref []
let start_row = ref 0
let unlocked_schemes = ref []


let sleep_time = ref 0.4
let scheme = ref 0

let scheme_values = [|0; 1000; 1000; 1000; 3110; 5000; 7000; 7000; 7000; 7000; 
                      50000; 30000; 40000|]

(**[fill_spaces color lst] is a unit that draws rectangles of color [color]
   in the Graphics board using coordinates from list [lst] *)
let rec fill_spaces color = function
  | [] -> ()
  | h::t -> begin
      set_color color;
      fill_rect (fst h) (snd h) 30 30; 
      fill_spaces color t
    end

(**[check_key ()] is a valid command based on user-input from keyboard. *)
let check_key () = 
  if key_pressed () then
    match read_key () with
    | 'l' -> Unix.sleepf 0.05; State.Right
    | 'j' -> Unix.sleepf 0.05; State.Left
    | 'i' -> Unix.sleepf 0.05; State.RotateCW
    | 'c' -> Unix.sleepf 0.05; State.RotateCC
    | 'h' -> Unix.sleepf 0.05; State.Store 
    | 'k' -> Unix.sleepf 0.05; State.Drop
    | ' ' -> Unix.sleepf 0.05; State.HardDrop
    | _ ->
      Unix.sleepf !sleep_time; State.NoUserCommand 
  else begin
    Unix.sleepf !sleep_time; State.NoUserCommand
  end

(** [record_score st] appends to the score file the score earned in 
    state [st]. *)
let record_score st = 
  let oc = open_out_gen [Open_append] 0 "scores.txt" in 
  Printf.fprintf oc "%d %s \n" (State.get_score st) !user_name;
  close_out oc

(** [record_coins st] appends to the cions file the amount of money earned
    in state [st]. *)
let record_coins st = 
  let oc = open_out_gen [Open_append] 0 "coinbank.txt" in 
  Printf.fprintf oc "%d %s \n" (State.get_coins st) !user_name;
  close_out oc

(** [purchase_scheme schemenumber price] appends the scheme deonted by 
    [schemenumber] to the unlocked schemes file if the player has enough 
    money to unlock the scheme which is of price [price]  *)
let purchase_scheme (schemenumber : int) (price : int) = 
  let oc = open_out_gen [Open_append] 0 "unlocked_schemes.txt" in 
  Printf.fprintf oc "%d %s \n" (schemenumber) !user_name;
  close_out oc;
  let oc = open_out_gen [Open_append] 0 "coinbank.txt" in 
  Printf.fprintf oc "%d %s \n" (-1 * price) !user_name;
  close_out oc

let rec get_bank_balance lst (name:string) accu = 
  match lst with
  | [] -> accu
  | h :: t -> let entry = String.split_on_char ' ' h in 
    if List.nth entry 1 = name
    then get_bank_balance t name (accu + (entry |> List.hd |> int_of_string))
    else get_bank_balance t name accu 

(** [read_file filename] is a string list. It reads the lines 
    from file [filename]. *)
let read_file filename= 
  let lines = ref [] in 
  let channel = open_in filename in 
  try
    while true; do 
      lines := input_line channel :: !lines
    done; !lines
  with End_of_file -> 
    close_in channel;
    List.rev !lines 

(** [set_unlocked_schemes ()] assigns to [unlocked_schemes] all the schemes
    that have been unlocked by the user as stored in the unlocked 
    schemes file. *)
let set_unlocked_schemes () = 
  let lst = read_file "unlocked_schemes.txt" in
  let rec get_schemes lst (name:string) accu= 
    match lst with
    | [] -> accu
    | h :: t -> let entry = String.split_on_char ' ' h in 
      if List.nth entry 1 = name
      then get_schemes t name ((entry |> List.hd |> int_of_string) :: accu)
      else get_schemes t name accu in

  unlocked_schemes := List.sort (Stdlib.compare) (get_schemes lst !user_name [])

(**[is_unlocked sn] is true if [i] is one of the unclocked schemes and false
   otherwise.*)
let is_unlocked sn = 
  List.mem sn !unlocked_schemes

(** [compare line1 line2] returns the difference between the scores on
    [line1] and [line2]. *)
let compare line1 line2 =
  let line1score = String.split_on_char ' ' line1 |> List.hd |> int_of_string in 
  let line2score = String.split_on_char ' ' line2 |> List.hd |> int_of_string in
  -(line1score - line2score)

(** [get_first_ten lst acc counter] gets the first ten elements of [lst], 
    or all the elements if the size of [lst] is less than or equal to 10. *)
let rec get_first_ten lst acc counter = 
  match lst with
  | [] -> acc
  | h::t -> if counter = 10 then acc else get_first_ten t (h::acc) (counter + 1)

(** [get_top_ten lst] is the top ten scores/players stored in [lst]. *)
let get_top_ten lst = 
  List.rev (get_first_ten (List.sort (compare) lst) [] 0)

(** [move_down acc sc posx posy] draws all elements of [sc],
    each on a new line starting at [posx], [posy]. *)
let rec move_down acc sc posx posy = 
  match sc with
  |[] -> draw_string("");
  |h :: t -> begin moveto posx posy; draw_string ((string_of_int acc)^". "^h); 
      move_down (acc+1) t posx (posy - 20)
    end 

(** [top_ten dis sc acc] draws the top ten high scores after the game over
    screen. *)
let rec top_ten sc = 
  set_color black;
  move_down 1 sc ten_pos.x ten_pos.y

(** [copy_row row off] replaces the row at [row]-off with the row at [row]+1*)
let copy_row row off =
  draw_image (get_image 0 ((row+1)*30) 301 31) 0 ((row-off)*30)

(** [move_down curr_r last_r off] [curr_r] to [last_r] are shifted down 
    by offset *)
let rec move_down curr_r last_r off = 
  if (curr_r >= last_r-1) then begin
    copy_row curr_r off;
  end else begin
    copy_row curr_r off;
    move_down (curr_r+1) last_r off
  end

(** [clear_rows f lst curr_r last_r off] clears the rows
    on the screen between [f] and [lst].*)
let rec clear_rows f lst curr_r last_r off = 
  match lst with
  | [] -> ()
  | h::[] -> begin
      if (h <> curr_r && f) then begin
        clear_rows f lst (curr_r+1) last_r off
      end
      else if (h<>curr_r) then begin
        copy_row curr_r (off);
        clear_rows f lst (curr_r+1) last_r off
      end else begin
        move_down curr_r last_r (off);
        set_color black;
        fill_rect 0 ((last_r - off)*30) 300 600;
      end
    end
  | h::t ->
    if (h <> curr_r && f) then begin
      clear_rows f lst (curr_r+1) last_r off
    end else if (h<>curr_r) then begin
      copy_row curr_r (off);
      clear_rows f lst (curr_r+1) last_r off
    end
    else begin
      copy_row curr_r (off); 
      clear_rows false t (curr_r+1) last_r (off+1)
    end

(**[translate_pos t] is a scaled coordinate pair to fit Graphics board. *)
let translate_pos t = 
  (fst t * 30, snd t * 30)

(** [translate_pos_f t] is a scaled coordinate pair given floats. *)
let translate_pos_f t =
  (int_of_float (fst t *. 30.), int_of_float (snd t *. 30.))

(**[translate_lst lst] is a list of translated coordinate pairs from [lst]*)
let translate_lst = List.map translate_pos 

(**[translate_lst_f lst] is a list of translated coordinate pairs from [lst]*)
let translate_lst_f = List.map translate_pos_f

(**[draw_gold x y] draws gold coins to board at [x] [y] *)
let draw_gold x y = 
  let pos = translate_pos (x, y) in
  let center = ((fst pos) + 15), ((snd pos) + 15) in
  set_color (rgb 179 149 9);
  fill_circle (fst center) (snd center) 12;
  set_color (rgb 128 106 0);
  fill_circle (fst center) (snd center) 8;
  set_color (rgb 179 149 9);
  set_text_size 5;
  moveto (fst center-2) (snd center-5);
  draw_string "$"

(**[draw_silver x y] draws silver coins to board at [x] [y] *)
let draw_silver x y = 
  let pos = translate_pos (x, y) in
  let center = ((fst pos) + 15), ((snd pos) + 15) in
  set_color (rgb 230 242 255);
  fill_circle (fst center) (snd center) 12;
  set_color (rgb 179 191 255);
  fill_circle (fst center) (snd center) 8;
  set_color white;
  set_text_size 5;
  moveto (fst center-2) (snd center-5);
  draw_string "$"

(**[draw_bronze x y] draws bronze coins to board at [x] [y] *)
let draw_bronze x y = 
  let pos = translate_pos (x, y) in
  let center = ((fst pos) + 15), ((snd pos) + 15) in
  set_color (rgb 204 102 0);
  fill_circle (fst center) (snd center) 12;
  set_color (rgb 153 77 0);
  fill_circle (fst center) (snd center) 8;
  set_color (rgb 204 102 0);
  set_text_size 5;
  moveto (fst center-2) (snd center-5);
  draw_string "$"

(** [update_hold st] updates the held piece in [st] in the game's display. *)
let update_hold st = 
  set_color (rgb 0 34 51);
  fill_rect hold_pos.x hold_pos.y 120 120;
  let hold_piece_coords = State.get_hold_piece st in
  let hold_piece = State.get_hold_piece_name st in
  let new_coords = 
    match hold_piece with
    | Pieces.I -> List.map 
                    (fun (x, y) -> ((float_of_int x)+.10.5, 
                                    (float_of_int y)+.17.5)) hold_piece_coords
    | Pieces.L 
    | Pieces.J -> List.map (fun (x, y) -> 
        ((float_of_int x)+.11., (float_of_int y)+.17.)) hold_piece_coords
    | Pieces.O -> List.map (fun (x, y) ->
        ((float_of_int x)+.11.5, (float_of_int y)+.17.)) hold_piece_coords
    | _ -> List.map (fun (x, y) -> 
        ((float_of_int x)+.11., (float_of_int y)+.17.)) hold_piece_coords in
  fill_spaces (State.get_hold_piece_color st) (translate_lst_f new_coords);
  fill_spaces black (translate_lst (State.get_piece_pos st))

(** [update_queue st] updates the queued piece in [st] in the game's visuals. *)
let update_queue st = 
  set_color (rgb 0 34 51);
  fill_rect queue_pos.x queue_pos.y 120 120;
  let queue_piece_coords = State.get_queue_piece st in
  let queue_piece = (fst (State.get_next_piece_info st)).name in
  let new_coords = 
    match queue_piece with
    | Pieces.I -> List.map (fun (x, y) -> 
        ((float_of_int x)+.10.5, (float_of_int y)+.1.5)) queue_piece_coords
    | Pieces.L 
    | Pieces.J -> List.map (fun (x, y) -> 
        ((float_of_int x)+.11., (float_of_int y)+.1.)) queue_piece_coords
    | Pieces.O -> List.map (fun (x, y) -> 
        ((float_of_int x)+.11.5, (float_of_int y)+.1.)) queue_piece_coords
    | _ -> List.map (fun (x, y) -> 
        ((float_of_int x)+.11., (float_of_int y)+.1.)) queue_piece_coords in
  fill_spaces (fst (State.get_next_piece_info st)).color 
    (translate_lst_f new_coords)

(** [update_score sc] is a unit. Modifies the score to display the score of
    int [sc]. *)
let update_score sc lvl = 
  set_color white;
  fill_rect 301 250 120 100;
  set_color blue;
  moveto score_pos.x score_pos.y;
  set_text_size score_pos.t_size;
  draw_string "Score: ";
  draw_string sc;
  moveto level_pos.x level_pos.y;
  set_text_size level_pos.t_size;
  draw_string "Level: ";
  draw_string lvl

(** [update_coins c] is a unit. Modifies the score to display the coins of
    int [c]. *)
let update_coins c = 
  set_color white;
  fill_rect 301 240 120 10;
  set_color blue;
  moveto coins_pos.x coins_pos.y;
  set_text_size score_pos.t_size;
  draw_string "Coins: ";
  draw_string c

(** [copy_up row off] is a unit. Visually copies the row below [row] and pastes
    it above [row] by an offset of [off]. *)
let copy_up row off = 
  draw_image (get_image (0) (row*30) 301 31) 0 ((row+off)*30)

(** [update_shorten curr_r last_r off] is a unit. Visually shortens the board
    by adding [off] number of gray rows and moving all other rows from 
    [last_r] to [curr_r] up by [off]. *)
let update_shorten curr_r last_r off = 
  if off=0 then () 
  else begin
    let rec recurs curr_r last_r off = begin
      if (curr_r = last_r) then begin
        copy_up curr_r off
      end else begin
        copy_up (curr_r) off;
        recurs (curr_r - 1) last_r off
      end
    end in
    recurs curr_r (last_r) off;
    set_color (rgb 96 96 96);
    fill_rect 0 ((!start_row)*30) 300 30;
    start_row := !start_row + 1
  end

(** [erase_prevs st] erases the the previous state of [st]'s visuals that 
    should be erased. *)
let erase_prevs st = 
  let prev_ghost = translate_lst (State.get_drop_coords_p st) in
  fill_spaces black prev_ghost;
  let prev_coin_pos = State.coin_pos st false in
  fill_spaces black (translate_lst (List.map 
                                      (fun (x,y,z) -> (x,y)) prev_coin_pos));
  let erase_pos = translate_lst (State.coords_to_erase st) in
  fill_spaces black erase_pos;
  let lines_to_clear = State.get_clear_lines st in
  begin
    if lines_to_clear = [] then ()
    else clear_rows true lines_to_clear 0 (State.get_colored_line st) 0
  end

(** [draw_coins positions] draws corresponding coins from [positions]. *)
let rec draw_coins positions = 
  match positions with 
  | [] -> ()
  | (x, y, v)::t -> begin
      if v=10 then (draw_bronze x y; draw_coins t)
      else if v=25 then (draw_silver x y; draw_coins t)
      else draw_gold x y; draw_coins t
    end

(** [set_pos st] draws onto visuals the things that are now colored compared
    to the previous state of [st]. *)
let set_pos st = 
  let set_pos = translate_lst (State.get_piece_pos st) in
  fill_spaces (State.get_piece_color st) set_pos;
  let coin_pos = State.coin_pos st true in
  draw_coins coin_pos

(** [display_st curr_st] updates the board, held piece, queue, score, and level
    in the game's visuals. *)
let display_st curr_st = 
  sleep_time := State.get_sleep_time curr_st;
  erase_prevs curr_st;
  update_hold curr_st;
  update_queue curr_st;
  if (State.get_gray_blocks curr_st <> !start_row) then
    update_shorten (State.get_colored_line curr_st) (!start_row) 1 else ();
  let ghost = translate_lst (State.get_drop_coords curr_st) in
  fill_spaces (Graphics.rgb 255 230 234) ghost;
  update_score (string_of_int (State.get_score curr_st)) 
    (string_of_int (State.get_level curr_st));
  update_coins (string_of_int (State.get_coins curr_st));
  set_pos curr_st;
  let command = check_key () in
  State.update command curr_st

(** [drawing_welcome_message ()] draws the buttons of the welcome screen. *)
let drawing_welcome_message () = 
  fill_rect (key_prompt_pos.x) (key_prompt_pos.y) 100 50;
  fill_rect (key_prompt_pos.x) (key_prompt_pos.y - 100) 100 50;
  fill_rect (key_prompt_pos.x) (key_prompt_pos.y - 200) 100 50;
  set_color white;
  moveto (key_prompt_pos.x + 35) (key_prompt_pos.y + 20);
  draw_string "START";
  moveto (key_prompt_pos.x + 10) (key_prompt_pos.y + 20 - 100);
  draw_string "COLOR SCHEMES";
  moveto (key_prompt_pos.x + 37) (key_prompt_pos.y + 20 - 200);
  draw_string "SCORES";
  sleep_time := 0.4;
  start_row := 0

let welcome_message usr = 
  clear_graph ();
  user_name := usr;
  set_unlocked_schemes ();
  set_color (rgb 1 143 151);
  moveto welcome_pos.x welcome_pos.y;
  set_text_size welcome_pos.t_size;
  draw_string "Welcome to TETRIS ";
  draw_string usr;
  drawing_welcome_message ()


(** [create_board ()] is a unit. Draws the initial game board with a black
    playing area and a score indicator. *)
let create_board () = 
  clear_graph ();
  set_color black;
  fill_rect 0 0 300 600;
  moveto score_pos.x score_pos.y;
  set_color blue;
  set_text_size score_pos.t_size;
  draw_string "SCORE: 0";
  moveto coins_pos.x coins_pos.y;
  draw_string "COINS: 0";
  set_color (rgb 0 34 51);
  fill_rect hold_pos.x hold_pos.y 120 120;
  fill_rect queue_pos.x queue_pos.y 120 120;
  set_color (rgb 204 136 0);
  set_text_size 200;
  moveto (hold_pos.x+25 ) (hold_pos.y -20);
  draw_string "Stored Piece";
  moveto (queue_pos.x + 20) (queue_pos.y + 140);
  draw_string "Next Piece"

let get_scheme () = 
  !scheme

(** [set_colors unlock lst acc] makes a list of colors that the buttons
    of the scheme select screen should be. *)
let rec set_colors unlock lst acc = 
  if acc = 13 then lst
  else begin
    match unlock with 
    | [] -> set_colors [] (black::lst) (acc+1)
    | h::t -> begin
        if h=acc then set_colors t ((rgb 236 62 75)::lst) (acc+1)
        else set_colors unlock (black::lst) (acc+1)
      end
  end 

(**[color_lock ()] is a list of colors that correspond to properties 
   of a scheme*)
let color_lock () = 
  List.rev (set_colors !unlocked_schemes [rgb 236 62 75] 1)

let coords_scheme = [(200, 500); (50, 400); (200, 400); (350, 400); (50, 300); 
                     (200, 300); (350, 300); (50, 200); (200, 200); 
                     (350, 200); (50, 100); (200, 100); (350, 100)]

let color_schemes = ["Default"; "The Vivian"; "The Cathy"; "The Rachel"; 
                     "Cornell CIS"; "GO BIG RED!"; "Cornell Winter"; 
                     "Cornell Spring"; "cornell Fall"; "Cornell Summer";
                     "Dunkin'"; "Starbucks"; "CTB"]

(**[create_buttons ()] is buttons for each scheme.*)
let create_buttons () = 
  clear_graph ();
  let colors_to_set = color_lock () in
  let rec helper coords colors names = 
    match coords, colors, names with
    | [], [], []-> ()
    | p::ps, c::cs, n::ns -> begin
        set_color c;
        fill_rect (fst p) (snd p) 100 50;
        moveto (fst p + 10) (snd p + 15);
        set_color white;
        if c = black then draw_string "Click to Unlock" else draw_string n;
        helper ps cs ns
      end
    | _ -> () in
  helper coords_scheme colors_to_set color_schemes

(**[display_score ()] displays the scores on the board *)
let rec display_score () = 
  clear_graph ();
  score_sc := get_top_ten (read_file "scores.txt"); top_ten !score_sc;
  set_color (rgb 110 192 183);
  fill_rect (key_prompt_pos.x) (key_prompt_pos.y - 100) 100 50;
  set_color white;
  moveto (key_prompt_pos.x + 40) (key_prompt_pos.y + 20 - 100);
  draw_string "BACK";
  back_start ()

(**[back_start ()] returns to welcome message *)
and back_start () = 
  let key_pressed = wait_next_event [Button_down] in 
  if (key_pressed.mouse_x <= 300 && key_pressed.mouse_x >= 200 &&
      key_pressed.mouse_y <= 200 && key_pressed.mouse_y >= 150) then begin
    welcome_message !user_name;
    start ()
  end
  else
    back_start ()

(**[back_start ()] returns to scheme screen*)
and back_scheme () = 
  let key_pressed = wait_next_event [Button_down] in 
  if (key_pressed.mouse_x <= 300 && key_pressed.mouse_x >= 200 &&
      key_pressed.mouse_y <= 200 && key_pressed.mouse_y >= 150) then begin
    create_buttons();
    scheme_screen ()
  end
  else
    back_scheme ()

(** [insuf_coins_h ()] is a helper to display the template of the screen
    that displays when a player has insufficient funds to unlock a scheme. *)
and insuf_coins_h () = 
  moveto welcome_pos.x welcome_pos.y;
  set_text_size welcome_pos.t_size;
  set_color red;
  draw_string "Insufficient Balance";
  moveto welcome_pos.x (welcome_pos.y - 30);
  draw_string "Current balance: "

(**[insuf_coins ()] is a screen displayed for insufficient coins *)
and insuf_coins ind = 
  clear_graph ();
  insuf_coins_h ();
  draw_string (string_of_int (get_bank_balance 
                                (read_file "coinbank.txt") !user_name 0));
  moveto welcome_pos.x (welcome_pos.y - 45);
  draw_string "Need additional ";
  let remaining_balance = (scheme_values.(ind) - get_bank_balance 
                             (read_file "coinbank.txt") !user_name 0) in
  draw_string (string_of_int remaining_balance);
  set_color (rgb 110 192 183);
  fill_rect (key_prompt_pos.x) (key_prompt_pos.y - 100) 100 50;
  set_color white;
  moveto (key_prompt_pos.x + 40) (key_prompt_pos.y + 20 - 100);
  draw_string "BACK";
  back_scheme ();

and scheme_helper sch = 
  if (is_unlocked sch) then 
    scheme := sch
  else begin
    if ((get_bank_balance 
           (read_file "coinbank.txt") !user_name 0) >= scheme_values.(sch)) 
    then begin
      purchase_scheme sch scheme_values.(sch);
      unlocked_schemes := List.sort (Stdlib.compare) (sch::(!unlocked_schemes));
      create_buttons();
      scheme_screen () end
    else begin
      insuf_coins sch
    end
  end

(**[scheme_screen ()] is a screen that displays buttons for each scheme *)
and scheme_screen () = 
  let key_pressed = wait_next_event [Button_down] in
  if (key_pressed.mouse_x <= 150 && key_pressed.mouse_x >= 50 
      && key_pressed.mouse_y <= 450 && key_pressed.mouse_y >= 400) then 
    (* access file to see if scheme is unlocked *)
    scheme_helper 1
  else if (key_pressed.mouse_x <= 300 && key_pressed.mouse_x >= 200
           && key_pressed.mouse_y <= 450 && key_pressed.mouse_y >= 400) then
    scheme_helper 2
  else if (key_pressed.mouse_x <= 450 && key_pressed.mouse_x >= 350
           && key_pressed.mouse_y <= 450 && key_pressed.mouse_y >= 400) then
    scheme_helper 3
  else if (key_pressed.mouse_x <= 150 && key_pressed.mouse_x >= 50 
           && key_pressed.mouse_y <= 350 && key_pressed.mouse_y >= 300) then 
    scheme_helper 4
  else if (key_pressed.mouse_x <= 300 && key_pressed.mouse_x >= 200
           && key_pressed.mouse_y <= 350 && key_pressed.mouse_y >= 300) then
    scheme_helper 5
  else if (key_pressed.mouse_x <= 450 && key_pressed.mouse_x >= 350
           && key_pressed.mouse_y <= 350 && key_pressed.mouse_y >= 300) then
    scheme_helper 6
  else if (key_pressed.mouse_x <= 150 && key_pressed.mouse_x >= 50 
           && key_pressed.mouse_y <= 250 && key_pressed.mouse_y >= 200) then 
    scheme_helper 7
  else if (key_pressed.mouse_x <= 300 && key_pressed.mouse_x >= 200
           && key_pressed.mouse_y <= 250 && key_pressed.mouse_y >= 200) then
    scheme_helper 8
  else if (key_pressed.mouse_x <= 450 && key_pressed.mouse_x >= 350
           && key_pressed.mouse_y <= 250 && key_pressed.mouse_y >= 200) then
    scheme_helper 9
  else if (key_pressed.mouse_x <= 150 && key_pressed.mouse_x >= 50 
           && key_pressed.mouse_y <= 150 && key_pressed.mouse_y >= 100) then 
    scheme_helper 10
  else if (key_pressed.mouse_x <= 300 && key_pressed.mouse_x >= 200
           && key_pressed.mouse_y <= 150 && key_pressed.mouse_y >= 100) then
    scheme_helper 11
  else if (key_pressed.mouse_x <= 450 && key_pressed.mouse_x >= 350
           && key_pressed.mouse_y <= 150 && key_pressed.mouse_y >= 100) then
    scheme_helper 12
  else if (key_pressed.mouse_x <= 300 && key_pressed.mouse_x >= 200
           && key_pressed.mouse_y <= 550 && key_pressed.mouse_y >= 500) then
    scheme := 0
  else scheme_screen ()

(**[start ()] is the start screen *)
and start () = 
  let key_pressed = wait_next_event [Button_down] in 
  if (key_pressed.mouse_x <= 300 && key_pressed.mouse_x >= 200 && 
      key_pressed.mouse_y <= 300 && key_pressed.mouse_y >= 250) then
    create_board ()
  else if (key_pressed.mouse_x <= 300 && key_pressed.mouse_x >= 200 && 
           key_pressed.mouse_y <= 200 && key_pressed.mouse_y >= 150) then 
    begin
      clear_graph ();
      create_buttons ();
      scheme_screen ();
      Pieces.change_color_scheme !scheme;
      welcome_message !user_name;
      start ()
    end
  else if (key_pressed.mouse_x <= 300 && key_pressed.mouse_x >= 200 && 
           key_pressed.mouse_y <= 100 && key_pressed.mouse_y >= 50) then
    begin display_score () end
  else
    start ()

(** [get_restart ()] is true if the user has selected to restart the game. 
    Otherwise, is false as the user has elected to quit. *)
let rec get_restart () = 
  let key_pressed = wait_next_event [Button_down] in 
  if (key_pressed.mouse_x <= 300 && key_pressed.mouse_x >= 200 && 
      key_pressed.mouse_y <= 300 && key_pressed.mouse_y >= 250) then
    true
  else if (key_pressed.mouse_x <= 300 && key_pressed.mouse_x >= 200 &&
           key_pressed.mouse_y <= 200 && key_pressed.mouse_y >= 150) then
    false else get_restart ()

let rec game_over is_dis =
  set_text_size game_over_pos.t_size;
  moveto game_over_pos.x game_over_pos.y;
  set_color red;
  draw_string "GAME OVER";
  set_color (rgb 110 192 183);
  fill_rect (key_prompt_pos.x) (key_prompt_pos.y) 100 50;
  fill_rect (key_prompt_pos.x) (key_prompt_pos.y - 100) 100 50;
  set_color white;
  moveto (key_prompt_pos.x + 35) (key_prompt_pos.y + 20);
  draw_string "RESTART";
  moveto (key_prompt_pos.x + 40) (key_prompt_pos.y + 20 - 100);
  draw_string "QUIT";
  get_restart ()
