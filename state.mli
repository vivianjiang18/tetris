(**
   Representation of dyanamic tetris board state.

   This module represents the state of the tetris game that is being played
   such as the current piece that's dropping, the current board height and
   width, the level, the score, the previous piece position, and the previous
   piece coordinates.
*)

(** The abstract type that represents the state of a dynamic tetric game.
    It includes things like the current falling piece, board dimensions,
    score, level, and coordinates. *)
type t

(** The type representing the command that can be given by the user *)
type command = Left | Right | RotateCW | RotateCC 
             | Drop | NoUserCommand | Store | HardDrop

(** [init_state bw bh] is the initial game state for a board with width [bw]
    and height [bh]*)
val init_state : int -> int -> t

(** Takes in Command.t as first argument*)
(** [update command st] is the updated state after applying
    [command] to [st]. *)
val update : command -> t -> t

(** [get_position state] is the current position of the falling piece
    on the board in [state]. This point is based off its anchor point *)
val get_position : t -> (int * int)

(** [is_game_over st] is true if [st] is a game over state. False
    otherwise. *)
val is_game_over: t -> int -> bool

(** [get_piece_pos st] is a list of coordinates of where the falling
    piece in state [st] is occupying. *)
val get_piece_pos : t -> (int*int) list

(**[get_coing st] is amount of coins collected in [st] *)
val get_coins : t -> int

(**[coin_pos st b] if [b] then is a list of triples (x, y, value*)
val coin_pos : t -> bool -> (int * int * int) list

(**[get_prev_command st] is the previous command in [st] *)
val get_prev_command : t -> command 

(** [get_piece_color st] is the color of the current falling piece in
    state [st]. *)
val get_piece_color : t -> Graphics.color

(** [coords_to_erase st] is a list of coordinates of where the piece was
    previously but no longer occupies so those coordinates must be erased
    of state [st]. *)
val coords_to_erase : t -> (int*int) list

(** [get_score st] is the score of state [st] *)
val get_score : t -> int

(**[get_rows_to_clear st] is an int list of the row numbers to clear. 
   Should be called before rows are cleared in the backend board. 
   (i.e. before the commands are executed. If this won't work, we can easily store 
   the previous copy of the board as an element of state.) *)
val get_rows_to_clear : t -> int list

(**[get_clear_lines st] is the list of rows to (or not to?) be cleared *)
val get_clear_lines : t -> int list

(**[get_colored_line st] is the row number of the last row with a placed piece*)
val get_colored_line : t -> int

(**[get_piece_info st] is the piece info *)
val get_piece_info : t -> (Pieces.piece * (int * int))

(**[get_next_piece_info] is the next piece and its starting coordinate *)
val get_next_piece_info : t -> (Pieces.piece * (int * int))

(**[get_sleep_time st] is the sleep time based on the current level in [st] *)
val get_sleep_time : t -> float

(**[get_level st] is the current level *)
val get_level : t -> int 

(** [get_stored_piece state] is the piece currently stored by player in [state]. *)
val get_stored_piece : t -> Pieces.piece

(** [get_hold_piece state] is the coordinates of the piece currently stored by
    player in [state]. *)
val get_hold_piece : t -> (int * int) list

(** [get_hold_piece_color state] is the color of the piece currently stored by
    player in [state]. *)
val get_hold_piece_color : t -> Graphics.color

(** [get_hold_piece_name st] is the name of the piece currently stored by
    player in [st]. *)
val get_hold_piece_name : t -> Pieces.name

(** [get_queue_piece state] is the list of coordinates of the piece queued
    to fall next in [state]. *)
val get_queue_piece : t -> (int * int) list

(** [get_gray_blocks st] is the number of rows of gray blocks at the bottom
    of the board in [st]. *)
val get_gray_blocks : t -> int 

(**[get_drop_coords st] is coordinates of ghost*)
val get_drop_coords : t -> (int * int) list

(**[get_drop_coords_p st] is the coordinates to erase from the previous ghost
   postion in [st] *)
val get_drop_coords_p : t -> (int * int) list