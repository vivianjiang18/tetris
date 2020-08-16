(** 
   Module with different calls to translate the 
   tetris game state to a visual representation.

   This module includes functionalities for different stages
   of the game (start screen, end screen) and visually updating
   states.
*)

(** [game_over is_dis] is a unit. Displays a flashing "game over"
    screen. Text is displayed when [is_dis] is true and is not
    displayed when [is_dis] is false. Exits when any key is pressed
    by the user. *)
val game_over : unit -> bool

(** [welcom_message usr] is a unit. Displays a welcoming message
    addressing the user by [usr] and prompts the user to press 
    "space" to start playing. *)
val welcome_message : string -> unit

(** [start ()] is a unit. Waiting state that waits for user input
    of "space" to start playing the game. *)
val start : unit -> unit

(** [display_st curr_st] is a new state after updating. Displays
    the current state [curr_st]. *)
val display_st : State.t -> State.t

(**[get_scheme ()] is the current scheme *)
val get_scheme : unit -> int

(** [record_score st] writes the score stored in [st] of the finished game 
    to a file *)
val record_score : State.t -> unit

(** [record_coins st] writes the coins value stored in [st] of the finished game 
    to a file *)
val record_coins : State.t -> unit

(**[get_bank_balance lst str accu] is the bank balance of a specific user 
   signaled by [name]. All account balances for all users found in [lst] *)
val get_bank_balance : string list -> string -> int -> int 