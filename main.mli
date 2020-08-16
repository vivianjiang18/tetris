(** 
   Representation of the main tetris file.

   This module allows tetris to be played, and loops through gameplay. 
*)

(** [main ()] is a unit. Displays a welcome message on the terminal and 
    prompts the user for a user name. *)
val main : unit -> unit

(** [new_game ()] is a unit. Initiates a new state, starts, visuals, and calls
    play. *)
val new_game : unit -> unit

(** [play st] plays the game from state [st] *)
val play : State.t -> unit