(** [user_name] is a reference to the string representing the user name *)
let user_name = ref ""

let main () = 
  ANSITerminal.(print_string [red] "Welcome to ");
  ANSITerminal.(print_string [yellow] "TETRIS ");
  ANSITerminal.(print_string [red] "! \n");
  ANSITerminal.(print_string [green] "Enter your Player name > ");
  user_name := read_line ()

let rec new_game () =
  Random.init (int_of_float (Unix.time()));
  Visuals.welcome_message !user_name;
  Visuals.start (); (*waits for spacebar input *)
  play (State.init_state 10 24) (* default tetris board is 10x20 *)

and play st = 
  if (not (State.is_game_over st 0)) then 
    play (Visuals.display_st st) 
  else
    begin
      Graphics.clear_graph ();
      if Visuals.game_over () then begin Visuals.record_coins st; 
        Visuals.record_score st; Graphics.clear_graph (); new_game() end
      else Visuals.record_coins st; Visuals.record_score st; ()
    end

(** Executes the game engine *)
let _ = 
  main ();
  Graphics.open_graph " 500x600";
  new_game ()