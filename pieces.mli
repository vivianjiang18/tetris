(** 
   Representation of the pieces of Tetris game.

   This module represents the different peices of tetris and the various
   functions applied to them. 
*)

(** The list of int tuples that represent the coordinates on the board *)
type coordinates = (int*int) list 

(** The different names of the tetris pieces *)
type name = I | L | J | O | S | T | Z | E

(** The type representing the different information of a tetris piece. *)
type piece = {name: name; mutable color : Graphics.color; shape : coordinates}

(** The type representing default piece values *)
type piece_record

(** [rotate piece is_cc] is [piece] with rotated shape. If [is_cc] is true, 
    then shape is rotated counter-clockwise. Else, shape is rotated clockwise.
*)
val rotate : piece -> bool -> (int * int) -> piece

(** [get_len name] is the width-wise length (span of x-coordinates) of the 
    piece of name [name] in its default settings. *)
val get_len : name -> int

(** [construct_piece idx] is a tetris piece chosen using int [idx]. *)
val construct_piece : int -> piece

(**[empty_piece ()] is an empty piece *)
val empty_piece : unit -> piece

(**[get_shape_init n] is the initial coordinate list of shape [n] *)
val get_shape_init : name -> (int * int) list

(** [change_color_scheme sch] changes the color scheme of the pieces to the
    the color scheme indicated by [sch]. *)
val change_color_scheme : int -> unit