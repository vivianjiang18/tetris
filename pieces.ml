type coordinates = (int*int) list 

(** The type representing the coordinate/tuple of the center of a piece. *)
type center = (int*int)

type name = I | L | J | O | S | T | Z | E

type piece = {name: name; mutable color : Graphics.color; shape : coordinates}

type piece_record = 
  {name: name; mutable color : Graphics.color; shape_init : coordinates}

let color_default = [| Graphics.rgb 255 77 77; Graphics.rgb 255 187 51; 
                       Graphics.rgb 255 255 102; Graphics.rgb 85 128 0; 
                       Graphics.rgb 102 127 255; Graphics.rgb 170 0 255; 
                       Graphics.rgb 255 153 238|]

let color_vivian = [|Graphics.rgb 33 145 251; Graphics.rgb 186 39 74; 
                     Graphics.rgb 132 28 38; Graphics.rgb 178 236 225; 
                     Graphics.rgb 140 222 220; Graphics.rgb 234 99 140; 
                     Graphics.rgb 191 154 202|]

let color_cathy = [|Graphics.rgb 132 153 177; Graphics.rgb 165 196 212; 
                    Graphics.rgb 123 109 141; Graphics.rgb 221 251 210; 
                    Graphics.rgb 89 63 98; Graphics.rgb 139 104 116; 
                    Graphics.rgb 142 65 98|]

let color_rachel = [|Graphics.rgb 20 70 160; Graphics.rgb 60 60 59; 
                     Graphics.rgb 219 48 105; Graphics.rgb 221 251 210; 
                     Graphics.rgb 245 213 71; Graphics.rgb 140 222 220; 
                     Graphics.rgb 235 235 211|]

let color_cis = [|Graphics.rgb 255 166 11; Graphics.rgb 25 209 246; 
                  Graphics.rgb 153 153 153; Graphics.rgb 255 255 255; 
                  Graphics.rgb 25 209 246; Graphics.rgb 255 166 11; 
                  Graphics.rgb 255 255 255|]

let color_go_big_red = [|Graphics.rgb 255 0 0; Graphics.rgb 255 0 0; 
                         Graphics.rgb 255 0 0; Graphics.rgb 255 0 0; 
                         Graphics.rgb 255 255 255; Graphics.rgb 255 255 255; 
                         Graphics.rgb 255 255 255|]

let color_cornell_winter = [|Graphics.rgb 220 220 220; Graphics.rgb 112 128 144; 
                             Graphics.rgb 105 105 105; Graphics.rgb 255 255 255; 
                             Graphics.rgb 169 169 169; Graphics.rgb 192 192 192; 
                             Graphics.rgb 47 79 79|]

let color_cornell_fall = [|Graphics.rgb 190 7 1; Graphics.rgb 174 140 83; 
                           Graphics.rgb 227 192 100; Graphics.rgb 197 131 42; 
                           Graphics.rgb 120 0 1; Graphics.rgb 109 113 1; 
                           Graphics.rgb 212 95 5|]

let color_cornell_summer = [|Graphics.rgb 50 208 129; Graphics.rgb 204 239 171; 
                             Graphics.rgb 246 104 103; Graphics.rgb 247 221 125; 
                             Graphics.rgb 91 197 234; Graphics.rgb 61 147 221; 
                             Graphics.rgb 124 151 2|]

let color_cornell_spring = [|Graphics.rgb 255 64 76; Graphics.rgb 255 204 51; 
                             Graphics.rgb 242 241 122; Graphics.rgb 251 160 227; 
                             Graphics.rgb 150 198 92; Graphics.rgb 71 170 54;
                             Graphics.rgb 156 209 255|]

let color_dunkin = [|Graphics.rgb 245 130 31; Graphics.rgb 245 130 31; 
                     Graphics.rgb 255 19 131; Graphics.rgb 255 255 255; 
                     Graphics.rgb 255 19 131; Graphics.rgb 103 55 22; 
                     Graphics.rgb 103 55 22|]

let color_starbucks = [|Graphics.rgb 0 98 65; Graphics.rgb 0 98 65; 
                        Graphics.rgb 255 255 255; Graphics.rgb 174 169 133; 
                        Graphics.rgb 114 93 84; Graphics.rgb 82 54 41; 
                        Graphics.rgb 238 236 224|]

let color_ctb = [|Graphics.rgb 255 255 255; Graphics.rgb 61 90 22; 
                  Graphics.rgb 254 210 0; Graphics.rgb 231 212 171; 
                  Graphics.rgb 189 139 101; Graphics.rgb 210 180 141; 
                  Graphics.rgb 169 110 41|]

(** [all_pieces] is an array containing the default values for all pieces.
    Use array (even though it's mutable) for O(1) access time given an index *)
let all_pieces = [|{name = I; color = Graphics.rgb 255 77 77; 
                    shape_init = [(0,0); (1,0); (2,0); (3,0)]}; 
                   {name = L; color = Graphics.rgb 255 187 51; 
                    shape_init = [(0,0); (0,1); (1,0); (2,0)]}; 
                   {name = J; color = Graphics.rgb 255 255 102; 
                    shape_init = [(0,0); (1,0); (2,0); (2,1)]};
                   {name = O; color = Graphics.rgb 85 128 0; 
                    shape_init = [(0,0); (1,0); (0,1); (1,1)]};
                   {name = S; color = Graphics.rgb 102 127 255; 
                    shape_init = [(0,0); (1,0); (1,1); (2,1)]};
                   {name = T; color = Graphics.rgb 170 0 255; 
                    shape_init = [(0,0); (1,0); (2,0); (1,1)]};
                   {name = Z; color = Graphics.rgb 255 153 238; 
                    shape_init = [(0,1); (1,1); (1,0); (2,0)]}|]

let get_shape_init n = 
  match n with
  | I -> all_pieces.(0).shape_init
  | L -> all_pieces.(1).shape_init
  | J -> all_pieces.(2).shape_init
  | O -> all_pieces.(3).shape_init
  | S -> all_pieces.(4).shape_init
  | T -> all_pieces.(5).shape_init
  | Z -> all_pieces.(6).shape_init
  | _ -> [(0, 0)]

let construct_piece idx = 
  let piece_type = all_pieces.(idx) in
  {name = piece_type.name; 
   color = piece_type.color; 
   shape = piece_type.shape_init}

let empty_piece () = 
  {name = E; color =  Graphics.(rgb 0 34 51); shape = []}

let get_len = function
  | I -> 4
  | L -> 3
  | J -> 3
  | O -> 2
  | S -> 3
  | T -> 3
  | Z -> 3
  | E -> failwith "No length"

(** [rotatelistcc lst acc] is the updated coordinate list after a 
    counter-clockwise rotation.  *)
let rotatelistcc = 
  List.fold_left (fun acc (a, b) -> (-1 * b, a)::acc) []

(** [rotatelistcc lst acc] is the updated coordinate list after a clockwise
    rotation.  *)
let rotatelistcw = 
  List.fold_left (fun acc (a, b) -> (b, -1 * a) :: acc) []

(** [find_min_x lst] is the minimum x-coordinate value in [lst]. *)
let find_min_x lst = 
  match lst with
  | [] -> 0
  | h::t ->  List.fold_left (fun acc x -> if (fst x)<acc then (fst x) else acc) 
               (fst h) lst

(** [find_max_x lst] is the maximum x-coordinate value in [lst]. *)
let find_max_x lst = 
  match lst with
  | [] -> 0
  | h :: t -> List.fold_left (fun acc x -> if (fst x)>acc then (fst x) else acc) 
                (fst h) lst

(**[valid_x lst coords] is a valid coordinate *)
let valid_x lst coord_lst = 
  if List.filter (fun x -> fst x > 9) coord_lst = [] then 0 
  else find_max_x lst - 9

(** [find_min_y lst] is the minimum y-coordinate value in [lst]. *)
let find_min_y lst = 
  match lst with
  | [] -> 0
  | h::t ->  List.fold_left (fun acc x -> if (snd x)<acc then (snd x) else acc)
               (snd h) lst

(** [make_fit lst] is a list of coordinates that translates all values in [lst]
    such that all values are non-negative. *)
let make_fit lst = 
  let min_x = find_min_x lst in
  let min_y = find_min_y lst in
  let x_pos = if (min_x < 0) 
    then List.map (fun x -> ((fst x) - min_x), snd x) lst 
    else lst in
  if min_y < 0 then List.map (fun x -> (fst x, (snd x) - min_y)) x_pos 
  else x_pos

(**[adj_right lst coords] adjusts coordinates so piece remains on board*)
let adj_right lst coord_lst= 
  let max_x = find_max_x coord_lst - 9 in
  List.map (fun x -> ((fst x) - max_x), snd x) lst 

(**[translate_to_new_coords] translates coordinates from [lst] by [pos]*)
let translate_to_new_coords lst pos= 
  List.map (fun x -> (fst x + fst pos, snd x + snd pos) ) lst

(**[val_right_bound lst pos] is true if no coordinates from [lst] are out of 
   bounds*)
let val_right_bound lst pos = 
  List.filter (fun x -> fst x + fst pos > 9) lst = []

(** [rotate pz is_cc] is a rotated piece. If is_cc is true, rotated 
    counter-clockwise. Otherwise, rotated clockwise.*)
let rotate pz is_cc pos = 
  let newcoords = 
    if is_cc then 
      let fit_left = make_fit (rotatelistcc pz.shape) in
      if (val_right_bound fit_left pos) then fit_left else
        adj_right fit_left (translate_to_new_coords fit_left pos)
    else
      let fit_left = make_fit (rotatelistcw pz.shape) in
      if (val_right_bound fit_left pos) then fit_left else
        adj_right fit_left (translate_to_new_coords fit_left pos) in
  {name = pz.name; color = pz.color; shape = newcoords}

(**[change_color_helper ary] changes all_pieces.(i).color to [ary].(i) *)
let change_color_helper ary = 
  if (Array.length ary == 0 || Array.length ary > 7) then () else
    let rec loop i = 
      if i > 6 then ()
      else begin all_pieces.(i).color <- ary.(i); loop (i+1) end in
    loop 0

let change_color_scheme i = 
  match i with
  | 0 -> change_color_helper color_default
  | 1 -> change_color_helper color_vivian
  | 2 -> change_color_helper color_cathy
  | 3 -> change_color_helper color_rachel
  | 4 -> change_color_helper color_cis
  | 5 -> change_color_helper color_go_big_red
  | 6 -> change_color_helper color_cornell_winter
  | 7 -> change_color_helper color_cornell_spring
  | 8 -> change_color_helper color_cornell_fall
  | 9 -> change_color_helper color_cornell_summer
  | 10 -> change_color_helper color_dunkin
  | 11 -> change_color_helper color_starbucks
  | 12 -> change_color_helper color_ctb
  | _ -> change_color_helper [||]
