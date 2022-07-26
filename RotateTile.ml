#use "Board.ml";;

(*
   rotated tiles
*)

(* type 'a rot_tile = 'a * int *)

let rotate s =
    match s with
    | UP -> RIGHT
    | RIGHT -> DOWN
    | DOWN -> LEFT
    | LEFT -> UP

let rec get_rot_side get_side (t,r) s =
    match r with
    | 0 -> get_side t s
    | _ -> get_rot_side get_side (t,(r+1) mod 4) (rotate s)

let rec render_rot_tile render (t,r) =
    match r with 
    | 0 -> render t
    | _ ->
        let s = render_rot_tile render (t,r-1) in
        rotate_board s