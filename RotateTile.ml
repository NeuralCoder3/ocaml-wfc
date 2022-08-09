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
    | _ -> raise (Domain "Only 2d is supported")

let rec get_rot_side get_side (t,r) s =
    let side = 
        match r with
        | 0 -> get_side t s
        | _ -> get_rot_side get_side (t,(r+1) mod 4) (rotate s)
    in
    (* side read top to bottom, left to right, but rotation is clockwise *)
    match s with
    | LEFT | RIGHT -> rev side
    | UP | DOWN -> side
    | _ -> raise (Domain "Only 2d is supported")

let rec render_rot_tile render (t,r) =
    match r with 
    | 0 -> render t
    | _ ->
        let s = render_rot_tile render (t,r-1) in
        rotate_board s