#use "Definition.ml";;
#use "Image.ml";;

#use "BoardWFC.ml";;
#use "RotateTile.ml";;

let folder = "circuit"
let folder = "WaveFunctionCollapse/tiles/mondriaan"

let tileNames = 
  Sys.readdir folder
  |> Array.to_list

let _ =
    print_string "Found tiles:\n";
    map print_endline tileNames

let images = map
    (read_image << (Printf.sprintf "%s/%s" folder))
    tileNames

let ids = mapi (fun i _ -> i) tileNames

(* sides as they are *)
let side id dir =
    let img = nth images id in
    let (w, h) = dimensions img in
    match dir with
    | UP -> hd img
    | RIGHT -> map (fun row -> nth row (w-1)) img
    | DOWN -> nth img (h-1)
    | LEFT -> map (List.hd) img

(* rotation is a tile property (rot tile indistinguishable from copied, rotated tile) *)
let tileOptions = 
    map 
    (fun id ->
        map 
        (fun rot ->
            (id, rot)
        )
        [0;1;2;3]
    )
    ids
    |> concat

let tile_to_img id =
    nth images id

let gE = computeBoard boardLenPrinter (get_rot_side side) tileOptions (20,20)


let _ = match gE with 
        | None -> ()
        | Some b -> 
            board_render_2d_tile (render_rot_tile tile_to_img) b
            |> saveBMP "img.bmp"