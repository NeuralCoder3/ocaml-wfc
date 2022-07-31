#use "Definition.ml";;
#use "Image.ml";;

#use "BoardWFC.ml";;
#use "RotateTile.ml";;

let folder = "circuit"

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

let side id dir =
    let img = nth images id in
    let (w, h) = dimensions img in
    match dir with
    | UP -> hd img
    | RIGHT -> map (fun row -> nth row (w-1)) img
    | DOWN -> rev(nth img (h-1))
    | LEFT -> rev(map (List.hd) img)
        (* hd (transpose img) *)

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


    (* TODO: unify with BoardWFC *)
let compatibleRot side s dir t =
    side s dir = rev(side t (opposite dir))

let computeBoardRot debug side options (w,h) =
    let board = List.init h (fun _ -> List.init w (fun _ -> 
        options
    )) in
    let uni_graph = graphFromBoard board in
    let graph = makeBidir opposite uni_graph in
    determineSuperposition (compatibleRot side) (fun g -> debug (boardFromGraph g)) graph
    |> option_map boardFromGraph

let gE = computeBoardRot boardLenPrinter (get_rot_side side) tileOptions (20,20)


let _ = match gE with 
        | None -> ()
        | Some b -> 
            board_render_2d_tile (render_rot_tile tile_to_img) b
            |> saveBMP "img.bmp"