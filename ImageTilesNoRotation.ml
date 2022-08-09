(* #use "Definition.ml";; *)
#use "Image.ml";;
#use "Board.ml";;
#use "BoardWFC.ml";;
#use "RotateTile.ml";;

(* let folder = "circuit" *)
(* let folder = "WaveFunctionCollapse/tiles/simple" *)

let tileNames folder = 
  Sys.readdir folder
  |> Array.to_list

(* let _ =
    print_string "Found tiles:\n";
    map print_endline tileNames *)

(* map (Printf.sprintf "%s/%s" folder) *)
let tile_images files = 
    files
    |> 
        map 
        (fun id ->
            map 
            (fun rot ->
                (id, rot)
            )
            [0;1;2;3]
        )
    |> concat
    |> map (render_rot_tile read_image)
    |> makeUnique


let img_side tile_images id dir =
    let img = nth tile_images id in
    let (w, h) = dimensions img in
    match dir with
    | UP -> hd img
    | RIGHT -> map (fun row -> nth row (w-1)) img
    | DOWN -> nth img (h-1)
    | LEFT -> map (List.hd) img
    | _ -> raise (Domain "Only 2d is supported")

let tileOptions tile_images = List.init (List.length tile_images) (fun x -> x)

let tile_to_img tile_images id =
    nth tile_images id

(* let gE = computeBoard boardLenPrinter side tileOptions (40,40)


(* let _ = match gE with 
        | None -> ()
        | Some b -> 
            board_render_2d_tile tile_to_img b
            |> saveBMP "img.bmp" *) *)