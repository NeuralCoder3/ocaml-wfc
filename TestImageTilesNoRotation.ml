#use "Definition.ml";;
#use "ImageTilesNoRotation.ml";;


(* let folder = "circuit" *)
let folder = "WaveFunctionCollapse/tiles/simple"

(* map (Printf.sprintf "%s/%s" folder) *)

let fileNames =
    tileNames folder
    |> map (Printf.sprintf "%s/%s" folder)

let images = tile_images fileNames

let gE = computeBoard boardLenPrinter (img_side images) (tileOptions images) (5,5)

let _ = match gE with 
        | None -> ()
        | Some b -> 
            board_render_2d_tile (tile_to_img images) b
            |> saveBMP "img.bmp";
            print_endline "Done"

