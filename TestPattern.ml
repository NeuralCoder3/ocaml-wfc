#use "Definition.ml";;
#use "ImageTilesNoRotation.ml";;
#use "PatternGen.ml";;

let filename = "WaveFunctionCollapse/tiles/abstract.png"
let image = read_image filename
let images = pattern_stride (3,3) image (6,6)

let _ = print_endline "Generated tiles"

let gE = computeBoard boardLenPrinter (img_side images) (tileOptions images) (10,10)

let _ = match gE with 
        | None -> print_endline "Impossible"
        | Some b -> 
            board_render_2d_tile (tile_to_img images) b
            |> saveBMP "img.bmp";
            print_endline "Done"

