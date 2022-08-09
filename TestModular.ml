#use "Definition.ml";;
#use "ImageTilesNoRotation.ml";;


(* let folder = "WaveFunctionCollapse/tiles/simple" *)
let folder = "circuit"

let fileNames =
    tileNames folder
    |> map (Printf.sprintf "%s/%s" folder)

let images = tile_images fileNames

let modular (gx,gy) (w,h) f init =
    let cx = (w+gx/2)/gx in
    let cy = (h+gy/2)/gy in
    List.fold_left
    (fun rows iy ->
        let row = 
            List.fold_left
            (fun imgs ix ->
                let isLeft = (length imgs = 0) in
                let isTop = (length rows = 0) in
                let init_img = 
                    List.init (1+gy) (fun y -> List.init (1+gx) (fun x -> 
                        if x==0 && not isLeft && y==0 && not isTop then
                            let up_row = hd (rev rows) in
                            let upper_img = nth up_row (ix-1) in
                            [hd (rev(hd (rev upper_img)))]
                            (* init *)
                        else if y==0 && x>0 && not isTop then
                            let up_row = hd (rev rows) in
                            let upper_img = nth up_row ix in
                            [nth (hd (rev upper_img)) (x-1)]
                            (* init *)
                        else if x==0 && y>0 && not isLeft then
                            (* let left_img = nth (ix-1) imgs in *)
                            let left_img = hd (rev imgs) in
                            [hd (rev (nth left_img (y-1)))]
                            (* init *)
                        else
                            init
                    )) 
                in
                let img = f init_img in
                let cut_img = tl (map tl img) in
                imgs @ [cut_img]
            )
            []
            (List.init cx (fun x -> x))
        in
        rows @ [row]
    )
    []
    (List.init cy (fun x -> x))
    |> board_render_2d_tile (fun x -> x)

   (* ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a *)

exception Unexpected of string

(* let gE = computeBoard boardLenPrinter (img_side images) (tileOptions images) (8,8) *)
let gE = 
    try
        Some(
            modular (8,8) (160,160) 
            (fun init ->
                match computeBoard_init boardLenPrinter (img_side images) init with
                | None -> raise (Unexpected "No solution")
                | Some board -> board
            )
            (tileOptions images)
        )
    with Unexpected msg -> None

let _ = match gE with 
        | None -> ()
        | Some b -> 
            board_render_2d_tile (tile_to_img images) b
            |> saveBMP "img.bmp";
            print_endline "Done"

