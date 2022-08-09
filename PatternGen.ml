#use "Definition.ml";;
#use "Image.ml";;
#use "Board.ml";;

(* let filename = "WaveFunctionCollapse/tiles/abstract.png"
let image = read_image filename *)

let get_pattern image (sx,sy) (x,y) =
    List.init sy 
    (fun ix ->
        List.init sx
        (fun iy ->
            nth (nth image (y+iy)) (x+ix)
        )
    )

let pattern_stride (stx,sty) image grid_size = 
    let (sx,sy) = grid_size in
    let (w,h) = dimensions image in
    List.init ((w-sx)/stx)
    (fun x ->
        List.init ((h-sy)/sty)
        (fun y ->
            (x*stx,y*sty)
        )
    )
    |> concat
    |> List.map (fun pos ->
        get_pattern image grid_size pos
    )
    (* |> makeUnique *)

let pattern = pattern_stride (1,1)

(* let _ =
    let counter = ref 0 in
    map 
    (fun img ->
        counter := !counter + 1;
        saveBMP (Printf.sprintf "out/%d.bmp" (!counter)) (extend_image img)
    )
    (pattern image (3,3)) *)
