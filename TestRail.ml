#use "Definition.ml";;
#use "Image.ml";;

#use "BoardWFC.ml";;
#use "RotateTile.ml";;

(*
   concrete test
   TODO: group from here on
*)

type tiles = 
    | EmptyTile
    | RailTile

type sides = 
    | EmptySide
    | TrackSide

let side t dir =
    match t,dir with
    | EmptyTile,_ -> EmptySide
    | RailTile,UP -> EmptySide
    | RailTile,_ -> TrackSide


let tileOptions = [(EmptyTile,0)] @
    map (fun x -> (RailTile,x)) [0;1;2;3]

let pairPrint (x,y) = Printf.sprintf "(%d,%d)" x y

let initBoard =
    let board = List.init 3 (fun _ -> List.init 3 (fun _ -> tileOptions)) in
    board


let initGraph =
    let graph = graphFromBoard initBoard in
    let graph = makeBidir opposite graph in
    graph



let gE = computeBoard boardLenPrinter (get_rot_side side) tileOptions (10,10)


let tileToStringBoard tile =
    map explode
    (match tile with
    | RailTile -> 
        ["   ";
         "###";
         " # "
         ]
    | EmptyTile -> 
        ["   ";
         "   ";
         "   "]
    )


let _ = match gE with 
        | None -> ()
        | Some b -> 
            board_render_2d_tile (render_rot_tile tileToStringBoard) b
            |> map implode
            |> String.concat "\n"
            |> Printf.printf "%s\n"

let tile_to_img t =
    read_image (Printf.sprintf "rail/%s.png" (
        match t with
        | RailTile -> "rail"
        | EmptyTile -> "empty"
    ))

let _ = match gE with 
        | None -> ()
        | Some b -> 
            board_render_2d_tile (render_rot_tile tile_to_img) b
            |> saveBMP "rail.bmp"