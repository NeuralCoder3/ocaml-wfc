
(* #use "Image.ml";; *)

let comp = compare;;
let (<<) f g x = f(g(x))
let (++) = String.cat

let explode s = List.init (String.length s) (String.get s)
let implode xs = String.of_seq (List.to_seq xs)

;;
#use "Image.ml";;
open List;;

let _ = Random.self_init ()

type 'a board = 'a list list

let updateList xs index value =
    mapi (fun i v -> if i=index then value else v) xs

let updateBoard board (x,y) value =
    updateList board y (updateList (nth board y) x value)

let apply f xs =
    let _ = map (fun x -> f x) xs in ()

let boardMapi f board =
    mapi (fun y ->
        mapi (fun x ->
            f (x,y)
        )
    ) board

let boardMap f board =
    boardMapi (fun _ -> f) board

let boardPrinter board =
    apply 
        (fun row ->
            apply (Printf.printf "%s ") row;
            Printf.printf "\n"
        )
        board;
    Printf.printf "\n"

let boardLenPrinter b = (boardPrinter << (boardMap (Int.to_string << length))) b

let connect_lines xs =
    rev(snd(List.fold_left
    (fun (ys,acc) _ ->
        (map tl ys,
        List.concat (map hd ys)::
        acc
        )
    )
    (xs,[])
    (hd xs)
    ))

let board_render_2d_tile render_tile board =
    board
    |> boardMapi (fun _ -> render_tile)
    |> map (fun xs -> connect_lines xs)
    |> concat

(* let stringBoardBoardToString board =
    board
    |> map (fun xs -> map implode (connect_lines xs))
    |> map (String.concat "\n") *)






(* type 'id 'data 'info graph
    = ('id * 'data) list * ('id * 'id * 'info) list *)

type ('id,'data) node = {id : 'id; data : 'data}
type ('id,'info) edge = {fst : 'id; snd : 'id; info : 'info}

type ('id,'data,'info) graph = 
    { nodes : (('id,'data) node) list;  
      edges : (('id,'info) edge) list }

let printGraph dataToString idToString infoToString graph =
    let nodes = graph.nodes in
    let edges = graph.edges in
    let printNode node =
        Printf.sprintf "%s: %s" (idToString node.id) (dataToString node.data) in
    let printEdge edge =
        Printf.sprintf "%s -> %s: %s" (idToString edge.fst) (idToString edge.snd) (infoToString edge.info) in
    let printNodes =
        map printNode nodes in
    let printEdges =
        map printEdge edges in
    let print =
        [Printf.sprintf "nodes:"] @
        printNodes @
        [Printf.sprintf "edges:"] @
        printEdges 
    in
    print
    |> apply (Printf.printf "%s\n")


let getNode graph id =
    let nodes = graph.nodes in
    let node = List.find (fun node -> node.id = id) nodes in
    node

let rec replace xs a b =
    match xs with
    | [] -> []
    | x :: xs ->
        if x = a then b :: replace xs a b
        else x :: replace xs a b

let updateNode graph id data =
    let nodes = graph.nodes in
    let node = List.find (fun node -> node.id = id) nodes in
    let new_node = {node with data = data} in
    let new_nodes = replace nodes node new_node in
    let new_graph = {graph with nodes = new_nodes} in
    new_graph

let rec propagate restrict graph id  =
    let outgoing = filter (fun edge -> edge.fst = id) graph.edges in
    fold_left
        (fun graph edge ->
            let node = getNode graph edge.snd in
            let new_data = restrict graph edge in
            if node.data <> new_data then
                propagate restrict 
                    (updateNode graph edge.snd new_data)
                    edge.snd
            else
                graph
        )
        graph
        outgoing


let determine impossible finished restrict collectOptions debug =
    let rec determine_graph graph =
        Printf.printf "Graph: \n";
        debug graph;
        let options = collectOptions graph in
        fold_left
            (fun result option ->
                match result with 
                | Some r -> result
                | None ->
                    let id=option.id in
                    let new_data=option.data in
                    let new_graph = updateNode graph id new_data in
        (* Printf.printf "updated Graph: \n"; *)
                    (* debug new_graph; *)
                    let propagated = propagate restrict new_graph id in
                    (* let (x,y) = id in *)
        (* Printf.printf "propagated Graph (%d,%d): \n" x y; *)
                    (* debug propagated; *)
                    if impossible propagated then 
                        (* (Printf.printf "impossible\n"; *)
                        None
                        (* ) *)
                    else 
                        if finished propagated then
                            Some propagated
                        else
                            (* (Printf.printf "recurse\n"; *)
                            determine_graph propagated
                            (* Some propagated) *)
            )
            None
            options
    in
        determine_graph


(* specialization:
   'd data = 'd list
*)

let data_predicate f graph =
    let nodes = graph.nodes in
    for_all (fun n -> f n.data) nodes


let impossible graph =
    data_predicate (fun data -> data = []) graph

let finished graph =
    data_predicate (fun data -> 
        match data with 
        | [_] -> true
        | _ -> false
    ) graph

let shuffle xs = 
    let rand = map (fun d -> (Random.bits (),d)) xs in
    map snd (sort (fun (a,_) (b,_) -> Int.compare a b) rand)

(* TODO: make lazy *)
let collectOptions graph =
    graph.nodes
    |> filter (fun node -> length node.data > 1)
    |> sort (fun a b -> List.compare_lengths (a.data) (b.data))
    |> map (fun node -> map (fun d -> {node with data = [d]}) (shuffle node.data))
    |> flatten

(* 
   edge.fst data changed => edge.snd data needs to be restricted
   returns the new data for edge.snd
*)
let restrict compatible graph edge =
    let fst_node = getNode graph edge.fst in
    let snd_node = getNode graph edge.snd in
    filter
        (fun d2 -> 
            exists
                (fun d1 -> compatible d1 edge.info d2)
                fst_node.data
        )
        snd_node.data

let extractSolution graph =
    let singularNodes =
        map (fun node -> {node with data = hd node.data}) (graph.nodes) in
    { graph with nodes = singularNodes }

let optionMap f opt =
    match opt with
    | Some x -> Some (f x)
    | None -> None

let determineSuperposition compatible debug graph =
    determine impossible finished (restrict compatible) collectOptions debug graph
    |> optionMap extractSolution

let superPosPrinter dataPrinter data =
    "[" ++
    String.concat ", " (map dataPrinter data) ++
    "]"


(* specialization:
   graph = 2d board
*)

type direction = LEFT | RIGHT | UP | DOWN

let dirPrinter dir =
    match dir with
    | LEFT -> "LEFT"
    | RIGHT -> "RIGHT"
    | UP -> "UP"
    | DOWN -> "DOWN"

let idFromCoordinates x y = (x,y)
(* let getId x y = Printf.sprintf "%d_%d" x y in *)

let graphFromBoard board =
    let getId = idFromCoordinates in
    let nodes = board
        |> mapi (fun r row -> mapi (fun c d -> {id = getId c r; data = d}) row)
        |> flatten
    in
    let edges = 
        board
        |> mapi (fun y -> mapi (fun x _ -> 
            (if y=0 then [] else [{fst = getId x (y-1); snd = getId x y; info = DOWN}]) @
            (if x=0 then [] else [{fst = getId (x-1) y; snd = getId x y; info = RIGHT}])
            ))
        |> flatten
        |> flatten
    in
    { nodes = nodes; edges = edges }

let makeBidir infoMirror graph =
    let edges = graph.edges in
    let edges_mirrored =
        map (fun edge ->
            {
                fst = edge.snd; 
                snd = edge.fst; 
                info = infoMirror edge.info
            }
        )
        edges
    in
    { graph with edges = edges @ edges_mirrored }


let opposite direction =
    match direction with
    | LEFT -> RIGHT
    | RIGHT -> LEFT
    | UP -> DOWN
    | DOWN -> UP

let boardFromGraph graph =
    let nodes = graph.nodes in
    let w = fold_left max 0 (map (fun node -> fst node.id) nodes) + 1 in
    let h = fold_left max 0 (map (fun node -> snd node.id) nodes) + 1 in
    List.init h
        (fun y ->
            List.init w
                (fun x ->
                    let id = idFromCoordinates x y in
                    let node = getNode graph id in
                    node.data
                )
        )

let compatible side s dir t =
    side s dir = side t (opposite dir)

let computeBoard debug side options (w,h) =
    let board = List.init h (fun _ -> List.init w (fun _ -> 
        options
    )) in
    let uni_graph = graphFromBoard board in
    let graph = makeBidir opposite uni_graph in
    determineSuperposition (compatible side) (fun g -> debug (boardFromGraph g)) graph
    |> optionMap boardFromGraph


(*
   rotated tiles
*)

type 'a rot_tile = 'a * int

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




let rotate_board xs =
    let h = List.length xs in
    let w = List.length (nth xs 0) in
    List.init w
        (fun y -> List.init h
            (fun x -> nth (nth xs (h-1-x)) y))

let rec render_rot_tile render (t,r) =
    match r with 
    | 0 -> render t
    | _ ->
        let s = render_rot_tile render (t,r-1) in
        rotate_board s

















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


(* let tilePrinter tile =
    match tile with
    | UPTile -> "UP"
    | DOWNTile -> "DOWN"
    | LEFTTile -> "LEFT"
    | RIGHTTile -> "RIGHT"
    | Empty -> "Empty" *)


let initBoard =
    let board = List.init 3 (fun _ -> List.init 3 (fun _ -> tileOptions)) in
    (* updateBoard board (0,0) [Empty] *)
    board


let initGraph =
    let graph = graphFromBoard initBoard in
    let graph = makeBidir opposite graph in
    graph
    (* updateNode graph (0,0) [UPTile] *)

 (* let _ = boardPrinter initBoard
let _ = boardPrinter (boardFromGraph initGraph)
let _ = printGraph (superPosPrinter tilePrinter) pairPrint dirPrinter initGraph *)

(* let _ = propagate (restrict (compatible side)) initGraph (0,0) *)

(* let g2 = propagate (restrict (compatible side)) initGraph (0,0)
let _ = boardPrinter (boardFromGraph g2) *)


let gE = computeBoard boardLenPrinter (get_rot_side side) tileOptions (150,150)
(* let _ = match gE with
    | Some g -> boardPrinter (map (map (fun a -> [a])) g)
    | None -> Printf.printf "None\n" *)

(* let tileToString tile =
    match tile with
    | UPTile -> "⊤"
    | DOWNTile -> "⊥"
    | LEFTTile -> "⊢"
    | RIGHTTile -> "⊣"
    | Empty -> " " *)


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

(*
   (string list list) list list

   [
    [
        ["   ";
         "===";
         " | "];
        [" | ";
         "===";
         "   "];
        ...
    ];
    ...
   ]

   =>
   string list
   [
        "    | ...";
        "======...";
        ...
   ]

*)

let _ = match gE with 
        | None -> ()
        | Some b -> 
            (* (boardMap (render_rot_tile tileToStringBoard) b)
            |> stringBoardBoardToString *)
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


(*
   SMT
   node = formula
   connection = shared vars
   [v] => if contain v then [v] else filter (!= not v)
   maybe need adjustment of superpos compare/restrict
*)

(*
   Sudoku
*)
