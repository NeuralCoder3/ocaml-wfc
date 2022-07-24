
(* #use "Image.ml";; *)

let comp = compare;;
let (<<) f g x = f(g(x))
open List;;

let _ = Random.self_init ()

(* type 'id 'data 'info graph
    = ('id * 'data) list * ('id * 'id * 'info) list *)

type ('id,'data) node = {id : 'id; data : 'data}
type ('id,'info) edge = {fst : 'id; snd : 'id; info : 'info}

type ('id,'data,'info) graph = 
    { nodes : (('id,'data) node) list;  
      edges : (('id,'info) edge) list }

let _ = print_endline "Test"


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

(* specialization:
   graph = 2d board
*)

type direction = LEFT | RIGHT | UP | DOWN

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
   concrete test
   TODO: group from here on
*)

type tiles = 
    | Empty
    | UPTile
    | DOWNTile
    | LEFTTile
    | RIGHTTile

type sides = 
    | EmptySide
    | TrackSide

let side t dir =
    match t,dir with
    | Empty,_ -> EmptySide
    | UPTile,UP -> EmptySide
    | DOWNTile,DOWN -> EmptySide
    | LEFTTile,LEFT -> EmptySide
    | RIGHTTile,RIGHT -> EmptySide
    | _,_ -> TrackSide

let apply f xs =
    let _ = map (fun x -> f x) xs in ()

let boardMap f board =
    map 
        (fun row -> map (fun x -> f x) row)
    board

let boardPrinter board =
    apply 
        (fun row ->
            apply (Printf.printf "%s ") row;
            Printf.printf "\n"
        )
        board;
    Printf.printf "\n"

let boardLenPrinter b = (boardPrinter << (boardMap (Int.to_string << length))) b

let tileOptions = [UPTile;DOWNTile;LEFTTile;RIGHTTile]







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

let (++) = String.cat

let superPosPrinter dataPrinter data =
    "[" ++
    String.concat ", " (map dataPrinter data) ++
    "]"

let pairPrint (x,y) = Printf.sprintf "(%d,%d)" x y

let dirPrinter dir =
    match dir with
    | LEFT -> "LEFT"
    | RIGHT -> "RIGHT"
    | UP -> "UP"
    | DOWN -> "DOWN"

let tilePrinter tile =
    match tile with
    | UPTile -> "UP"
    | DOWNTile -> "DOWN"
    | LEFTTile -> "LEFT"
    | RIGHTTile -> "RIGHT"
    | Empty -> "Empty"


let updateList xs index value =
    mapi (fun i v -> if i=index then value else v) xs

let updateBoard board (x,y) value =
    updateList board y (updateList (nth board y) x value)

let initBoard =
    let board = List.init 3 (fun _ -> List.init 3 (fun _ -> tileOptions)) in
    (* updateBoard board (0,0) [Empty] *)
    board


let initGraph =
    let graph = graphFromBoard initBoard in
    let graph = makeBidir opposite graph in
    updateNode graph (0,0) [UPTile]

 (* let _ = boardPrinter initBoard
let _ = boardPrinter (boardFromGraph initGraph)
let _ = printGraph (superPosPrinter tilePrinter) pairPrint dirPrinter initGraph *)

(* let _ = propagate (restrict (compatible side)) initGraph (0,0) *)

(* let g2 = propagate (restrict (compatible side)) initGraph (0,0)
let _ = boardPrinter (boardFromGraph g2) *)


let gE = computeBoard boardLenPrinter side tileOptions (10,10)
(* let _ = match gE with
    | Some g -> boardPrinter (map (map (fun a -> [a])) g)
    | None -> Printf.printf "None\n" *)

let tileToString tile =
    match tile with
    | UPTile -> "⊤"
    | DOWNTile -> "⊥"
    | LEFTTile -> "⊢"
    | RIGHTTile -> "⊣"
    | Empty -> " "


let explode s = List.init (String.length s) (String.get s)
let implode xs = String.of_seq (List.to_seq xs)

let tileToStringBoard tile =
    map explode
    (match tile with
    | UPTile -> 
        ["   ";
         "===";
         " | "
         ]
    | DOWNTile -> 
        [" | ";
         "===";
         "   "]
    | LEFTTile -> 
        [" | ";
         " |=";
         " | "]
    | RIGHTTile -> 
        [" | ";
         "=| ";
         " | "]
    | Empty -> 
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

let stringBoardBoardToString b = 
    let h = length b in
    let w = length (nth b 0) in
    let block0 = nth (nth b 0) 0 in
    let ih = length block0 in
    let iw = length (nth block0 0) in

    let oh = h * ih in
    let ow = w * iw in

    (* let out = Array.init oh (fun _ -> Array.init ow (fun _ -> " ")) in *)
    let out = Array.make_matrix oh ow ' ' in
    let _ = mapi 
    (fun y row ->
        mapi 
        (fun x ib ->
            mapi 
            (fun iy irow ->
                mapi 
                (fun ix c ->
                    out.(y*ih+iy).(x*iw+ix) <- c
                )
                irow
            )
            ib
        )
        row
    )
    b in
    out
    |> Array.to_list
    |> map Array.to_list
    |> map implode


let _ = match gE with 
        | None -> ()
        | Some b -> 
            (boardMap tileToStringBoard b)
            |> stringBoardBoardToString
            |> String.concat "\n"
            |> Printf.printf "%s\n"

(*
   rotated tiles
*)

(*
   SMT
*)