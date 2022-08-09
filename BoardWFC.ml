#use "SuperposWFC.ml";;

(* specialization:
   graph = 2d board
*)


let dirPrinter dir =
    match dir with
    | LEFT -> "LEFT"
    | RIGHT -> "RIGHT"
    | UP -> "UP"
    | DOWN -> "DOWN"
    | TOP -> "TOP"
    | BOTTOM -> "BOTTOM"

let idFromCoordinates x y = (x,y)

let graphFromBoardId getId board =
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

let graphFromBoard =
    graphFromBoardId idFromCoordinates


let opposite direction =
    match direction with
    | LEFT -> RIGHT
    | RIGHT -> LEFT
    | UP -> DOWN
    | DOWN -> UP
    | TOP -> BOTTOM
    | BOTTOM -> TOP

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

let computeBoard_init debug side init =
    let uni_graph = graphFromBoard init in
    let graph = makeBidir opposite uni_graph in
    determineSuperposition (compatible side) (fun g -> debug (boardFromGraph g)) graph
    |> option_map boardFromGraph

let computeBoard debug side options (w,h) =
    let board = List.init h (fun _ -> List.init w (fun _ -> 
        options
    )) in
    computeBoard_init debug side board