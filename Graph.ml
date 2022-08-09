#use "util.ml";;

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

let updateNode graph id data =
    let nodes = graph.nodes in
    let node = List.find (fun node -> node.id = id) nodes in
    let new_node = {node with data = data} in
    let new_nodes = replace nodes node new_node in
    let new_graph = {graph with nodes = new_nodes} in
    new_graph

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