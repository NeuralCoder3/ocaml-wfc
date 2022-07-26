#use "GraphWFC.ml";;

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

let determineSuperpositionRestrict restrict debug graph =
    determine impossible finished restrict collectOptions debug graph
    |> optionMap extractSolution

let determineSuperposition compatible =
    determineSuperpositionRestrict (restrict compatible)

let superPosPrinter dataPrinter data =
    "[" ++
    String.concat ", " (map dataPrinter data) ++
    "]"