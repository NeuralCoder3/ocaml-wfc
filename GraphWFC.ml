#use "Graph.ml"

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
        (* Printf.printf "Graph: \n"; *)
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
                    let propagated = propagate restrict new_graph id in
                    if impossible propagated then 
                        None
                    else 
                        if finished propagated then
                            Some propagated
                        else
                            determine_graph propagated
            )
            None
            options
    in
        determine_graph