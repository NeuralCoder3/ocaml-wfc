#use "Definition.ml";;

#use "SuperposWFC.ml";;
#use "BoardWFC.ml";;

let idFromCoordinates x y z = (x,y,z)

let graphFromCube cube =
    let boardGraphs = mapi (fun z -> graphFromBoardId (fun x y -> idFromCoordinates x y z)) cube in
    let nodes = map (fun g -> g.nodes) boardGraphs in
    let edges = map (fun g -> g.edges) boardGraphs in
    let connections =
        mapi
        (fun z board ->
            if z = 0 then [] else
            mapi
            (fun y row ->
                mapi
                (fun x _ ->
                    {
                        fst = idFromCoordinates x y (z-1);
                        snd = idFromCoordinates x y z;
                        info = BOTTOM
                    }
                )
                row
            )
            board
        ) 
        cube
        |> concat
        |> concat
    in
    {
        nodes = concat nodes;
        edges = connections @ concat edges
    }


let cubeFromGraph graph =
    let nodes = graph.nodes in
    let d = fold_left max 0 (map (fun node -> let (_,_,z) = node.id in z) nodes) + 1 in
    List.init d
        (fun z ->
            boardFromGraph
            {
                nodes = filter_map (fun node -> let (x',y',z') = node.id in 
                    if z' = z then Some {node with id = (x',y')} else None) nodes;
                edges = []
                (* edges = map (fun edge -> 
                    let (x1,y1,z1) = edge.fst in
                    let (x2,y2,z2) = edge.snd in
                    {fst = (x1,y1); snd = (x2,y2); info = edge.info}) graph.edges *)
            }
        )