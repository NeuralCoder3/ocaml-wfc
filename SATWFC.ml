#use "Definition.ml";;
#use "SuperposWFC.ml";;
(*
   SMT
   node = formula
   connection = shared vars
   [v] => if contain v then [v] else filter (!= not v)
   maybe need adjustment of superpos compare/restrict
*)

type 'a negVar = Positive of 'a | Negative of 'a
type 'a formula = 'a negVar list list
(*
   conjunction of disjunction of negation of variable
*)

let getVar a =
    match a with
    | Positive x -> x
    | Negative x -> x
    
let isNegated a =
    match a with
    | Positive _ -> false
    | Negative _ -> true

let formulaToGraph form =
    let nodes = mapi 
        (fun i xs ->
            {
                id = i;
                data = xs
            }
        )
        form in
    let edges =
        mapi 
        (fun i xs ->
            mapi 
            (fun j ys ->
                if exists (fun a -> exists (fun b -> a = b) ys) xs then
                [{
                    fst = i;
                    snd = j;
                    info = filter (fun a -> exists (fun b -> a = b) ys) xs
                }]
                else []
            )
            form
        ) form
        |> concat
        |> concat
    in
    {
        nodes = nodes;
        edges = edges
    }

let restrictFormula graph edge =
    let fst_node = getNode graph edge.fst in
    let snd_node = getNode graph edge.snd in
    let fst_formula = fst_node.data in
    let snd_formula = snd_node.data in
    match fst_formula with
    | [a] ->
        if exists (fun b -> b = a) snd_formula then
            [a] (* already solved clause *)
        else
            filter
            (fun b -> getVar b <> getVar a)
            snd_formula
    | _ -> snd_formula

let graphToFormula graph =
    map 
    (fun n -> n.data)
    graph.nodes

let formulaToStr formula =
    let varToStr var =
        match var with
        | Positive x -> "" ++ x ++ ""
        | Negative x -> "~" ++ x
    in
    formula
    |> map (map varToStr)
    |> map (String.concat " \\/ ")
    |> String.concat " /\\ "

let solve form =
    determineSuperpositionRestrict restrictFormula (Printf.printf "%s\n" << formulaToStr << graphToFormula) (formulaToGraph form)

let rec makeUnique xs =
    match xs with
    | [] -> []
    | x::xr -> 
        x::makeUnique (filter (fun a -> a <> x) xr)

let sol = solve 
    [
        (* [positive "a"; negative "b"; negative "c"];
        [Negative "d"; Positive "e"; Positive "f"] *)
        [Positive "a"; Positive "b"; Positive "c"];
        [Positive "a"; Positive "b"; Negative "c"];
        [Positive "a"; Negative "b"; Positive "c"];
        [Positive "a"; Negative "b"; Negative "c"];
        [Negative "a"; Positive "b"; Positive "c"];
        [Negative "a"; Positive "b"; Negative "c"];
        [Negative "a"; Negative "b"; Negative "c"]
    ]

let _ = match sol with
    | Some x -> 
        x
        |> graphToFormula
        |> makeUnique
        |> map (fun a -> [a])
        |> formulaToStr
        |> Printf.printf "Solution: %s\n"
    | None -> Printf.printf "no solution\n"