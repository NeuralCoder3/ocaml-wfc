#use "Definition.ml";;
(* # use "Image.ml";; *)
#use "SuperposWFC.ml";;
#use "Board.ml";;

(*
   Sudoku
*)

let indices = List.init 9 (fun x -> x)
let numbers = map (fun x -> x+1) indices

let lines = 
   map (fun x -> map (fun y -> 
      map 
      (fun z -> 
         (if z <> x then 
            [((x,y),(z,y))] (* row *)
         else
            [])@
         (if z <> y then 
            [((x,y),(x,z))] (* column *)
         else
            [])
      )
      indices
   ) indices) indices
   |> concat |> concat |> concat

let squares =
   let three = [0;1;2] in
   map (fun bx ->
      map (fun by ->
            map (fun ix ->
               map (fun iy ->
            map (fun ix2 ->
               map (fun iy2 ->
                     if ix2 <> ix || iy2 <> iy then
                        [((ix,iy),(ix2,iy2))]
                     else
                        []
               ) three
            ) three
               ) three
            ) three
      ) three
   ) three
   |> concat |> concat |> concat
   |> concat |> concat |> concat

let sudokuToGraph sudoku =
   let grid = sudoku in
   (* let grid = map
      (map
         (fun x ->
            (* match x with
            | None -> numbers
            | Some n -> [n] *)
         )
      )
      sudoku
   in *)
   let nodes =
      mapi (fun y ->
         mapi (fun x d ->
            {
               id = (x,y);
               data = d
            }
         )
      )
      grid
      |> concat
   in
   let edges =
      (lines @ squares)
      |> map (fun (a,b) ->
         {
            fst = a;
            snd = b;
            info = ()
         }
         )
   in
   {
      nodes = nodes;
      edges = edges
   }

let graphToSudoku graph =
   Array.init 9
   (fun y -> Array.init 9 
   (fun x -> (getNode graph (x,y)).data)
   )
   |> Array.to_list
   |> map Array.to_list

(* TODO: correct restrict *)
let restrict graph edge =
    let fst_node = getNode graph edge.fst in
    let snd_node = getNode graph edge.snd in
    match fst_node.data with
    | [a] -> filter (fun b -> b <> a) snd_node.data
    | _ -> snd_node.data
    (* filter
    (fun b ->
      exists (fun a -> a<>b) fst_node.data
    )
    snd_node.data *)
   
let printSudoku sudoku =
   sudoku
   |> map (String.concat " ")
   |> String.concat "\n"
   |> Printf.printf "%s\n"
   
let debug g =
   Printf.printf "Sudoku:\n";
   g
   |> graphToSudoku
   |> boardMap (fun x ->
      match x with
      (* | [v] -> "_" ++ Int.to_string v ++ "_"
      | _ -> "[" ++ Int.to_string (List.length x) ++ "]" *)
      | [v] -> Int.to_string v
      | _ -> " "
   )
   |> printSudoku;
   Printf.printf "\n"

let solveSudoku sudoku =
   determineSuperpositionRestrict restrict debug (sudokuToGraph sudoku)
   |> option_map graphToSudoku
   |> option_map (printSudoku << (boardMap Int.to_string))

let prepare =
      boardMap
         (fun x ->
            if x = 0 then numbers else [x]
            (* match x with
            | None -> numbers
            | Some n -> [n] *)
         )
let sudoku = 
   prepare 
[
[5;3;0;0;7;0;0;0;0];
[4;0;0;1;9;5;0;0;0];
[0;9;8;0;0;0;0;6;0];
[8;0;0;0;6;0;0;0;3];
[4;0;0;8;0;3;0;0;1];
[7;0;0;0;2;0;0;0;6];
[0;6;0;0;0;0;2;8;0];
[0;0;0;4;1;9;0;0;5];
[0;0;0;0;8;0;0;7;9]
]

let _ = solveSudoku sudoku
(* let _ = debug (sudokuToGraph sudoku) *)


(*
  ILP
*)

(*
   Image tiles => boundary by pixels
*)