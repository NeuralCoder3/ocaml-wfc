#use "util.ml";;

(* type 'a board = 'a list list *)

let updateBoard board (x,y) value =
    updateList board y (updateList (nth board y) x value)

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

let board_render_2d_tile render_tile board =
    board
    |> boardMapi (fun _ -> render_tile)
    |> map (fun xs -> connect_lines xs)
    |> concat

let rotate_board xs =
    let h = List.length xs in
    let w = List.length (nth xs 0) in
    List.init w
        (fun y -> List.init h
            (fun x -> nth (nth xs (h-1-x)) y))