(*
    - [ ] map type: Array / Function
    - [ ] collapseField i j B => set & updateField
    - [ ] updateField
        - [ ] Sudoku
        - [ ] filter boundary fits
    - [ ] generate Rotations
    - [ ] Tetris
*)

open List;;

let imgMap f = 
    map (map f)

let imgMapi f = 
    mapi (fun y -> mapi (fun x -> f (x,y)))


let _ = print_string "Hello worlds\n"

;;