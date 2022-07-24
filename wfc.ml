(*
    - [ ] map type: Array / Function
    - [ ] collapseField i j B => set & updateField
    - [ ] updateField
        - [ ] Sudoku
        - [ ] filter boundary fits
    - [ ] generate Rotations
    - [ ] Tetris



*)

(* #use "topfind";; *)

(* #use "topfind";;
#thread;;
#camlp4o;;
#require "core.top";;
#require "core.syntax";; *)
(* #use "topfind";;
#require "Images";; *)

(* open Images;; *)

#use "Image.ml";;
open List;;

(* let f = Images.load;; *)

(* let imgMap f = map (map f) *)
(* let imgMapi f = mapi (fun y -> mapi (fun x -> f (x, y))) *)



(*
 https://ocaml.org/p/camlimages/5.0.1/doc/Images/index.html  
*)

type 'a state = 
    | Collapsed of 'a 
    | Superposition of ('a list)

let w = 50
let h = w


type 'a board = ('a list) list

type 'a result =
    | Error
    | Solution of ('a board)
    | Progress of (('a state) board)

exception Impossible of string


(* let boardFold  *)

let foldi f a xs =
    snd(fold_left 
    (fun (i,a) v ->
        (i+1,f i v a)
    )
    (0,a)
    xs)

let boardFoldi f a board = 
    foldi (fun y xs a -> 
            foldi (fun x v a ->
                f (x,y) v a
            )
            a
            xs
    ) a board

let boardMapi f board =
    mapi (fun y ->
        mapi (fun x ->
            f (x,y)
        )
    ) board

let getField board (x,y) =
    nth (nth board y) x

let updateField board (x,y) v =
    boardMapi (fun (x',y') v' ->
        if x' == x && y' == y then
            v
        else
            v'
    ) board


let determineFields board =
    boardMapi (fun _ v ->
        match v with 
        | Superposition [x] -> Collapsed x
        | _ -> v
    ) board

let isCollapsed board =
    if exists (exists (fun v ->
        match v with
        | Superposition _ -> true
        | _ -> false
    )) board then
        None
    else
        Some (boardMapi
            (fun _ v ->
                match v with
                | Collapsed x -> x
                | _ -> raise (Impossible "only contains collapsed fields")
            ) board)

let getMinOption board =
    snd(boardFoldi
    (fun idx v ((min_value,_) as a) ->
        match v with
        | Superposition xs ->
            if List.length xs < min_value then
                (List.length xs, Some (idx,xs))
            else
                a
        | _ -> a
    )
    (Int.max_int,None)
    board)

let choose_random xs =
    let idx = Random.int (List.length xs) in
    nth xs idx

let collapseField board =
    match getMinOption board with
    | Some (idx,xs) ->
        updateField board idx (Collapsed (choose_random xs)) 
    | None -> board


type sides = 
    | Top
    | Right
    | Bottom
    | Left

type 'a rot_tile = 'a * int


let rotate s =
    match s with
    | Top -> Right
    | Right -> Bottom
    | Bottom -> Left
    | Left -> Top

let opposite s =
    match s with
    | Top -> Bottom
    | Right -> Left
    | Bottom -> Top
    | Left -> Right

let rec get_rot_boundary get_boundary (t,r) s =
    match r with
    | 0 -> get_boundary t s
    | _ -> get_rot_boundary get_boundary (t,(r+1) mod 4) (rotate s)

let isImpossible board =
    exists (exists (fun v ->
        v = Superposition []
    )) board

let compatible_tile get_boundary rot_tile side connected_rot_tile = 
    get_rot_boundary get_boundary rot_tile side = 
    rev(get_rot_boundary get_boundary connected_rot_tile (opposite side))

let compatible get_boundary rot_tile side field = 
    match field with
    | Collapsed t -> 
        compatible_tile get_boundary rot_tile side t
    | Superposition xs ->
        exists (compatible_tile get_boundary rot_tile side) xs

let restrictBoard get_boundary board =
    boardMapi (fun (x,y) v ->
        match v with
        | Superposition ys ->
            (* let bf = get_rot_boundary (getField board (x,y)) in *)
            Superposition 
            (filter
            (fun rt ->
                (x  <=0 || compatible get_boundary rt Left    (getField board (x-1,y))) &&
                (y  <=0 || compatible get_boundary rt Top   (getField board (x,y-1))) &&
                (x+1>=w || compatible get_boundary rt Right  (getField board (x+1,y))) &&
                (y+1>=h || compatible get_boundary rt Bottom (getField board (x,y+1)))
            )
            ys)
        | Collapsed x -> v
    ) board

let rotate_board xs =
    let h = List.length xs in
    let w = List.length (nth xs 0) in
    List.init w
        (fun y -> List.init h
            (fun x -> nth (nth xs (h-1-x)) y))





let rec render_rot_tile render (t,r) =
    match r with 
    | 0 -> render t
    | _ ->
        let s = render_rot_tile render (t,r-1) in
        rotate_board s

let render_field render_tile superpos_render f =
    match f with
    | Collapsed v ->
        render_rot_tile render_tile v
    | Superposition xs ->
        superpos_render

let connect_lines xs =
    rev(snd(List.fold_left
    (fun (ys,acc) _ ->
        (map tl ys,
        List.concat (map hd ys)::
        acc
        )
    )
    (xs,[])
    (hd xs)
    ))













(* let rotate_tile (t,r) =
    (t,(r+1) mod 4) *)

(* restrict  *)


(* let collapseField board =
    let superpos = List.filter (fun x -> match x with
        | Superposition _ -> true
        | _ -> false) (collapse board) in
    match superpos with
    | [] -> 
        (* everything is collapsed *)
        Solution (map (fun x -> 
        match x with
        | Collapsed v -> v
        | _ -> raise (Impossible "Only collapsed fields"))
         board)
    | _ -> 
        fold_left
            (fun (minv,min_e)) *)



(* let counter =
    let counter = ref 0 in
    fun () ->
        counter := !counter + 1;
        !counter *)

let board_to_img render_rot_tile filename board =
    board
    |> boardMapi (fun _ -> render_rot_tile)
    |> map (fun xs -> connect_lines xs)
    |> concat
    (* |> saveBMP (Printf.sprintf "board_%d.bmp" (counter())) *)
    |> saveBMP filename

let board_to_string tile_to_string superpos_string board =
    board
    |> boardMapi (fun _ -> render_field tile_to_string superpos_string)
    |> map (fun xs -> map implode (connect_lines xs))
    (* |> boardMapi (fun _ -> map implode) *)
    (* |> map (fun xs -> [
            String.concat "" (map hd xs);
            String.concat "" (map (fun ys -> hd (tl ys)) xs);
            String.concat "" (map (fun ys -> hd (tl (tl ys))) xs)
        ]) *)
    |> map (String.concat "\n")
    |> String.concat "\n"




(* type tile =
    | Blank
    | Track
    (* clockwise *)

let get_boundary t s =
    match t,s with
    | (Blank,_) -> "B"
    | (Track,Top) -> "B"
    | (Track,_) -> "T"

let tiles =
    [
        (Blank,0);
        (Track,0);
        (Track,1);
        (Track,2);
        (Track,3)
    ] *)

(* let down_image = read_image "down.png"
let blank_image = read_image "blank.png"
let superpos_image = read_image "superpos.png"


let tile_to_img t =
    match t with
    | Blank -> blank_image
    | Track -> down_image *)




(* let tile_to_string t =
    map explode 
    (match t with
    | Blank -> 
    [
        "   ";
        "   ";
        "   "
    ]
    | Track ->
    [
        "   ";
        "###";
        " # "
    ])

let superpos_string =
    map explode
    [
        "***";
        "***";
        "***"
    ] *)





type tile =
    | IC
    | Blank
    | Pin
    | GrayLine
    | Connector
    | Corner
    | GreenLine
    | Cross
    | LongPin
    | TCross
    | DoubleDiag
    | Diag
    | DoublePin

let getBound side xs =
    nth xs
    (match side with
    | Top -> 0
    | Right -> 1
    | Bottom -> 2
    | Left -> 3)


    (* clockwise *)
let get_boundary t s =
    explode (match t with
    | IC        -> "III" 
    | Blank     -> "   "
    | Pin       -> getBound s ["   ";" G ";"   ";"   "]
    | GrayLine  -> getBound s ["   ";" L ";"   ";" L "]
    | Connector -> getBound s ["I  ";" G ";"  I";"III"]
    (* | Connector -> getBound s ["I  ";" G ";"I  ";"III"] *)
    | Corner    -> getBound s ["I  ";"   ";"   ";"  I"]
    (* | Corner    -> getBound s ["I  ";"   ";"   ";"I  "] *)
    | GreenLine -> getBound s ["   ";" G ";"   ";" G "]
    | Cross     -> getBound s [" L ";" G ";" L ";" G "]
    | LongPin   -> getBound s [" L ";"   ";" G ";"   "]
    | TCross    -> getBound s [" G ";" G ";"   ";" G "]
    | DoubleDiag-> getBound s [" G ";" G ";" G ";" G "]
    | Diag      -> getBound s [" G ";" G ";"   ";"   "]
    | DoublePin -> getBound s ["   ";" G ";"   ";" G "]
    )

let tiles =
    concat (map (fun x -> [(x,0)])
    [
        IC;
        Blank
    ])@
    concat (map (fun x -> [(x,0);(x,1)])
    [
        GrayLine;
        GreenLine;
        Cross;
        DoubleDiag;
        DoublePin
    ]
    )@
    concat (map (fun x -> [(x,0);(x,1);(x,2);(x,3)])
    [
        Pin;
        Connector;
        Corner;
        LongPin;
        TCross;
        Diag
    ]
    )


let tile_to_img t =
    read_image (Printf.sprintf "circuit/%d.png" (
    match t with
    | IC         -> 0
    | Blank      -> 1
    | Pin        -> 2
    | GrayLine   -> 3
    | Connector  -> 4
    | Corner     -> 5
    | GreenLine  -> 6
    | Cross      -> 7
    | LongPin    -> 8
    | TCross     -> 9
    | DoubleDiag -> 10
    | Diag       -> 11
    | DoublePin  -> 12
    ))

let superpos_image = read_image "circuit/s.png"

let render_field tile_to_img s =
    match s with 
    | Collapsed v -> render_rot_tile tile_to_img v
    | Superposition v -> superpos_image







let board = 
    List.init h
        (fun y -> List.init w
            (fun x -> Superposition tiles))

let rec repeat f a =
    match f a with
    | None -> repeat f a
    | Some a -> a

let rec fixpoint f a =
    let a' = f a in
    if a = a' then
        a
    else
        fixpoint f a'

let count =
    let counter = ref 0 in
    fun () ->
        counter := !counter + 1;
        !counter

let rec iterate board =
    let restricted_board = fixpoint (restrictBoard get_boundary) board in
    let determined_board = determineFields restricted_board in
    (* let _ = print_string (board_to_string determined_board) in
    let _ = print_string "\n\n\n" in *)
    let i = count () in
    let filename = Printf.sprintf "board_%d.bmp" i in
    let _ = print_endline (Printf.sprintf "Saving %s" filename) in
    if (i mod 5) = 0 then
        board_to_img (render_field tile_to_img)
        filename determined_board
    else
        ();
    match isCollapsed determined_board with
    | Some b -> 
        Some b
    | None -> 
        if isImpossible determined_board then
            None
        else
            Some (repeat (fun determined_board ->
            let collapsed_board = collapseField determined_board in
            match iterate collapsed_board with
            | Some b -> Some b
            | None -> None (* repeat collapse *)
            )
            determined_board)

(* let b2 = collapseField board;;
let b3 = restrictBoard b2;; *)

(* let _ = print_string (board_to_string b3);; *)

let finished_board = iterate board
let render_finished () = 
    match finished_board with
    | Some b ->
        board_to_img (render_rot_tile tile_to_img) "board.bmp" b
    | None ->
        print_string "Impossible"

let _ = render_finished ()

    (* 
    isImpossible
    isCollapsed

    *)