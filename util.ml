(* Other *)
let comp = compare;;
let _ = Random.self_init ()
let (<<) f g x = f(g(x))

let option_map f opt =
    match opt with
    | Some x -> Some (f x)
    | None -> None

(* String *)
let (++) = String.cat
let explode s = List.init (String.length s) (String.get s)
let implode xs = String.of_seq (List.to_seq xs)

(* Tuple *)
let mapTriple f (a,b,c) = (f a,f b,f c)

(* List *)
open List;;
let updateList xs index value =
    mapi (fun i v -> if i=index then value else v) xs

let rec replace xs a b =
    match xs with
    | [] -> []
    | x :: xs ->
        if x = a then b :: replace xs a b
        else x :: replace xs a b

let apply f xs =
    let _ = map (fun x -> f x) xs in ()

let connect_lines xs =
    rev(snd(fold_left
    (fun (ys,acc) _ ->
        (map tl ys,
        concat (map hd ys)::
        acc
        )
    )
    (xs,[])
    (hd xs)
    ))

let shuffle xs = 
    let rand = map (fun d -> (Random.bits (),d)) xs in
    map snd (sort (fun (a,_) (b,_) -> Int.compare a b) rand)

let first f xs = find_map f xs

let rec makeUnique xs =
    match xs with
    | [] -> []
    | x :: xs ->
        if List.exists (fun y -> x = y) xs then
            makeUnique xs
        else
            x :: makeUnique xs