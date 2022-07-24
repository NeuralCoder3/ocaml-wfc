
open List;;

let explode s = List.init (String.length s) (String.get s)
let implode xs = String.of_seq (List.to_seq xs)
let mapTriple f (a,b,c) = (f a,f b,f c)
let (<<) f g x = f(g(x))
exception Domain