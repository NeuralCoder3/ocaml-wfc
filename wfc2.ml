
#use "Definition.ml";;
#use "Image.ml";;

#use "BoardWFC.ml";;
#use "RotateTile.ml";;


























(*
   SMT
   node = formula
   connection = shared vars
   [v] => if contain v then [v] else filter (!= not v)
   maybe need adjustment of superpos compare/restrict
*)

(*
   Sudoku
*)

(*
   Image tiles => boundary by pixels
*)