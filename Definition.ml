
type direction = LEFT | RIGHT | UP | DOWN
  | TOP | BOTTOM (* 3d coordinate *)

type ('id,'data) node = {id : 'id; data : 'data}
type ('id,'info) edge = {fst : 'id; snd : 'id; info : 'info}

type ('id,'data,'info) graph = 
    { nodes : (('id,'data) node) list;  
      edges : (('id,'info) edge) list }

exception Domain of string