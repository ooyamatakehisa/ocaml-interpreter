type 'a t

exception Not_bound

val empty : 'a t
val to_list :   'a t  -> (Syntax.id* 'a) list 
val from_list : (Syntax.id* 'a)list -> 'a t  
val extend : Syntax.id -> 'a -> 'a t -> 'a t
val lookup : Syntax.id -> 'a t -> 'a
val map : ('a -> 'b) -> 'a t -> 'b t
val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
val find :  Syntax.id -> 'a t -> bool
