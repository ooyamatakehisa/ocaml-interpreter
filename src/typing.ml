open Syntax

exception Error of string

let err s = raise (Error s)

(* Type Environment *) 
type tyenv = ty Environment.t
let ty_prim op ty1 ty2 = match op with 
  Plus -> (match ty1, ty2 with 
    TyInt, TyInt -> TyInt 
    | _ -> err ("Argument must be of integer: +")) 
  | Mult -> (match ty1, ty2 with 
    TyInt, TyInt -> TyInt 
    | _ -> err ("Argument must be of integer: *")) 
  | Lt ->  (match ty1, ty2 with 
    TyInt, TyInt -> TyBool
    | _ -> err ("Argument must be of integer: <")) 
  | Or -> (match ty1, ty2 with 
    TyBool, TyBool -> TyInt 
    | _ -> err ("Argument must be of boolean: ||"))
  | And -> (match ty1, ty2 with 
    TyBool, TyBool -> TyInt 
    | _ -> err ("Argument must be of boolean: &&"))
  (* | Cons -> err "Not Implemented!" *)

let rec ty_exp tyenv = function 
  Var x -> (try Environment.lookup x tyenv with 
    Environment.Not_bound -> err ("variable not bound: " ^ x)) 
    | ILit _ -> TyInt 
    | BLit _ -> TyBool 
    | BinOp (op, exp1, exp2) -> 
      let tyarg1 = ty_exp tyenv exp1 in 
      let tyarg2 = ty_exp tyenv exp2 in 
      ty_prim op tyarg1 tyarg2 
    | IfExp (exp1, exp2, exp3) -> (match (ty_exp tyenv exp1) with
      TyBool ->
        if ((ty_exp tyenv exp2)=(ty_exp tyenv exp3)) then
        ty_exp tyenv exp2
        else err ("the result of an if-statement must be same type. ")
      | _ -> err ("the condition of an if-statement must be type bool. "))
    | LetExp (id, exp1, exp2) -> 
      let typ = ty_exp tyenv exp1 in
      let newenv = Environment.extend id typ tyenv in
       ty_exp newenv exp2
    (* | FunExp (id, exp) ->  *)
     
    | _ -> err ("Not Implemented!")


let ty_decl tyenv = function 
  Exp e -> ty_exp tyenv e 
  | Decl (id, e) -> ty_exp tyenv e 
  | _ -> err ("Not Implemented!")



let subst_type _ =
  assert false (* Exercise 4.3.2 *)

let unify _ =
  assert false (*Exercise 4.3.3*)



