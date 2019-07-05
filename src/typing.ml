open Syntax

exception Error of string

(* 型代入の型 *)
type subst = (tyvar * ty) list

let err s = raise (Error s)


(* val subst_type : subst -> ty -> ty 
  例えば， 
  let alpha = fresh_tyvar () in 
  subst_type [(alpha, TyInt)] (TyFun (TyVar alpha, TyBool)) 
  の値は 
  TyFun (TyInt, TyBool) になる *)
let rec subst_type sbst ty1 = match sbst with
  (alpha,ty2)::tl -> (match ty1 with
    TyFun(ty1,ty2) -> TyFun((subst_type sbst ty1),(subst_type sbst ty2))
    | TyVar theta -> 
      if (alpha=theta) then subst_type tl ty2
      else subst_type tl ty1
    | _ -> ty1)
  | _ -> ty1

let rec subst_unify sub x = match x with
  [] -> []
  | (ty1,ty2)::tl -> ((subst_type (sub::[]) ty1),(subst_type (sub::[]) ty2))::(subst_unify sub tl )

(* 単一化の実装 *)
let rec unify tylst = match tylst with
  [] -> []
  | (ty1,ty2)::tl -> 
    if(ty1=ty2) then unify tl 
    else match (ty1,ty2) with 
      (TyFun(ty1,ty2),TyFun(ty3,ty4)) -> unify ((ty1,ty3)::(ty2,ty4)::tl)
      | (TyVar a,ty) -> 
        if (MySet.member a (freevar_ty ty)) then err "can't unify"
        else (a,ty)::(unify (subst_unify (a,ty) tl))
      |(ty,TyVar a) -> 
        if (MySet.member a (freevar_ty ty)) then err "can't unify"
        else (a,ty)::(unify (subst_unify (a,ty) tl))
      | _ -> err "can't unify"
    
(* eqs_of_subst : subst -> (ty * ty) list 
  型代入を型の等式集合に変換 *) 
let rec eqs_of_subst s = match s with
  [] -> []
  | (a,ty)::tl -> (TyVar a,ty)::(eqs_of_subst tl)

(* subst_eqs: subst -> (ty * ty) list -> (ty * ty) list 
  型の等式集合に型代入を適用 *) 
let rec subst_eqs s eqs = match s with
  [] -> []
  | (a,ty)::tl -> (subst_unify (a,ty) eqs)::(subst_eqs tl eqs)




(* Type Environment *) 
type tyenv = ty Environment.t
let ty_prim op ty1 ty2 = match op with 
  Plus ->  ([(ty1, TyInt); (ty2, TyInt)], TyInt) 
  | Mult -> ([(ty1, TyInt); (ty2, TyInt)], TyInt) 
  | Lt ->  ([(ty1, TyInt); (ty2, TyInt)], TyBool) 
  | Or -> ([(ty1, TyBool); (ty2, TyBool)], TyBool) 
  | And -> ([(ty1, TyBool); (ty2, TyBool)], TyBool) 
  (* | Cons -> err "Not Implemented!" *)

let rec ty_exp tyenv = function 
  Var x -> (try ([],Environment.lookup x tyenv) with 
    Environment.Not_bound -> err ("variable not bound: " ^ x)) 
    | ILit _ -> ([],TyInt) 
    | BLit _ -> ([],TyBool) 
    | BinOp (op, exp1, exp2) -> 
      let  (s1, ty1)  = ty_exp tyenv exp1 in 
      let  (s2, ty2)  = ty_exp tyenv exp2 in 
      let (eqs3, ty) = ty_prim op ty1 ty2 in 
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ eqs3 in 
      let s3 = unify eqs in 
      (s3, subst_type s3 ty) 
    | IfExp (exp1, exp2, exp3) -> 
      let  (s1, ty1)  = ty_exp tyenv exp1 in 
      let  (s2, ty2)  = ty_exp tyenv exp2 in 
      let  (s3, ty3)  = ty_exp tyenv exp3 in 
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ (eqs_of_subst s3) @ [(ty1,TyBool);(ty2,ty3)] in
      let s4 = unify eqs 
      in (s4, subst_type s4 ty2) 
    | LetExp (id, exp1, exp2) -> 
      let  (s1, ty1)  = ty_exp tyenv exp1 in
      let newenv = Environment.extend id ty1 tyenv in
      let  (s2, ty2)  = ty_exp newenv exp2 in 
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) in 
      let s4 = unify eqs 
      in (s4, subst_type s4 ty2) 
    | FunExp (id, exp) -> 
      let domty = TyVar (fresh_tyvar ()) in 
      let s, ranty = 
        ty_exp (Environment.extend id domty tyenv) exp in 
        (s, TyFun (subst_type s domty, ranty)) 
    | AppExp (exp1, exp2) -> 
      let domty1 = TyVar (fresh_tyvar ()) in 
      let domty2 = TyVar (fresh_tyvar ()) in 
      let  (s1, ty1)  = ty_exp tyenv exp1 in 
      let  (s2, ty2)  = ty_exp tyenv exp2 in   
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ [(TyFun(domty1,domty2),ty1);(domty1,ty2)] in 
      let s4 = unify eqs 
      in (s4, subst_type s4 domty2)
    | _ -> err ("Not Implemented!")

  



let ty_decl tyenv = function 
  Exp e -> ty_exp tyenv e 
  | Decl (id, e) -> ty_exp tyenv e 
  | _ -> err ("Not Implemented!")



