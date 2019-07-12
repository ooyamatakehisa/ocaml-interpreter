(* ML interpreter / type reconstruction *)
type id = string

type binOp = Plus | Mult | Lt | And | Or

type exp =
    Var of id
  | ILit of int
  | BLit of bool
  | BinOp of binOp * exp * exp
  | MidPlusExp
  | MidMultExp
  | IfExp of exp * exp * exp
  | LetExp of id * exp * exp

  (* (ML3)add *)
  | FunExp of id * exp
  | DFunExp of id * exp
  | AppExp of exp * exp

  (* (ML4)add *)
  | LetRecExp of id * id * exp * exp
  | LetAndInExp of program * exp

  (* | ListContAeExp of exp * exp
  | ListContLiExp of exp * exp
  | NilExp *)




and program =
    Exp of exp
    | Decl of id * exp
    | RecDecl of id * id * exp
    | VarDecl of id * exp * program
    | LetAndDecl of id * exp * program
    | LetAndRecExp of id * exp * program
    | LetOneExp of id * exp


type tyvar = int
type ty =
    TyInt
  | TyBool
  | TyVar of tyvar
  | TyFun of ty * ty
  | TyList of ty

let fresh_tyvar = 
  let counter = ref 0 in 
  let body () = 
    let v = !counter in 
    counter := v + 1; 
    v 
  in body

type tysc = TyScheme of tyvar list * ty
let tysc_of_ty ty = TyScheme ([], ty)


(* 与えられた型中の型変数(int)の集合を返す関数 *)
let rec freevar_ty ty = 
  let lst = MySet.empty in 
  match ty with
  TyFun(ty1,ty2) -> 
    MySet.union (freevar_ty ty2) (freevar_ty ty1)
  | TyVar a -> MySet.insert a lst
  | _ -> lst 

  let freevar_tysc tysc = 
  let TyScheme(lst,ty1) = tysc in
  let tyvarlst = freevar_ty ty1 in
  MySet.diff tyvarlst (MySet.from_list lst ) 

 
(* 型変数のintを引数に型変数の記号を返す関数 *)
let char_tyvar a = 
    let c = a/26 in
    if(c=0) then 
    "'" ^ Char.escaped(char_of_int (a+97-26*c)) 
    else
      "'" ^ Char.escaped(char_of_int (a+97-26*c)) ^ (string_of_int c)

let rec string_of_ty =  function
    TyInt  ->  "int"
  | TyBool ->  "bool"
  | TyFun(a,b) -> "(" ^ (string_of_ty a)^ " -> " ^ (string_of_ty b) ^ ")"
  | TyVar a -> char_tyvar a
  | _ -> "others"

let pp_ty ty  = print_string  (string_of_ty ty )

  
