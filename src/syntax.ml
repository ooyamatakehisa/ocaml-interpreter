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
    counter := v + 1; v 
  in body

let rec freevar_ty ty = 
  let lst = MySet.empty in 
  match ty with
  TyFun(ty1,ty2) -> 
    (* let a = MySet.union (freevar_ty ty1) lst in *)
    MySet.union (freevar_ty ty2) (freevar_ty ty1)
  | TyVar a -> MySet.insert a lst
  | _ -> lst 
 

let rec string_of_ty =  function
    TyInt  ->  "int"
  | TyBool ->  "bool"
  | TyFun(a,b) -> "(" ^ (string_of_ty a)^ "->" ^ (string_of_ty b) ^ ")"
  | TyVar a -> "'" ^ Char.escaped(char_of_int (a+97))

let pp_ty  =  function 
  TyInt -> print_string "int" 
  | TyBool -> print_string "bool" 
  (* print_string (string_of_ty ty) *)