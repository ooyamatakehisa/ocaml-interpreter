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

let freevar_ty _ =
  assert false (* Exercise 4.3.1 *)

let string_of_ty _ =
  assert false (* Exercise 4.3.1 *)

let pp_ty ty =
  print_string (string_of_ty ty)
