open Syntax

type exval =
    IntV of int
  | BoolV of bool
  (* (ML3)add *)
  | ProcV of id * exp * dnval Environment.t ref
  | DProcV of id * exp 
and dnval = exval

exception Error of string

let err s = raise (Error s)

(* pretty printing *)
let rec string_of_exval = function
    IntV i -> string_of_int i
  | BoolV b -> string_of_bool b

let pp_val v = print_string (string_of_exval v)

let rec apply_prim op arg1 arg2 = match op, arg1, arg2 with
    Plus, IntV i1, IntV i2 -> IntV (i1 + i2)
  | Plus, _, _ -> err ("Both arguments must be integer: +")
  | Mult, IntV i1, IntV i2 -> IntV (i1 * i2)
  | Mult, _, _ -> err ("Both arguments must be integer: *")
  | Lt, IntV i1, IntV i2 -> BoolV (i1 < i2)
  | Lt, _, _ -> err ("Both arguments must be integer: <")
  (* 以下、&&と||のケースの演算適用結果の場合 *)
  | Or, BoolV i1, BoolV i2 -> BoolV (i1 || i2) 
  | Or, BoolV i1 ,_ -> if i1 = true then BoolV true 
    else err("Another arguments must be boolean: ||")
  | Or, _, BoolV i1  -> if i1 = true then BoolV true 
    else err("Another arguments must be boolean: ||")
  | Or, _,_ -> err ("Both arguments must be boolean: ||")
  | And, BoolV i1, BoolV i2 -> BoolV (i1 && i2)
  | And, _,_ -> err ("Both arguments must be boolean: &&")

let a = ref []

let rec eval_exp env = function
    Var x ->
    (try Environment.lookup x env with
       Environment.Not_bound -> err ("Variable not bound: " ^ x))
  | ILit i -> IntV i
  | BLit b -> BoolV b
  | BinOp (op, exp1, exp2) ->
    (* &&と||の片方の引数がundefでなおかつもう片方の引数がそれぞれfalseとtrueだった場合の対処 *)
    if ((op=Or)&&((exp1=BLit true)||(exp2=BLit true))) then BoolV true 
    else if((op=And)&&((exp1=BLit false)||(exp2=BLit false))) then BoolV false
    else 
    let arg1 = eval_exp env exp1 in
    let arg2 = eval_exp env exp2 in
    apply_prim op arg1 arg2
  | IfExp (exp1, exp2, exp3) ->
    let test = eval_exp env exp1 in
    (match test with
       BoolV true -> eval_exp env exp2
     | BoolV false -> eval_exp env exp3
     | _ -> err ("Test expression must be boolean: if"))

  (* (ML2)add *)
  | LetExp (id, exp1, exp2) -> 
    let value = eval_exp env exp1 in 
    eval_exp (Environment.extend id value env) exp2

  | FunExp (id, exp) -> 
    let env' = ref env in 
    ProcV (id, exp, env') 
  | AppExp (exp1, exp2) ->  
      let funval = eval_exp env exp1 in 
      let arg = eval_exp env exp2 in 
        (match funval with 
          ProcV (id, body, env') -> 
          (* クロージャ内の環境を取り出して仮引数に対する束縛で拡張 *) 
            let newenv = Environment.extend id arg !env' in 
            eval_exp newenv body 
          (* DFunのための関数閉包、環境の情報を保持しないためこのクロージャが持つ値は2つ *)
          | DProcV(id,body) -> let newenv = Environment.extend id arg env in
            (* 関数宣言時の環境ではなく関数適用時の環境で評価 *)
            eval_exp newenv body 
          | _ -> err ("Non-function value is applied"))
  | LetRecExp (id, para, exp1, exp2) -> 
    (* ダミーの環境への参照を作る *) 
    let dummyenv = ref Environment.empty in 
    (* 関数閉包を作り，idをこの関数閉包に写像するように現在の環境envを拡張 *) 
    let newenv = Environment.extend id (ProcV (para, exp1, dummyenv)) env in 
    (* ダミーの環境への参照に，拡張された環境を破壊的代入してバックパッチ *) 
    dummyenv := newenv ;
    eval_exp newenv exp2
  | DFunExp (id ,exp) -> DProcV (id,exp)

  (* let a=3 and b=3 in a+bのような文の評価の場合分け *)
  | LetAndInExp((LetAndRecExp(x,e1,e2)),e3) -> 
    a := [];
    (* 上の例のlet a=3 and b=3の部分はLetAndRecExp(x,e1,e2)型なのでその部分はeval_declで評価（相互再帰） *)
    let (x ,newenv,newv) = eval_decl env (LetAndRecExp(x,e1,e2)) in
    (* そのeval_declで拡張した環境のもとでin以下を評価 *)
    eval_exp newenv e3

  (* 中置演算子の評価 *)    
  | MidMultExp ->
    let env' = ref env in 
    (* 適当にxとyを識別子としてx*yを返す関数を自分で作ってそれを評価結果とする。 *)
    ProcV("x",FunExp("y",(BinOp(Mult,(Var "x"),(Var "y")))),env')
  | MidPlusExp ->
    let env' = ref env in 
    (* 適当にxとyを識別子としてx+yを返す関数を自分で作ってそれを評価結果とする。 *)
    ProcV("x",FunExp("y",(BinOp(Plus,(Var "x"),(Var "y")))),env')
  
  (* 評価するexpがどれにも当てはまら買ったらエラー *)
  |_ -> err "no pattern matching"
  

and eval_decl env decl= match decl with
  
    Exp e -> let v = eval_exp env e in ("-", env, v)
    | Decl (id, e) -> let v = eval_exp env e in (id, Environment.extend id v env, v)
    | RecDecl (id, para, exp) ->
      let dummyenv = ref Environment.empty in
      let newenv = Environment.extend id (ProcV (para, exp, dummyenv)) env in
      dummyenv := newenv;
      (id, newenv,  (ProcV (para, exp, dummyenv)))
    | VarDecl (id ,e, top) -> 
      let v = eval_exp env e in 
      eval_decl (Environment.extend id v env) top
    | LetAndDecl (id, e, top) -> 
      a := id::[] ;  
      let firstenv = env in
      let v = eval_exp firstenv e in
      let (x ,newenv,newv) = eval_decl firstenv top in
      (x,(Environment.extend id v newenv),newv) 
    | LetAndRecExp(id,e,top) -> 
      if (List.mem id !a) then  failwith "error"
      else 
      a := id::(!a) ;
      let firstenv = env in
      let v = eval_exp firstenv e in
      let (x ,newenv,newv) = eval_decl firstenv top in
      (x,(Environment.extend id v newenv ),newv)  
      
    | LetOneExp(id,e) -> 
      if (List.mem id !a) then  failwith "error"
      else 
      let v = eval_exp env e in
       (id,(Environment.extend id v env ),v) 
    
      


