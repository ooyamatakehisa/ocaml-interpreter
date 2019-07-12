open Eval
open Typing
open Syntax


let rec read_eval_print env tyenv=
  print_string "# ";
  flush stdout;
  let back message = 
    Printf.printf "%s" message;
    print_newline();
    read_eval_print env tyenv in
    
    (*ここでtry-with文を書くことで例外処理ができる*)
    (try 
      let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
      let (s,ty) = ty_decl tyenv decl in 
      
      (*letの連続複数回宣言やletをandで結んだ宣言などの識別子の束縛値の表示のために値を出力する部分を再帰関数として定義*)
      let rec printrec dec = 
      match dec with 
       (*let a=3 let b=4;;のような宣言ために場合分け*)
        Syntax.VarDecl(x,e1,top) -> 
            let (newtyenv, ty) = ty_decl tyenv decl in 
            let (id, newenv, v) = eval_decl env dec in
            (*ひとつ目のlet宣言の識別子がx *)
            Printf.printf "val %s : " x; 
            pp_ty ty; 
            print_string " = "; 
            (*そのxに束縛されている値を環境で探し出力*)
            pp_val (Environment.lookup x newenv); 
            print_newline();
            (*残りのlet宣言のdeclに再帰的にこのprintrec関数を適用*) 
            printrec top;
            read_eval_print newenv newtyenv

        (*let a=3 and b=4;;のような宣言ために場合分け*)
        | Syntax.LetAndDecl(x,e1,top) ->
            let (newtyenv, ty) = ty_decl tyenv decl in 
            let (id, newenv, v) = eval_decl env dec in
            (*ひとつ目のlet宣言の識別子がx *)
            Printf.printf "val %s : " x; 
            pp_ty ty; 
            print_string " = "; 
            (*そのxに束縛されている値を環境で探し出力*)
            pp_val (Environment.lookup x newenv); 
            print_newline();
            (*残りのandで宣言された部分にに再帰的にこのprintrec関数を適*) 
            printrec top;
            read_eval_print newenv newtyenv

        (*上のdecがLetAndDecl(x,e1,top)のときのtopの部分はlet a=3 and b=4におけるb=4のような形になっており
        LetAndRecExp(x,e,top)になっているのでその時の出力表示用に場合分け *)
        | LetAndRecExp(x,e,top) -> 
          let (newtyenv, ty) = ty_decl tyenv decl in 
          let (id, newenv, v) = eval_decl env dec in
            (*ひとつ目のlet宣言の識別子がx *)
            Printf.printf "val %s : " x; 
            pp_ty ty; 
            print_string " = "; 
            (*そのxに束縛されている値を環境で探し出力*)
            pp_val (Environment.lookup x newenv); 
            print_newline();
            (*残りのandで宣言された部分にに再帰的にこのprintrec関数を適用*) 
            printrec top;
            read_eval_print newenv newtyenv


        (*decが普通の宣言のときの残りのケース*)
        |_ -> 
          let (id, newenv, v) = eval_decl env decl in
          let (newtyenv, ty) = ty_decl tyenv decl in 
          Printf.printf "val %s : " id;
          pp_ty ty; 
          print_string " = "; 
          pp_val v;
          print_newline();
          read_eval_print newenv newtyenv
      in
      printrec decl
    (*以下が例外発生時の処理文。ただしエラーをはき再び入力を受け付けるという一連の動作をbackという関数で定義してエラー発生時に再帰的に呼び出している。*)
    with Failure message -> back message
    | Parser.Error -> back "Parser Error"
    | Eval.Error message -> back message
    | _ -> back "Error") 
       
 

let initial_env =
  Environment.extend "i" (IntV 1)
    (Environment.extend "v" (IntV 5)
       (Environment.extend "x" (IntV 10)
          (Environment.extend "ii" (IntV 2)
            (Environment.extend "iii" (IntV 3)
              (Environment.extend "iv" (IntV 4) Environment.empty)))))

let initial_tyenv = 
  Environment.extend "i" (TyScheme([],TyInt))
    (Environment.extend "v"  (TyScheme([],TyInt)) 
      (Environment.extend "x" (TyScheme([], TyInt)) Environment.empty))