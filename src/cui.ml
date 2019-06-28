open Eval


let rec read_eval_print env =
  print_string "# ";
  flush stdout;
  let back message = Printf.printf "%s" message;
    print_newline();
    read_eval_print env in
  (try
      let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
      let rec printrec dec = 
      match dec with 
        Syntax.VarDecl(x,e1,top) -> 
            let (Syntax.VarDecl(id1,e,top)) = dec in
            let (id, newenv, v) = eval_decl env dec in
            Printf.printf "val %s = " id1;
            pp_val (Environment.lookup id1 newenv);
            print_newline();
            printrec top;
            read_eval_print newenv
        
        | Syntax.LetAndDecl(x,e1,top) ->
          let (Syntax.LetAndDecl(id1,e,top)) = dec in
            let (id, newenv, v) = eval_decl env dec in
            Printf.printf "val %s = " id1;
            pp_val (Environment.lookup id1 newenv);
            print_newline();
            printrec top;
            read_eval_print newenv
        |_ -> 
        let (id, newenv, v) = eval_decl env decl in
          Printf.printf "val %s = " id;
          pp_val v;
          print_newline();
          read_eval_print newenv
      in
      printrec decl
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
