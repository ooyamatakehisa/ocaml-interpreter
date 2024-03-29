{
let reservedWords = [
  (* Keywords *)
  ("else", Parser.ELSE);
  ("false", Parser.FALSE);
  ("if", Parser.IF);
  ("then", Parser.THEN);
  ("true", Parser.TRUE);

  (* (ML2)add *)
  ("in", Parser.IN); 
  ("let", Parser.LET);

  (* (ML3)add *)
  ("fun", Parser.FUN);
  ("dfun", Parser.DFUN);

  (* (ML4)add *)
  ("rec", Parser.REC);

  ("and", Parser.LETAND);

]
}

rule main = parse
  (* ignore spacing and newline characters *)
  [' ' '\009' '\012' '\n']+     { main lexbuf }
 | "(*" {comment 0 lexbuf}

| "-"? ['0'-'9']+
    { Parser.INTV (int_of_string (Lexing.lexeme lexbuf)) }

| "(" { Parser.LPAREN }
| ")" { Parser.RPAREN }
| "[" { Parser.LBRACKETS }
| "]" { Parser.RBRACKETS }
| ";;" { Parser.SEMISEMI }
| "::" { Parser.COLCOL}
| ";" { Parser.SEMI}
| "+" { Parser.PLUS }
| "*" { Parser.MULT }
| "<" { Parser.LT }

| "&&" { Parser.AND}
| "||" { Parser.OR}

(* (ML2)add *)
| "=" { Parser.EQ }

(* (ML3)add *)
| "->" { Parser.RARROW }

| ['a'-'z'] ['a'-'z' '0'-'9' '_' '\'']*
    { let id = Lexing.lexeme lexbuf in
      try
        List.assoc id reservedWords
      with
      _ -> Parser.ID id
     }
| eof { exit 0 }

(* コメント専用のルール引数を取ることで入れ子構造を可能にしている。 *)
and comment n = parse
 "*)" {if n = 0 then main lexbuf else comment (n-1) lexbuf}
| "(*" {comment (n+1) lexbuf }
|_  {comment n lexbuf}

