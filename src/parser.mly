%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token LBRACKETS RBRACKETS COLCOL SEMI
%token PLUS MULT LT AND OR
%token IF THEN ELSE TRUE FALSE

/* (ML2)add */
%token LET IN EQ LETAND

/* (ML3)add */
%token RARROW FUN DFUN MIDFUN

%token REC

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    e=Expr SEMISEMI { Exp e }

    /* (ML2)add */
  | LET x=ID EQ e=Expr SEMISEMI { Decl (x, e) }
  | LET x=ID EQ e=Expr top=toplevel {VarDecl(x,e,top)}
  | LET REC f=ID EQ FUN x=ID RARROW e=Expr SEMISEMI {RecDecl(f,x,e)}
  | LET REC f=ID EQ DFUN x=ID RARROW e=Expr SEMISEMI {RecDecl(f,x,e)}
  | LET x=ID EQ e1=Expr LETAND e2=LetAndExpr SEMISEMI { LetAndDecl (x, e1, e2) }



Expr :

  /* (ML2)add */
  | e=LetExpr { e }

  /* (ML3)add  */
  | e=FunExpr { e }
  | e=DFunExpr { e }
  | e=ORExpr { e }
  | e=LetAndInExp { e }
  /* | e=ListExpr{ e } */

LetAndInExp :
  LET x=ID EQ e1=Expr LETAND e2=LetAndExpr IN e3=Expr { LetAndInExp((LetAndRecExp(x,e1,e2)),e3) }


/* (ML2)add */
LetExpr :
  LET x=ID EQ e1=Expr IN e2=Expr { LetExp (x, e1, e2) }
  | LET REC x=ID EQ FUN y=ID RARROW e1=Expr IN e2=Expr { LetRecExp (x, y, e1, e2) }
  | LET REC x=ID EQ DFUN y=ID RARROW e1=Expr IN e2=Expr { LetRecExp (x, y, e1, e2) }



LetAndExpr :
  x=ID EQ e1=Expr LETAND e2=LetAndExpr { LetAndRecExp(x,e1,e2) }
  | x=ID EQ e=Expr {LetOneExp(x,e)}



ORExpr :
   l=ANDExpr OR r=ORExpr { BinOp (Or,l,r) }
  /* | l=AExpr OR r=ORExpr { BinOp (Or,l,r) }
  | l=ORExpr OR r=AExpr { BinOp (Or,l,r) } */
  | e=ANDExpr { e }

ANDExpr :
   l=LTExpr AND r=ANDExpr { BinOp (And,l,r) }
  /* | l=LTExpr AND r=AExpr { BinOp (And,l,r) }
  | l=AExpr AND r=LTExpr { BinOp (And,l,r) }
  | l=LTExpr AND r=LTExpr { BinOp (And,l,r) } */
  | e=LTExpr { e }

LTExpr :
  l=PExpr LT r=PExpr { BinOp (Lt, l, r) }
  | e=PExpr { e }

PExpr :
  l=PExpr PLUS r=MExpr { BinOp (Plus, l, r) }
  | e=MExpr { e }

/* (ML3)instead above one */
MExpr :
  e1=MExpr MULT e2=AppExpr { BinOp (Mult, e1, e2) }
  | e=AppExpr { e }

/*
ListExpr :
  LBRACKETS e=ListContExpr RBRACKETS { e }
  | LBRACKETS RBRACKETS { NilExp }

ListContExpr:
e1=AExpr SEMI e2=ListContExpr { ListContAeExp(e1,e2)}
| e1=ListExpr SEMI e2=ListContExpr { ListContLiExp(e1,e2)}
| e=AExpr { e }
| e=ListExpr { e } */


FunExpr :
FUN e1=ID RARROW e2=Expr { FunExp (e1, e2) }


DFunExpr :
DFUN e1=ID RARROW e2=Expr { DFunExp (e1, e2) }



AExpr :
  i=INTV { ILit i }
| TRUE   { BLit true }
| FALSE  { BLit false }
| i=ID   { Var i }
| LPAREN e=Expr RPAREN { e }
| LPAREN PLUS RPAREN  { MidPlusExp }
| LPAREN MULT RPAREN  { MidMultExp }
| e=IfExpr { e }


  /* (ML3)add */
AppExpr :
  e1=AppExpr e2=AExpr { AppExp (e1, e2) }
  | e=AExpr { e }


IfExpr :
    IF c=Expr THEN t=Expr ELSE e=Expr { IfExp (c, t, e) }
