/* File parser.mly */
%{
open Parse_tree

%}

%token <int> INT
%token PLUS MINUS MUL DIV
%token LPAREN RPAREN
%token EOF BEGIN
%token <string> IDENT
%token SEMI
%token WHILE DO
%token ASSIGN
%token IF THEN ELSE
%token PRINT
%token VAR EQ NEQ
%token GREATER LESS GEQ LEQ
%token LBRACE RBRACE
%nonassoc ASSIGN

%left PLUS MINUS        /* lowest precedence */
%left MUL DIV           /* medium precedence */
%left EQ GREATER GEQ LESS LEQ
%nonassoc VAR WHILE DO IF THEN ELSE
%nonassoc LPAREN RPAREN LBRACE RBRACE
%nonassoc IDENT INT
%nonassoc SEMI

%start main             /* the entry point */
%type <Parse_tree.expr> main

%%
main:
  top EOF                           { $1 }

top:
  BEGIN block                       { $2 }

block:
  LBRACE seq_decl RBRACE            { $2 }
| decl                              { $1 }

seq_decl:
  decl seq_decl                     { Seq ($1, $2) }
| decl                              { $1 }

decl:
  VAR ident EQ exp SEMI             { Var ($2, $4) }
| IF exp THEN block ELSE block SEMI { If ($2, $4, $6) }
| ident ASSIGN exp SEMI             { Assign ($1, $3) }
| WHILE exp DO block SEMI      { While ($2, $4) }
| PRINT exp SEMI                    { Print $2 }

exp:
  ident                             { $1 }
| INT                               { Const (Int $1) }
| MINUS INT                         { Const (Int (- $2)) }
| arith                             { $1 }
| comp                              { $1 }

comp:
| exp EQ exp                        { Eq ($1, $3) }
| exp GREATER exp                   { Greater ($1, $3) }
| exp GEQ exp                       { Geq ($1, $3) }
| exp LESS exp                      { Less ($1, $3) }
| exp LEQ exp                       { Leq ($1, $3) }
| exp NEQ exp                       { Neq ($1, $3) }

arith:
| LPAREN exp RPAREN                 { $2 }
| exp PLUS exp                      { Plus ($1, $3) }
| exp MINUS exp                     { Minus ($1, $3) }
| exp MUL exp                       { Mul ($1, $3) }
| exp DIV exp                       { Div ($1, $3) }

ident:
  IDENT                             { Ident $1 }
