/* File parser.mly */
%{
open Parse_tree

let mkblock var decl =
  {var_decls = List.rev var; decls = List.rev decl}

let mkvar_decl name exp =
  {name; initialize = exp}

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
%type <Parse_tree.block> main

%%

ident:
  IDENT                             { Ident $1 }

main:
  top EOF                           { $1 }

top:
  BEGIN block                       { $2 }

block:
  LBRACE seq_var seq_decl RBRACE    { mkblock $2 $3 }
| decl                              { mkblock [] [$1] }

seq_var:
  /* empty */                       { [] }
| decl_var seq_var                  { $1::$2 }

decl_var:
  VAR IDENT EQ exp SEMI             { mkvar_decl $2 $4 }

seq_decl:
  /* empty */                       { [] }
| decl seq_decl                     { $1 :: $2 }

decl:
| IF exp THEN block if_continue     { If ($2, $4, $5) }
| ident ASSIGN exp SEMI             { Assign ($1, $3) }
| WHILE exp DO block SEMI           { While ($2, $4) }
| PRINT exp SEMI                    { Print $2 }

if_continue:
  /* empty */                       { None }
| ELSE block                        { Some $2}

exp:
  ident                             { $1 }
| INT                               { Const (Int $1) }
| MINUS INT                         { Const (Int (- $2)) }
| arith                             { $1 }
| comp                              { $1 }

comp:
| exp EQ exp                        { Eq ($1, $3) }
| exp GREATER exp                   { Less ($3, $1) }
| exp GEQ exp                       { Leq ($3, $1) }
| exp LESS exp                      { Less ($1, $3) }
| exp LEQ exp                       { Leq ($1, $3) }
| exp NEQ exp                       { Neq ($1, $3) }

arith:
| LPAREN exp RPAREN                 { $2 }
| exp PLUS exp                      { Plus ($1, $3) }
| exp MINUS exp                     { Minus ($1, $3) }
| exp MUL exp                       { Mul ($1, $3) }
| exp DIV exp                       { Div ($1, $3) }
