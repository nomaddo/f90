/* File parser.mly */
%{
open Parse_tree

let mkblock var decl =
  {var_decls = List.rev var; decls = List.rev decl}

let mkvar_decl ?init ?kind typ_str ident =
  let typ = string_to_typ typ_str in
  {name = ident; typ; init; kind }

%}

%token <int> INT
%token PROGRAM
%token DIMENSION POINTER
%token PLUS MINUS MUL DIV
%token LPAREN RPAREN
%token EOF
%token <string> IDENT
%token WHILE DO END
%token COMMA COLON
%token COLCOL
%token IF THEN ELSE
%token PRINT
%token VAR EQ NEQ
%token GREATER LESS GEQ LEQ
%token LBRACE RBRACE

%left PLUS MINUS        /* lowest precedence */
%left MUL DIV           /* medium precedence */
%left EQ GREATER GEQ LESS LEQ
%nonassoc WHILE DO IF THEN ELSE
%nonassoc LPAREN RPAREN LBRACE RBRACE
%nonassoc IDENT INT
%nonassoc SEMI

%start main             /* the entry point */
%type <Parse_tree.block> main

%%

ident:
  IDENT                             { $1 }

main:
  PROGRAM ident top_block END PROGRAM ident EOF { $3 }

top_block:
  seq_var seq_decl                  { mkblock $1 $2 }

block:
  seq_decl                          { $1 }

seq_var:
  /* empty */                       { [] }
| decl_var seq_var                  { $1 :: $2 }

decl_var:
  typ opt_kind COLCOL IDENT EQ exp   { mkvar_decl $1 $4 ~kind:$2 ~init:$6 }
| typ IDENT                          { mkvar_decl $1 $2 }

typ:
  ident                             { $1 }

opt_kind:
  /* empty */                       { [] }
| COMMA kind opt_kind               { $2 :: $3 }

kind:
  POINTER                                  { Pointer }
| DIMENSION LPAREN adecl seq_adecl RPAREN  { Dimension ($3 :: $4)}

adecl:
| exp   { Exp $1 }
| COLON { Colon }

seq_adecl:
  /* empty */           { [] }
| COMMA adecl seq_adecl { $2 :: $3 }

seq_exp:
  /* empty */                       { [] }
| COMMA exp seq_exp                 { $2 :: $3 }

seq_decl:
  /* empty */                       { [] }
| decl seq_decl                     { $1 :: $2 }

decl:
| ident EQ exp                      { Assign ($1, $3) }

exp:
  ident                             { Ident $1 }
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
