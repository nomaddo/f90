/* File parser.mly */
%{
open Parse_tree
open Location

let mkblock var decl =
  {vardecls = List.rev var; decls = List.rev decl}

let mktyp ~loc typ_desc =
  { typ_desc; typ_loc = loc}

let mkvar_decl ?init ?kind typ ident =
  let loc = mkloc () in
  let vardecl_desc = {name = ident; typ; init; kind} in
  {vardecl_desc; vardecl_loc = loc}

let mkkind ~loc kind =
  {kind_desc=kind; kind_loc=loc}

let mkexp ~loc exp =
  {expr_loc = loc; expr_desc=exp}

let mkconst ~loc con =
  {const_desc = con; const_loc = loc}

let mkdim_param ~loc param =
  {dim_param_desc=param; dim_param_loc = loc}

let mkdecl ~loc dec =
  {decl_desc = dec; decl_loc = loc}

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
%token REAL INTEGER
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

main:
  PROGRAM IDENT top_block END PROGRAM IDENT EOF
  { assert ($2 = $6); $3 }

top_block:
  seq_var seq_decl                  { mkblock $1 $2 }

seq_var:
  /* empty */                       { [] }
| decl_var seq_var                  { $1 :: $2 }

decl_var:
  typ opt_kind COLCOL IDENT EQ exp   { mkvar_decl $1 $4 ~kind:$2 ~init:$6 }
| typ opt_kind COLCOL IDENT          { mkvar_decl $1 $4 ~kind:$2 }
| typ IDENT                          { mkvar_decl $1 $2 }

typ:
  INTEGER                            { mktyp ~loc:(mkloc ()) Integer }
| REAL                               { mktyp ~loc:(mkloc ()) Real }

opt_kind:
  /* empty */                       { [] }
| COMMA kind opt_kind               { $2 :: $3 }

kind:
  POINTER
  { mkkind ~loc:(mkloc ()) Pointer }
| DIMENSION LPAREN adecl seq_adecl RPAREN
  { mkkind ~loc:(mkloc ()) (Dimension ($3 :: $4)) }

adecl:
| exp   { mkdim_param ~loc:(mkloc ()) (Exp $1) }
| COLON { mkdim_param ~loc:(mkloc ()) Colon }

seq_adecl:
  /* empty */           { [] }
| COMMA adecl seq_adecl { $2 :: $3 }

seq_decl:
  /* empty */                       { [] }
| decl seq_decl                     { $1 :: $2 }

decl:
| IDENT EQ exp
  { mkdecl ~loc:(mkloc ()) (Assign ($1, $3)) }

exp:
  IDENT
  { mkexp ~loc:(mkloc ()) (Ident $1) }
| INT
  { mkexp ~loc:(mkloc ()) (Const (mkconst ~loc:(mkloc ()) (Int $1))) }
| MINUS INT
  { mkexp ~loc:(mkloc ()) (Const (mkconst ~loc:(mkloc ()) (Int (- $2)))) }
| MINUS exp
  { mkexp ~loc:(mkloc ()) (Rev $2) }
| arith
  { mkexp ~loc:(mkloc ()) $1 }
| comp
  { mkexp ~loc:(mkloc ()) $1 }
| LPAREN exp RPAREN
  { $2 }

comp:
| exp EQ exp                        { Eq ($1, $3) }
| exp GREATER exp                   { Less ($3, $1) }
| exp GEQ exp                       { Leq ($3, $1) }
| exp LESS exp                      { Less ($1, $3) }
| exp LEQ exp                       { Leq ($1, $3) }
| exp NEQ exp                       { Neq ($1, $3) }

arith:
| exp PLUS exp                      { Plus ($1, $3) }
| exp MINUS exp                     { Minus ($1, $3) }
| exp MUL exp                       { Mul ($1, $3) }
| exp DIV exp                       { Div ($1, $3) }
