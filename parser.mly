%{
open Parse_tree
open Location

let mkblock var decl =
  {vardecls = var; decls = decl}

let mktyp ~loc typ_desc =
  { typ_desc; typ_loc = loc}

let mkvar_decl ?init ?kind typ ident =
  let loc = mkloc () in
  let vardecl_desc = {name = ident; typ; init; kind} in
  {vardecl_desc; vardecl_loc = loc}

let mkkind ~loc kind =
  {kind_desc = kind; kind_loc = loc}

let mkexp ~loc exp =
  {expr_loc = loc; expr_desc = exp; expr_typ = ()}

let mkconst ~loc con =
  {const_desc = con; const_loc = loc}

let mkdim_param ~loc param =
  {dim_param_desc = param; dim_param_loc = loc}

let mkdecl ~loc dec =
  {decl_desc = dec; decl_loc = loc}

let mkrange ~loc range_left range_right =
  { range_left; range_right; range_loc = loc}

let mkcase ~loc case_option  case_decls =
  { case_option ; case_decls; case_loc = loc }

let mkselect ~loc select_expr select_cases =
  { select_expr; select_cases; select_loc = loc }

let mksub ~loc sub =
  { sub_subprogram = sub; sub_loc = loc }

let mksubroutine ~loc ident args decls =
  { sub_name = ident; sub_args = args; sub_decls = decls }

let mkfunc ~loc ident args decls =
  { func_name = ident; func_args = args; func_decls = decls }

%}

%token <int> INT
%token <string> FLOAT
%token TRUE FALSE
%token PROGRAM CONTAINS
%token DIMENSION POINTER ALLOCATABLE PARAMETER
%token PLUS MINUS MUL DIV
%token NOT AND OR EQ NEQ
%token LPAREN RPAREN
%token EOF
%token <string> IDENT
%token WHILE DO END
%token COMMA COLON
%token COLCOL
%token IF THEN ELSE
%token CALL
%token VAR EQV NEQV
%token GREATER LESS GEQ LEQ
%token REAL INTEGER LOGICAL COMPLEX DOUBLE PRECISION
%token LBRACE RBRACE
%token SELECT CASE DEFAULT
%token SUBROUTINE FUNCTION

%left PLUS MINUS        /* lowest precedence */
%left MUL DIV           /* medium precedence */
%left EQ GREATER GEQ LESS LEQ
%left NOT AND OR EQV NEQV
%nonassoc WHILE DO IF THEN ELSE
%nonassoc LPAREN RPAREN LBRACE RBRACE
%nonassoc IDENT INT FLOAT

%start main             /* the entry point */
%type <unit Parse_tree.main> main

%%

main:
| PROGRAM IDENT top_block CONTAINS seq_subprogram END PROGRAM ident_or_blank EOF
  { assert ($2 = $8); {program = $3; subprograms = $5} }
| PROGRAM IDENT top_block END PROGRAM IDENT EOF
  { assert ($2 = $6); {program = $3; subprograms = []} }

ident_or_blank:
| /* empty */    { "empty" }
| IDENT          { $1 }

seq_subprogram:
| /* empty */       { [] }
| subprogram seq_subprogram { $1 :: $2 }

subprogram:
| SUBROUTINE IDENT LPAREN seq_ident RPAREN seq_decl END SUBROUTINE ident_or_blank
  { mksub ~loc:(mkloc ()) (Subroutine (mksubroutine ~loc:(mkloc ()) $2 $4 $6)) }
| FUNCTION IDENT LPAREN seq_ident RPAREN seq_decl END FUNCTION ident_or_blank
  { mksub ~loc:(mkloc ()) (Function (mkfunc ~loc:(mkloc ()) $2 $4 $6)) }

seq_ident:
| /* empty */           { [] }
| IDENT COMMA seq_ident { $1 :: $3 }
| IDENT                 { [$1] }

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
  INTEGER                            { mktyp ~loc:(mkloc ()) Tinteger }
| REAL                               { mktyp ~loc:(mkloc ()) Treal    }
| COMPLEX                            { mktyp ~loc:(mkloc ()) Tcomplex }
| LOGICAL                            { mktyp ~loc:(mkloc ()) Tlogical }
| DOUBLE PRECISION                   { mktyp ~loc:(mkloc ()) Tdouble  }

opt_kind:
  /* empty */                       { [] }
| COMMA kind opt_kind               { $2 :: $3 }

kind:
  POINTER
  { mkkind ~loc:(mkloc ()) Pointer }
| DIMENSION LPAREN adecl seq_adecl RPAREN
  { mkkind ~loc:(mkloc ()) (Dimension ($3 :: $4)) }
| ALLOCATABLE
  { mkkind ~loc:(mkloc ()) Allocatable }
| PARAMETER
  { mkkind ~loc:(mkloc ()) Parameter }

adecl:
| exp   { mkdim_param ~loc:(mkloc ()) (Exp $1) }
| COLON { mkdim_param ~loc:(mkloc ()) Colon }

seq_adecl:
  /* empty */           { [] }
| COMMA adecl seq_adecl { $2 :: $3 }

seq_decl:
  /* empty */                       { [] }
| decl seq_decl                     { $1 :: $2 }

seq_exp:
  /* empty */        { [] }
| exp                { [$1] }
| exp COMMA seq_exp  { $1 :: $3 }

seq_case:
| /* empty */   { [] }
| case seq_case { $1 :: $2 }

case:
| CASE LPAREN seq_case_opt RPAREN seq_decl
  { mkcase ~loc:(mkloc ()) $3 $5 }
| CASE DEFAULT seq_decl
  { mkcase ~loc:(mkloc ()) [] $3 }

seq_case_opt:
| case_opt                    { [$1]     }
| case_opt COMMA seq_case_opt { $1 :: $3 }

case_opt:
| range         { Range $1 }
| simple_exp    { Scala $1 }

range:
| simple_exp COLON simple_exp
  { mkrange ~loc:(mkloc ()) (Some $1) (Some $3) }
| COLON simple_exp
  { mkrange ~loc:(mkloc ()) None      (Some $2) }
| simple_exp COLON
  { mkrange ~loc:(mkloc ()) (Some $1) None      }

decl:
| CALL IDENT LPAREN seq_exp RPAREN
  { mkdecl ~loc:(mkloc ()) (Call ($2, $4)) }
| IDENT EQ exp
  { mkdecl ~loc:(mkloc ()) (Assign ($1, $3)) }
| DO IDENT EQ exp COMMA exp COMMA exp seq_decl END DO
  { mkdecl ~loc:(mkloc ()) (Do ($2, $4, $6, Some $8, $9))}
| DO IDENT EQ exp COMMA exp seq_decl END DO
  { mkdecl ~loc:(mkloc ()) (Do ($2, $4, $6, None, $7))}
| DO WHILE LPAREN exp RPAREN seq_decl END DO
  { mkdecl ~loc:(mkloc ()) (While ($4, $6))}
| SELECT CASE LPAREN exp RPAREN seq_case END SELECT ident_or_blank
  { mkdecl ~loc:(mkloc ()) (Select (mkselect ~loc:(mkloc ()) $4 $6)) }

exp:
| simple_exp { $1 }
| MINUS exp
  { mkexp ~loc:(mkloc ()) (Rev $2) }
| arith
  { mkexp ~loc:(mkloc ()) $1 }
| comp
  { mkexp ~loc:(mkloc ()) $1 }
| logical
  { mkexp ~loc:(mkloc ()) $1 }
| LPAREN exp RPAREN
  { $2 }
| IDENT LPAREN seq_exp RPAREN
  { mkexp ~loc:(mkloc ()) (Funcall ($1, $3)) }

simple_exp:
| const
  { mkexp ~loc:(mkloc ()) (Const $1) }
| IDENT
  { mkexp ~loc:(mkloc ()) (Ident $1) }

const:
| TRUE
  { mkconst ~loc:(mkloc ()) (Cbool true) }
| FALSE
  { mkconst ~loc:(mkloc ()) (Cbool false) }
| INT
  { mkconst ~loc:(mkloc ()) (Cint $1) }
| FLOAT
  { mkconst ~loc:(mkloc ()) (Creal $1) }

logical:
| NOT exp      { Not $2        }
| exp AND exp  { And ($1, $3)  }
| exp OR exp   { Or ($1, $3)   }
| exp EQV exp  { Eqv ($1, $3)  }
| exp NEQV exp { Neqv ($1, $3) }

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
