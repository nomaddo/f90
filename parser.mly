%{
open Parse_tree
open Location

let mkblock var decl =
  {vardecls = var; decls = decl}

let mktyp ~loc typ_desc =
  { typ_desc; typ_loc = loc}

let mkkind ~loc kind =
  {kind_desc = kind; kind_loc = loc}

let mkvar_decl ~kind typ pairs =
  List.map (fun (var, dim, init, loc) ->
    let kind =
      begin match dim with
      | None -> kind
      | Some a -> mkkind ~loc (Dimension a) :: kind end in
    {vardecl_desc = {var; init; kind; typ}; vardecl_loc = loc}) pairs

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
%token EQV NEQV
%token GREATER LESS GEQ LEQ EQEQ
%token REAL INTEGER LOGICAL COMPLEX DOUBLE PRECISION
%token SELECT CASE DEFAULT
%token SUBROUTINE
%token FUNCTION RETURN STOP
%token BR GO TO GOTO
%token LBRACE RBRACE LPAREN_S S_RPAREN
%token COMMENT
%left PLUS MINUS        /* lowest precedence */
%left MUL DIV           /* medium precedence */
%left EQ GREATER GEQ LESS LEQ
%left NOT AND OR EQV NEQV

%start main             /* the entry point */
%type <unit Parse_tree.main> main

%%

eof:
| BR eof      { () }
| EOF         { () }

br:
| BR          { () }
| BR br       { () }

main:
| PROGRAM IDENT br top_block CONTAINS br seq_subprogram END PROGRAM ident_or_blank eof
  { {program = $4; subprograms = $7} }
| PROGRAM IDENT br top_block END PROGRAM ident_or_blank eof
  { {program = $4; subprograms = []} }

ident_or_blank:
| /* empty */    { "empty" }
| IDENT          { $1 }

seq_subprogram:
| /* empty */       { [] }
| subprogram seq_subprogram { $1 :: $2 }

subprogram:
| SUBROUTINE IDENT LPAREN seq_ident RPAREN br seq_decl END SUBROUTINE ident_or_blank br
  { mksub ~loc:(mkloc ()) (Subroutine (mksubroutine ~loc:(mkloc ()) $2 $4 $7)) }
| FUNCTION IDENT LPAREN seq_ident RPAREN br seq_decl END FUNCTION ident_or_blank br
  { mksub ~loc:(mkloc ()) (Function (mkfunc ~loc:(mkloc ()) $2 $4 $7)) }

seq_ident:
| /* empty */           { [] }
| IDENT COMMA seq_ident { $1 :: $3 }
| IDENT                 { [$1] }

top_block:
  seq_var seq_decl                  { mkblock $1 $2 }

seq_var:
  /* empty */                       { [] }
| decl_var seq_var                  { $1 @ $2 }

decl_var:
| typ opt_kind COLCOL seq_decl_assign br
  { mkvar_decl $1 $4 ~kind:$2 }
| typ seq_decl_assign br
  { mkvar_decl $1 $2 ~kind:[] }

decl_assign:
| IDENT        { ($1, None, None, mkloc ()) }
| IDENT EQ exp { ($1, None, Some $3, mkloc ()) }
| IDENT LPAREN seq_adecl RPAREN { ($1, Some $3, None, mkloc ())}
| IDENT LPAREN seq_adecl RPAREN EQ exp { ($1, Some $3, Some $6, mkloc ())}

seq_decl_assign:
| decl_assign
  { [$1] }
| decl_assign COMMA seq_decl_assign
  { $1 :: $3 }

typ:
| INTEGER          { mktyp ~loc:(mkloc ()) Tinteger }
| REAL             { mktyp ~loc:(mkloc ()) Treal    }
| COMPLEX          { mktyp ~loc:(mkloc ()) Tcomplex }
| LOGICAL          { mktyp ~loc:(mkloc ()) Tlogical }
| DOUBLE PRECISION { mktyp ~loc:(mkloc ()) Tdouble  }

opt_kind:
  /* empty */                       { [] }
| COMMA kind opt_kind               { $2 :: $3 }

kind:
  POINTER
  { mkkind ~loc:(mkloc ()) Pointer }
| DIMENSION LPAREN seq_adecl RPAREN
  { mkkind ~loc:(mkloc ()) (Dimension $3) }
| ALLOCATABLE
  { mkkind ~loc:(mkloc ()) Allocatable }
| PARAMETER
  { mkkind ~loc:(mkloc ()) Parameter }

adecl:
| exp
  { mkdim_param ~loc:(mkloc ()) (Exp $1) }
| COLON
  { mkdim_param ~loc:(mkloc ()) Default }
| exp COLON exp
  { mkdim_param ~loc:(mkloc ())
    (Colon (Some $1, Some $3, None)) }
| exp COLON exp COLON exp
  { mkdim_param ~loc:(mkloc ())
    (Colon (Some $1, Some $3, Some $5)) }
| COLON exp
  { mkdim_param ~loc:(mkloc ())
    (Colon (None, Some $2, None)) }
| COLON exp COLON exp
  { mkdim_param ~loc:(mkloc ())
    (Colon (None, Some $2, Some $4)) }
| COLON COLON exp
  { mkdim_param ~loc:(mkloc ())
    (Colon (None, None, Some $3)) }
| COLCOL exp /* XXX duplication */
  { mkdim_param ~loc:(mkloc ())
    (Colon (None, None, Some $2)) }

seq_adecl:
| /* empty */           { []       }
| adecl                 { [$1]     }
| adecl COMMA seq_adecl { $1 :: $3 }

seq_decl:
| /* empty */                       { [] }
| decl seq_decl                     { $1 :: $2 }

seq_exp:
| /* empty */        { [] }
| exp                { [$1] }
| exp COMMA seq_exp  { $1 :: $3 }

seq_case:
| /* empty */   { [] }
| case seq_case { $1 :: $2 }

case:
| CASE LPAREN seq_case_opt RPAREN br seq_decl
  { mkcase ~loc:(mkloc ()) $3 $6 }
| CASE DEFAULT br seq_decl
  { mkcase ~loc:(mkloc ()) [] $4 }

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

one_line_decl:
| CALL IDENT LPAREN seq_exp RPAREN br
  { mkdecl ~loc:(mkloc ()) (Call ($2, $4)) }
| IDENT EQ exp br
  { mkdecl ~loc:(mkloc ()) (Assign ($1, $3)) }
| IDENT LPAREN seq_adecl RPAREN EQ exp br
  { mkdecl ~loc:(mkloc ()) (Assign_a ($1, $3, $6))}
| RETURN exp br
  { mkdecl ~loc:(mkloc ()) (Return $2)}
| STOP br
  { mkdecl ~loc:(mkloc ()) Stop}
| GO TO INT br
  { mkdecl ~loc:(mkloc ()) (Goto $3)}
| GOTO INT br
  { mkdecl ~loc:(mkloc ()) (Goto $2)}

decl:
| DO IDENT EQ exp COMMA exp COMMA exp br seq_decl END DO br
  { mkdecl ~loc:(mkloc ()) (Do ($2, $4, $6, Some $8, $10))}
| DO IDENT EQ exp COMMA exp br seq_decl END DO br
  { mkdecl ~loc:(mkloc ()) (Do ($2, $4, $6, None, $8))}
| DO WHILE LPAREN exp RPAREN br seq_decl END DO br
  { mkdecl ~loc:(mkloc ()) (While ($4, $7))}
| SELECT CASE LPAREN exp RPAREN br seq_case END SELECT ident_or_blank br
  { mkdecl ~loc:(mkloc ()) (Select (mkselect ~loc:(mkloc ()) $4 $7)) }
| IF LPAREN exp RPAREN THEN br seq_decl END IF br
  { mkdecl ~loc:(mkloc ()) (If ($3, $7, []))}
| IF LPAREN exp RPAREN THEN br seq_decl ELSE br seq_decl END IF br
  { mkdecl ~loc:(mkloc ()) (If ($3, $7, $10))}
| IF LPAREN exp RPAREN one_line_decl
  { mkdecl ~loc:(mkloc ()) (If ($3, [$5], []))}
| one_line_decl
  { $1 }
| INT decl
  { mkdecl ~loc:(mkloc ()) (Label ($1, $2))}

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
| IDENT LPAREN seq_adecl RPAREN
  { mkexp ~loc:(mkloc ()) (Funcall ($1, $3)) }
| LBRACE seq_exp RBRACE
  { mkexp ~loc:(mkloc ()) (Array $2)}
| LPAREN_S seq_exp S_RPAREN
  { mkexp ~loc:(mkloc ()) (Array $2)}

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
| exp EQEQ exp                      { Eq ($1, $3) }
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
