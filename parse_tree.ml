open Sexplib.Std

type 'a main =
  {program: 'a block; subprograms: 'a sub list}

and 'a sub =
  {sub_subprogram: 'a sub_desc; sub_loc: Location.t}

and 'a sub_desc =
  | Subroutine of 'a subroutine
  | Function   of 'a func

and 'a subroutine =
  {sub_name: string; sub_args: string list; sub_decls: 'a decl list}

and 'a func =
  {func_name:string; func_args: string list; func_decls: 'a decl list}

and 'a block =
  {vardecls: 'a vardecl list; decls: 'a decl list}

and 'a vardecl =
  {vardecl_desc: 'a vardecl_desc; vardecl_loc: Location.t}

and 'a vardecl_desc =
  {var: string; init: 'a expr option; typ: typ; kind: 'a kind list}

and typ =
  {typ_desc: typ_desc; typ_loc: Location.t}

and typ_desc =
  | Tinteger
  | Treal
  | Tcomplex
  | Tlogical
  | Tdouble

and 'a kind =
  {kind_desc: 'a kind_desc; kind_loc: Location.t}

and 'a kind_desc =
  | Pointer
  | Dimension of 'a dim_param list
  | Allocatable
  | Parameter

and 'a dim_param =
  {dim_param_desc: 'a dim_param_desc; dim_param_loc: Location.t}

and 'a dim_param_desc =
  | Default
  | Colon of 'a expr option * 'a expr option * 'a expr option
  | Exp   of 'a expr

and 'a decl =
  {decl_desc: 'a decl_desc; decl_loc:  Location.t}

and 'a decl_desc =
  | While    of 'a expr * 'a decl list
  | If       of 'a expr * 'a decl list * 'a decl list
  | Assign   of string * 'a expr
  | Assign_a of string * 'a dim_param list * 'a expr
  | Do       of string * 'a expr * 'a expr * 'a expr option * 'a decl list
  | Select   of 'a select
  | Call     of string * 'a expr list
  | Return   of 'a expr
  | Stop
  | Label    of int * 'a decl
  | Goto     of int

and 'a expr =
  {expr_desc: 'a expr_desc; expr_loc: Location.t; expr_typ: 'a}

and 'a expr_desc =
  | Const   of const
  | Funcall of string * 'a dim_param list
  | Ident   of string
  | Access  of string * 'a dim_param list

  | Array    of 'a expr list

  | Rev     of 'a expr
  | Plus    of 'a expr * 'a expr
  | Minus   of 'a expr * 'a expr
  | Mul     of 'a expr * 'a expr
  | Div     of 'a expr * 'a expr
  | Eq      of 'a expr * 'a expr
  | Neq     of 'a expr * 'a expr
  | Leq     of 'a expr * 'a expr
  | Less    of 'a expr * 'a expr

  | Not     of 'a expr
  | And     of 'a expr * 'a expr
  | Or      of 'a expr * 'a expr
  | Eqv     of 'a expr * 'a expr
  | Neqv    of 'a expr * 'a expr

and const =
  {const_desc: const_desc; const_loc: Location.t}

and const_desc =
  | Cint  of int
  | Creal of string
  | Cbool of bool

and 'a case =
    { case_option: 'a case_option list; (* [] means DEFAULT *)
      case_decls: 'a decl list;
      case_loc: Location.t }

and 'a case_option =
  | Range of 'a range
  | Scala of 'a expr

and 'a range =
    { range_left:  'a expr option;
      range_right: 'a expr option;
      range_loc: Location.t }

and 'a select =
    { select_expr: 'a expr;
      select_cases: 'a case list;
      select_loc: Location.t }
[@@deriving sexp]
