open Sexplib.Std

type 'a block =
  {vardecls: 'a vardecl list; decls: 'a decl list}

and 'a vardecl =
  {vardecl_desc: vardecl_desc; vardecl_loc: Location.t}

and vardecl_desc =
  {name: string; typ: typ; init: unit expr option; kind: kind list option}

and typ =
  {typ_desc: typ_desc; typ_loc: Location.t}

and typ_desc =
  | Tinteger
  | Treal
  | Tcomplex
  | Tlogical
  | Tdouble

and kind =
  {kind_desc: kind_desc; kind_loc: Location.t}

and kind_desc =
  | Pointer
  | Dimension of dim_param list
  | Allocatable
  | Parameter

and dim_param =
  {dim_param_desc: dim_param_desc; dim_param_loc: Location.t}

and dim_param_desc =
  | Colon
  | Exp of unit expr

and 'a decl =
  {decl_desc: 'a decl_desc; decl_loc:  Location.t}

and 'a decl_desc =
  | While   of 'a expr * 'a decl list
  | If      of 'a expr * 'a decl list * 'a decl list
  | Assign  of string * 'a expr
  | Do      of string * 'a expr * 'a expr * 'a expr option * 'a decl list
  | Select  of 'a select

and 'a expr =
  {expr_desc: 'a expr_desc; expr_loc: Location.t; expr_typ: 'a}

and 'a expr_desc =
  | Const   of const
  | Ident   of string
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
[@@deriving sexp]

and 'a case =
    { case_range: 'a range;
      case_decls: 'a decl list;
      case_loc: Location.t }

and 'a range =
    { range_left:  'a expr option;
      range_right: 'a expr option;
      range_loc: Location.t }

and 'a select =
    { select_expr: 'a expr;
      select_cases: 'a case list;
      select_loc: Location.t }
