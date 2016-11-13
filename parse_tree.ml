open Sexplib.Std

type block =
  {vardecls: vardecl list; decls: decl list}

and vardecl =
  {vardecl_desc: vardecl_desc; vardecl_loc: Location.t}

and vardecl_desc =
  {name: string; typ: typ; init: expr option; kind: kind list option}

and typ =
  {typ_desc: typ_desc; typ_loc: Location.t}

and typ_desc =
  | Integer
  | Real

and kind =
  {kind_desc: kind_desc; kind_loc: Location.t}

and kind_desc =
  | Pointer
  | Dimension of dim_param list

and dim_param =
  {dim_param_desc: dim_param_desc; dim_param_loc: Location.t}

and dim_param_desc =
  | Colon
  | Exp of expr

and decl =
  {decl_desc: decl_desc; decl_loc:  Location.t}

and decl_desc =
  | While   of expr * block
  | If      of expr * block * block option
  | Assign  of string * expr
  | Print   of expr

and expr =
  {expr_desc: expr_desc; expr_loc: Location.t}

and expr_desc =
  | Const   of const
  | Ident   of string
  | Rev     of expr
  | Plus    of expr * expr
  | Minus   of expr * expr
  | Mul     of expr * expr
  | Div     of expr * expr
  | Eq      of expr * expr
  | Neq     of expr * expr
  | Not     of expr
  | Leq     of expr * expr
  | Less    of expr * expr

and const =
  {const_desc: const_desc; const_loc: Location.t}

and const_desc =
  | Int of int
[@@deriving sexp]

let string_to_typ = function
  | "real" -> Real
  | "integer" -> Integer
  | _ as str -> "string_to_typ" ^ str |> failwith
