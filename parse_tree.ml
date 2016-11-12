type block =
  {var_decls: var_decl list; decls: decl list}

and var_decl =
  {name: string; typ: typ; init: expr option; kind: kind list option}

and typ =
  | Integer
  | Real

and kind =
  | Pointer
  | Dimension of dim_param list

and dim_param =
  | Colon
  | Exp of expr

and decl =
  | While   of expr * block
  | If      of expr * block * block option
  | Assign  of string * expr
  | Print   of expr

and expr =
  | Const   of const
  | Ident   of string
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
  | Int of int
[@@derivin sexp]

let string_to_typ = function
  | "real" -> Real
  | "integer" -> Integer
  | _ as str -> "string_to_typ" ^ str |> failwith
