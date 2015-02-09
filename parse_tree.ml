type block =
  {var_decls: var_decl list; decls: decl list}

and var_decl =
  {name: string; initialize: expr}

and decl =
  | While   of expr * block
  | If      of expr * block * block option
  | Assign  of expr * expr
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
  | Int     of int
  | Bool    of int
  | Char    of int
