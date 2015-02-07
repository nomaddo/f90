type expr =
  | Const   of const
  | Ident   of string
  | While   of expr * expr
  | If      of expr * expr * expr
  | Assign  of expr * expr
  | Seq     of expr * expr
  | Print   of expr
  | Var     of expr * expr
  | Plus    of expr * expr
  | Minus   of expr * expr
  | Mul     of expr * expr
  | Div     of expr * expr
  | Eq      of expr * expr
  | Neq     of expr * expr
  | Not     of expr
  | Leq     of expr * expr
  | Less    of expr * expr
  | Geq     of expr * expr
  | Greater of expr * expr

and const =
  | Int of int
