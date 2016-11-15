open Format

type t = {
  loc_start: Lexing.position;
  loc_end: Lexing.position;
  loc_ghost: bool; }

let mkloc () =
  { loc_start = Parsing.symbol_start_pos ();
    loc_end = Parsing.symbol_end_pos ();
    loc_ghost = false; }

let sexp_of_t { loc_start; loc_end } =
  Sexplib.Std.sexp_of_string "dummy"

let t_of_sexp sexp =
  {loc_start = Lexing.dummy_pos; loc_end = Lexing.dummy_pos; loc_ghost = true}
