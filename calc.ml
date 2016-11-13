let _ =
  if not (Array.length Sys.argv = 2) then exit 1
  else
  try
    let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in
    let result = Parser.main Lexer.token lexbuf in
    Parse_tree.sexp_of_block result
    |> Sexplib.Sexp.output stdout;
    print_endline "";
    flush stdout;
    exit 0;
  with _ ->
    let line, b, e = !Lexer.loc in
    Format.printf "%s: %s@.  line %d, start char %d, end char %d@."
      Sys.argv.(1) !Lexer.target line b e;
    exit 1
