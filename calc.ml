let _ =
  try
    let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in
    let result = Parser.main Lexer.token lexbuf in
    ()
  with _ ->
    let line, b, e = !Lexer.loc in
    Format.printf "error at: %s@. %s line %d, start char %d, end char %d@."
      !Lexer.target __FILE__ line b e;
    exit 0
