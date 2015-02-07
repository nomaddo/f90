(* File calc.ml *)
let _ =
  try
    let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in
    let result = Parser.main Lexer.token lexbuf in
    Format.printf "Parse Sucessful !@."
  with _ ->
    let line, b, e = !Lexer.loc in
    Format.printf "error at: %s@. line %d, start char %d, end char %d@."
      !Lexer.target line b e;
    exit 0
