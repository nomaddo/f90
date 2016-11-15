(* File lexer.mll *)
{
open Parser        (* The type token is defined in parser.mli *)
exception Error of string

let mkhash l =
  let h = Hashtbl.create (List.length l) in
  List.iter (fun (s, k) -> Hashtbl.add h s k) l; h

let dot_keyword_table =
  mkhash [
    ".true."  , TRUE;
    ".false." , FALSE;
    ".not."   , NOT;
    ".and."   , AND;
    ".or."    , OR;
    ".eqv."   , EQV;
    ".neqv."  , NEQV;
]

let keyword_table =
  mkhash [
    "if", IF;
    "then", THEN;
    "else", ELSE;
    "while", WHILE;
    "case", CASE;
    "select", SELECT;
    "do", DO;
    "print", PRINT;
    "program", PROGRAM;
    "end", END;
    "dimension", DIMENSION;
    "pointer", POINTER;
    "parameter", PARAMETER;
    "allocatable", ALLOCATABLE;

    "real", REAL;
    "integer", INTEGER;
    "complex", COMPLEX;
    "logical", LOGICAL;
  ]

let loc = ref (-1, -1, -1)
let line = ref 1
let target = ref ""

let chars = ref 0
let line_chars = ref 1

let count lexbuf =
  let i = Lexing.lexeme_start lexbuf in
  let j = Lexing.lexeme_end lexbuf in
  j - i

let sum lexbuf =
  line_chars := !line_chars + count lexbuf

let update_loc lexbuf =
  let i = Lexing.lexeme_start lexbuf in
  let j = Lexing.lexeme_end lexbuf in
  loc := (!line, i - !chars + 1, j - !chars + 1)

let set_taget lexbuf =
  target := Lexing.lexeme lexbuf

let update lexbuf =
  sum lexbuf; update_loc lexbuf; set_taget lexbuf

let endline lexbuf =
  incr line;
  chars := !line_chars + !chars + 1;
  line_chars := 0
}

let head = ['A'-'Z' 'a'-'z']
let char = ['A'-'Z' 'a'-'z' '_' '0'-'9']

rule token = parse
  [' ' '\t']        { incr line_chars; token lexbuf }     (* skip blanks *)
| '\n'              { endline lexbuf;  token lexbuf }
| ['0'-'9']+ as lxm { update lexbuf; INT (int_of_string lxm) }
| ('0' | ['1'-'9'] ['0'-'9']*) '.' ['0'-'9']*
    as lxm { update lexbuf; FLOAT lxm }
| '+'               { update lexbuf; PLUS }
| '-'               { update lexbuf; MINUS }
| '*'               { update lexbuf; MUL }
| '/'               { update lexbuf; DIV }
| '('               { update lexbuf; LPAREN }
| ')'               { update lexbuf; RPAREN }
| '='               { update lexbuf; EQ }
| "::"              { update lexbuf; COLCOL }
| "!="              { update lexbuf; NEQ }
| "<="              { update lexbuf; LEQ }
| ">="              { update lexbuf; GEQ }
| ','               { update lexbuf; COMMA }
| ':'               { update lexbuf; COLON }
| '<'               { update lexbuf; LESS }
| '>'               { update lexbuf; GREATER }
| '{'               { update lexbuf; LBRACE }
| '}'               { update lexbuf; RBRACE }
| '.' char * '.'    {
  let s = Lexing.lexeme lexbuf in
  update lexbuf;
  try
    Hashtbl.find dot_keyword_table s
  with
    Not_found -> failwith "dot_keyword_table"
  }
| head char *       {
    let s = Lexing.lexeme lexbuf in
    update lexbuf;
    try
      Hashtbl.find keyword_table s
    with Not_found -> IDENT s
  }
| eof               { EOF }
