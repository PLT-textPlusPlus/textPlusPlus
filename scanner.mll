{ open Parser }

let whitespace = [' ' '\t' '\r' '\n']
let digits = ['0'-'9']
let decimal = ['.']
let esc = '\\' ['\\' ''' '"' 'n' 'r' 't']
let alphabet = ['a'-'z' 'A'-'Z' ]
let ascii = ([' '-'!' '#'-'[' ']'-'~'])


(* Data Types *)
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let integer = digits+
let float = digits* decimal digits+ | digits+ decimal digits*


rule token = parse
    whitespace  { token lexbuf }           (* Whitespace *)
  | "/*"          { blockComment lexbuf  } (* Comments *)
  | "//" 		      { lineComment  lexbuf  }
  | '('           { LPAREN }
  | ')'           { RPAREN }
  | '{'           { LBRACE }
  | '}'           { RBRACE }
  | '['           { LBRACKET }
  | ']'           { RBRACKET }
  | '@'           { TAG }
  | ';'           { SEMI }
  | ':'	          { COLON }
  | ','           { COMMA }

  (* Arithmetic Operators *)

  | '+'           { PLUS }
  | '-'           { MINUS }
  | "++"          { DECREMENT }
  | "--"          { INCREMENT }
  | '*'           { TIMES }
  | '/'           { DIVIDE }
  | '%'	          { MODULO }
  | '='           { ASSIGN }
  | "=="          { EQ }
  | "!="          { NEQ }

  (* Relational Operators *)
  | '<'           { LT }
  | "<="          { LEQ }
  | ">"           { GT }
  | ">="          { GEQ }

  (* Logical Operators *)
  | "&&"          { AND }
  | "||"          { OR }
  | "!"           { NOT }

  (* Control Flow *)
  | "if"          { IF }
  | "else"        { ELSE }
  | "for"         { FOR }
  | "while"  	    { WHILE }
  | "continue"    { CONT }
  | "break"       { BREAK }
  | "return"      { RETURN }

  (* Keywords *)
  | "int"       { INT }
  | "float"     { FLOAT }
  | "string"    { STRING }
  | "bool"      { BOOL }
  | "null"      { NULL }
  | "true"      { TRUE }
  | "false"     { FALSE }
  | "def"         { DEFINE }
  | "dec"         { DECLARE }

  (* Literals and Identifiers *)
  | integer as lxm      { NUMBER(int_of_string lxm) }
  | float as lxm        { FLOAT(float_of_string lxm)}
  | '"'([^ '"']* as str)'"' as lxm { STRING(str)}
  | id as lxm           { ID(lxm) }

  
  (* End of File *)
  | eof { EOF }

  (* Error Handling *)
  | _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }


and blockComment = parse
    "*/" { token lexbuf }
  | _    { blockComment lexbuf }

and lineComment = parse
    ['\n']  { token lexbuf }
  | _       { lineComment lexbuf }


