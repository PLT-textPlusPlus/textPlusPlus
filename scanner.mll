(* Scanner for text++ *)

{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }   (* Whitespace *)
| "/*"          { blockComment lexbuf  }     (* Comments *)
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
| '+'           { PLUS }
| '-'           { MINUS }
| '--'          { DECREMENT }
| '++'          { INCREMENT }
| '*'           { TIMES }
| '/'           { DIVIDE }
| '%'	          { MODULO }
| '='           { ASSIGN }
| "=="          { EQ }
| "!="          { NEQ }
| '<'           { LT }
| "<="          { LEQ }
| ">"           { GT }
| ">="          { GEQ }
| "&&"          { AND }
| "||"          { OR }
| "!"           { NOT }
| "if"          { IF }
| "else"        { ELSE }
| "elif"        { ELIF }
| "for"         { FOR }
| "while"  	    { WHILE }
| "continue"    { CONT }
| "break"       { BREAK }
| "return"      { RETURN }
| "bool"        { BOOL }
| "true"        { TRUE }
| "false"       { FALSE }
| "def"         { DEFINE }
| "dec"         { DECLARE }
| eof           { EOF }

(* Styling Tokens *)

|  "*"                      { BOLD }
|  "**"                     { ITALICS }
|  "__"                     { UNDERLINE }
| ['h']['1'-'6'] as size    { HEADING(size) }
| "font"                    { FONT }

| 
| [ 'left' 'right' 'center'] as position  { ALIGNMENT(position) }





| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }

| [ '' ]


| ['a'-'z']+ as id { VARIABLE(id) }

| ['0'-'9']+ as lxm { INTEGER(int_of_string lxm) }
| ['0'-'9']*'.'['0'-'9']+ | ['0'-'9']*'.'['0'-'9']+ as lxm { FLOAT(float_of_string lxm)}


| '"'([^ '"']* as str)'"' as lxm { STRING(str)} /*check this*/



| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and blockComment = parse
  "*/" { token lexbuf }
| _    { blockComment lexbuf }

and lineComment = parse
  ['\n']  { token lexbuf }
| _       { lineComment lexbuf }
