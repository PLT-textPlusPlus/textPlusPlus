(*Scanner for text++ *)

{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }   (* Whitespace *)
| "/*"       { blockComment lexbuf  }     (* Comments *)
| "//" 		 { lineComment  lexbuf  }
| '('        { LPAREN }
| ')'        { RPAREN }
| '{'        { LBRACE }
| '}'        { RBRACE }
| ';'        { SEMI }
| ':'	     { COLON }
| ','        { COMMA }
| '+'        { PLUS }
| '-'        { MINUS }
| '*'        { TIMES }
| '/'        { DIVIDE }
| '%'	     { MODULO }
| '='        { ASSIGN }
| "=="       { EQ }
| "!="       { NEQ }
| '<'        { LT }
| "<="       { LEQ }
| ">"        { GT }
| ">="       { GEQ }
| "&&"       { AND }
| "||"       { OR }
| "!"        { NOT }
| "if"       { IF }
| "else"     { ELSE }
| "elif"     { ELIF }
| "for"      { FOR }
| "while"  	 { WHILE }
| "continue" { CONT }
| "break"    { BREAK }
| "return"   { RETURN }
| "bool"     { BOOL }
| "void"     { VOID }
| "true"     { TRUE }
| "false"    { FALSE }
| ['0'-'9']+ as lxm { INTEGER(int_of_string lxm) }
| ['0'-'9']*'.'['0'-'9']+ | ['0'-'9']*'.'['0'-'9']+ as lxm { FLOAT(float_of_string lxm)}
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| '"'([^ '"']* as str)'"' as lxm { STRING(str)} /*check this*/
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and blockComment = parse
  "*/" { token lexbuf }
| _    { blockComment lexbuf }

and lineComment = parse
  ['\n']  { token lexbuf }
| _       { lineComment lexbuf }
