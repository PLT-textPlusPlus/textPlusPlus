

{ open Parser }

rule token = parse
  [' ' '\t' '\r' ] { token lexbuf }   (* Whitespace *)
| "/*"          { blockComment lexbuf  }     (* Comments *)
| "//" 		      { lineComment  lexbuf  }
| '\n'          { ENDLINE }  
| '('           { LPAREN }
| ')'           { RPAREN }
| '{'           { LBRACE }
| '}'           { RBRACE }
| '['           { LBRACKET }
| ']'           { RBRACKET }
| '@'           { TAG }
| ';'           { SEMI }
| ':'	        { COLON }
| ','           { COMMA }
| '+'           { PLUS }
| '-'           { MINUS }
| "mm"          { DECREMENT }
| "pp"          { INCREMENT }
| '*'           { TIMES }
| '/'           { DIVIDE }
| '%'	        { MODULO }
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

| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| ['a'-'z']+ as id { VARIABLE(id) }
| ['0'-'9']+ as lxm { NUMBER(int_of_string lxm) }
| ['0'-'9']*'.'['0'-'9']+ | ['0'-'9']*'.'['0'-'9']+ as lxm { FLOAT(float_of_string lxm)}
| '"'([^ '"']* as str)'"' as lxm { STRING(str)}

and blockComment = parse
  "*/" { token lexbuf }
| _    { blockComment lexbuf }

and lineComment = parse
  ['\n']  { token lexbuf }
| _       { lineComment lexbuf }