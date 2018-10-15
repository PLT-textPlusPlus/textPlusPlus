/*Parser for test++ */


%token SEMI COLON LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET TAG
%token PLUS MINUS TIMES DIVIDE MODULO ASSIGN NOT COMMA
%token EQ NEQ LT NEQ GT GEQ AND OR 
%token IF ELSE ELIF FOR WHILE CONT RETURN BOOL DEFINE DECLARE


%left COMMA
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left GT GEQ LT LEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT 


| '@'           { TAG }
| ';'           { SEMI }
| ':'	        { COLON }
| ','           { COMMA }
| '+'           { PLUS }
| '-'           { MINUS }
| '--'          { DECREMENT }
| '++'          { INCREMENT }
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