type token =
  | SEMI
  | COLON
  | COMMA
  | ENDLINE
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACKET
  | RBRACKET
  | TAG
  | HEADING
  | FONT
  | ALIGNMENT
  | VARIABLE of (string)
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | MODULO
  | ASSIGN
  | NOT
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | TRUE
  | FALSE
  | AND
  | OR
  | IF
  | ELSE
  | ELIF
  | FOR
  | WHILE
  | CONT
  | RETURN
  | BOOL
  | BOLD
  | ITALICS
  | UNDERLINE
  | LINE
  | BULLET
  | PAGEBREAK
  | RENDER
  | INDENT
  | DEFINE
  | DECLARE
  | NUMBER of (int)
  | FLOAT of (float)
  | STRING of (string)
  | STRLITERAL of (string)
  | CHAR of (string)
  | ID of (string)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
