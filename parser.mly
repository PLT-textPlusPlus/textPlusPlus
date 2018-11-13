(*open the Ast file*)
%{ open Ast %}

%token SEMI COLON COMMA ENDLINE LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET TAG
%token HEADING FONT ALIGNMENT VARIABLE 
%token PLUS MINUS TIMES DIVIDE MODULO ASSIGN NOT 
%token EQ NEQ LT LEQ NEQ GT GEQ TRUE FALSE AND OR
%token IF ELSE ELIF FOR WHILE CONT RETURN BOOL
%token BOLD ITALICS UNDERLINE HEADING FONT LINE BULLET
%token ALIGNMENT PAGEBREAK RENDER INDENT
%token DEFINE
%token DECLARE

%token <int> NUMBER
%token <float> FLOAT
%token <string> STRING
%token <string> STRLITERAL
%token <string> CHAR
%token <string> ID
%token <string> VARIABLE

%token EOF

%start program
%type <Ast.program> program

%nonassoc ELSE
%nonassoc ELIF
%left COMMA
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left GT GEQ LT LEQ
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%right NOT 
%left DECREMENT INCREMENT


%start program
%type <Ast.program> program

%%


program:
  declarations EOF { $1 }


declarations:
   /* nothing */ { [], [] }
 | declarations var_declaration { ($2 :: fst $1), snd $1 }
 | declarations func_declaration { fst $1, ($2 :: snd $1) }


 func_declaration:
   TAG DEFINE ID LPAREN optional_formal_parameters RPAREN LBRACE var_declaration_list stmt_list RBRACE
     { {  function_name = $3;
        parameters= $5;
        local_variables = List.rev $8;
        code_block = List.rev $9; 
     } }


type:
    NUMBER  { Int }
  | BOOL    { Bool }
  | FLOAT   { Float } 
  | CHAR    { Char } 
  | STRING  { String } 


optional_formal_parameters:
    /* nothing */ { [] }
  | formal_parameters_list   { List.rev $1 }


formal_parameters_list:
    type ID                   { [($1,$2)] }
  | formal_parameters_list COMMA type ID { ($3,$4) :: $1 }


var_declaration_list:
    /* nothing */    { [] }
  | var_declaration_list var_declaration { $2 :: $1 }


var_declaration:
   TAG DECLARE ID LBRACKET RBRACKET { ($3) }


stmt_list:
   stmt_list stmt { $2 :: $1 }


stmt:
    expr ENDLINE { Expr $1 }
  | RETURN ENDLINE { Return Noexpr }
  | RETURN expr ENDLINE { Return $2 }
  | TAG LPAREN stmt_list RPAREN { Block(List.rev $3) }
  | TAG LBRACE stmt_list RBRACE { Block(List.rev $3) }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN optional_expr SEMI expr SEMI optional_expr RPAREN stmt
     { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }

optional_expr:
  /* nothing */ { Noexpr }
  | expr          { $1 }

expr:

    STRLITERAL                               { Literal($1) }
  | NUMBER                         { Number($1)}
  | FLOAT                          { Float($1) }
  | STRING                         { String($1)}
  | TRUE                         { Boolean(true)}
  | FALSE                      { Boolean(false)}
  | TAG  VARIABLE              { Var($2) }
  | expr PLUS   expr             { Binop($1, Add,   $3) }
  | expr MINUS  expr             { Binop($1, Sub,   $3) }
  | expr TIMES  expr             { Binop($1, Mult,  $3) }
  | expr DIVIDE expr             { Binop($1, Div,   $3) }
  | expr MODULO expr             { Binop($1, Mod,   $3) }
  | expr EQ     expr             { Binop($1, Equal, $3) }
  | expr NEQ    expr             { Binop($1, Neq,   $3) }
  | expr LT     expr             { Binop($1, Less,  $3) }
  | expr LEQ    expr             { Binop($1, Leq,   $3) }
  | expr GT     expr             { Binop($1, Greater, $3) }
  | expr GEQ    expr                   { Binop($1, Geq,   $3) }
  | expr AND    expr                   { Binop($1, And,   $3) }
  | expr OR     expr                   { Binop($1, Or,    $3) }
  | NOT expr                                 { Unop(Not, $2) }
  | expr DECREMENT                           { Binop($1, Decrement) }
  | expr INCREMENT                           { Binop($1, Increment) }
  | LPAREN expr RPAREN                       { $2 }
  | TAG VARIABLE LBRACKET expr RBRACKET      { Assign($2, $4) }
  | TAG ID LPAREN option_args RPAREN         { Call($2, $4) }

option_args:
    /* nothing */           { [] }
  | args_list               { List.rev $1 }

args_list:
    expr                    { [$1] }
  | args_list COMMA expr    { $3 :: $1 }