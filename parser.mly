/* Ocamlyacc parser for textPlusPlus */

%{
open Ast
%}


%token PLUS MINUS DECREMENT INCREMENT TIMES DIVIDE MODULO ASSIGN EQ NEQ 
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET TAG SEMI COLON COMMA
%token LT LEQ GT GEQ AND OR NOT
%token INT FLOAT STRING BOOL NULL TRUE FALSE DEFINE DECLARE
%token IF ELSE FOR WHILE CONT BREAK RETURN

%token <int> NUMBER
%token <float> FLOAT
%token <string> STRING
%token <string> ID

%token EOF

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

typ:
    INT  { Int }
  | BOOL    { Bool }
  | FLOAT   { Float }
  | STRING  { String } 

optional_formal_parameters:
    /* nothing */ { [] }
  | formal_parameters_list   { List.rev $1 }

formal_parameters_list:
    typ ID                   { [($1,$2)] }
  | formal_parameters_list COMMA typ ID { ($3,$4) :: $1 }

var_declaration_list:
    /* nothing */    { [] }
  | var_declaration_list var_declaration { $2 :: $1 }

var_declaration:
   TAG DECLARE ID LBRACKET RBRACKET { ($3) }

stmt_list:
   stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr $1 }
  | RETURN SEMI { Return Noexpr }
  | RETURN expr SEMI { Return $2 }
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

  |	NUMBER						             { Number($1)}
  |	FLOAT  						             { Float($1) }
  |	STRING   					             { String($1)}
  |	TRUE						             { Boolean(true)}
  |	FALSE						      		 { Boolean(false)}
  |	TAG  ID							         { Id($2) }
  | expr PLUS   expr 						 { Binop($1, Add,   $3) }
  |	expr MINUS  expr 						 { Binop($1, Sub,   $3) }
  |	expr TIMES  expr 						 { Binop($1, Mult,  $3) }
  | expr DIVIDE expr 						 { Binop($1, Div,   $3) }
  |	expr MODULO expr						 { Binop($1, Mod,   $3) }
  |	expr EQ     expr 						 { Binop($1, Equal, $3) }
  |	expr NEQ    expr 						 { Binop($1, Neq,   $3) }
  |	expr LT     expr 						 { Binop($1, Less,  $3) }
  |	expr LEQ    expr 						 { Binop($1, Leq,   $3) }
  | expr GT     expr 						 { Binop($1, Greater, $3) }
  |	expr GEQ    expr 			             { Binop($1, Geq,   $3) }
  | expr AND    expr 			             { Binop($1, And,   $3) }
  | expr OR     expr 			             { Binop($1, Or,    $3) }
  | NOT expr                                 { Unop(Not, $2) }
  | expr DECREMENT                           { Postop($1, Decrement) }
  | expr INCREMENT                           { Postop($1, Increment) }
  | LPAREN expr RPAREN                       { $2 }
  | TAG ID LBRACKET expr RBRACKET      { Assign($2, $4) }
  | TAG ID LPAREN option_args RPAREN         { Call($2, $4) }

option_args:
    /* nothing */           { [] }
  | args_list               { List.rev $1 }

args_list:
    expr                    { [$1] }
  | args_list COMMA expr    { $3 :: $1 }
