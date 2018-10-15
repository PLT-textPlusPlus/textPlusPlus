/*Parser for test++ */


%token SEMI COLON LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET TAG
%token HEADING FONT ALIGNMENT VARIABLE ENDLINE
%token PLUS MINUS TIMES DIVIDE MODULO ASSIGN NOT COMMA
%token EQ NEQ LT NEQ GT GEQ AND OR 
%token IF ELSE ELIF FOR WHILE CONT RETURN BOOL DEFINE DECLARE
%token DEFINE
%token DECLARE

%token <int> INTEGER
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

%%

expr:
	NUMBER						             { Number($1)}
|	FLOAT  						             { Float($1) }
| 	STRING   					             { String($1)}
|	TRUE						             { Boolean(true)}
|	FALSE						      		 { Boolean(false)}
|	ID										 { Id($1)}
|	TAG  VARIABLE							 { Var($1)}
| 	expr PLUS   expr 						 { Binop($1, Add,   $3) }
|	expr MINUS  expr 						 { Binop($1, Sub,   $3) }
|	expr TIMES  expr 						 { Binop($1, Mult,  $3) }
| 	expr DIVIDE expr 						 { Binop($1, Div,   $3) }
|	expr MODULO expr						 { Binop($1, Mod,   $3) }
|	expr EQ     expr 						 { Binop($1, Equal, $3) }
| 	expr NEQ    expr 						 { Binop($1, Neq,   $3) }
|	expr LT     expr 						 { Binop($1, Less,  $3) }
|	expr LEQ    expr 						 { Binop($1, Leq,   $3) }
| 	expr GT     expr 						 { Binop($1, Greater, $3) }
|	expr GEQ    expr 			             { Binop($1, Geq,   $3) }
| 	expr AND    expr 			             { Binop($1, And,   $3) }
| 	expr OR     expr 			             { Binop($1, Or,    $3) }

decls:
	TAG DECLARE VARIABLE						{ [] }
| 	TAG DECLARE VARIABLE LBRACKET expr RBRACkET { Call($1, $3) }

fdecl:
	TAG DEFINE ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
	fname = $1
	formals = $4
	locals = List.rev $7
	body = List.rev

vdecl_list:
	/*nothing*/								{ [] }
|   vedcl_list vdecl 						{ $2 :: $3}

vdecl:
	decls 									{ $1 }
		
	

(*
|	TAG LBRACE expr RBRACE 					 { $2 }
|	TAG LPAREN expr RPAREN					 { $2 }


expr_math:
	| 	ID ASSIGN	expr			{ Assign($1, $3) 
*)







