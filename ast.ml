
type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or | Mod 

type uop = Not 

type pop = Decr | Incr

type typ = Int | Bool | Float | String | Void

type bind = typ * string

type expr =
Number of int (*insertedt space for debugging purposes*)
| Float of float
| String of string
| Boolean of bool
| Id of string
| Binop of expr * op * expr
| Unop of uop * expr
| Postop of expr * pop
| Assign of expr * expr 
(*| AssignDecl of typ * string * expr do we need this?*)
| Call of string * expr list
| Noexpr 

  type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

  type func_decl = {
    function_typ : typ;
    function_name : string;
    parameters : bind list;
    local_variables : bind list;
    code_block : stmt list;
  }


  type program = bind list * func_decl list

  (* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"
  | Mod -> "%"

let string_of_uop = function
    Not -> "!"

let string_of_pop = function
  Decr -> "--"
| Incr -> "++"

let rec string_of_expr = function
    Number(n) -> string_of_int n
  | Float(f) -> string_of_float f
  | String(s) -> s
  | Boolean(true) -> "true"
  | Boolean(false) -> "false"
  | Id(i) -> i
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Postop(e, o) -> string_of_expr e ^ string_of_pop o
  | Assign(v, e) -> 
      "@ " ^ v ^ "[" ^ string_of_expr e ^ "]"    (* ASK ABOUT THIS SHIT *)
  | Call(f, el) ->
    "@  " ^ f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "@ if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "@ if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "@ else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "@ for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "@ while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_typ = function
    Int -> "int"
  | Float -> "float"
  | String -> "string"
  | Bool -> "bool"
  | Void -> "void"

let string_of_vdecl (t, id, e) = 
  "@ dec" ^ string_of_typ t ^ " " ^ id ^ "[ " ^ string_of_expr e ^ " ]"


let string_of_fdecl fdecl =
  "@ def" string_of_typ fdecl.function_typ ^ " " ^
  fdecl.function_name ^ "(" ^ String.concat ", " (List.map snd fdecl.parameters) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.local_variables) ^
  String.concat "" (List.map string_of_stmt fdecl.code_block) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
