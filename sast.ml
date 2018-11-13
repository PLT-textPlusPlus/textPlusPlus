(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

(* Come back to line below *)
(* Come back to line below *)
type sexpr = typ * xpr
and xpr =
    SNumber of int
  | SFloat of float
  | SString of string
  | SBoolean of bool
  | SId of string
  | SBinop of sexpr * op * sexpr
  | SUnop of uop * sexpr
  | SPostop of sexpr * pop
  | SAssign of sexpr * sexpr 
  (*| AssignDecl of typ * string * expr do we need this?*)
  | SCall of string * sexpr list
  | SNoexpr 

  type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SReturn of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SWhile of sexpr * sstmt

  type sfunc_decl = {
    sfunction_typ : typ;
    sfunction_name : string;
    sparameters : bind list;
    slocal_variables : bind list;
    scode_block : sstmt list;
  }


  type sprogram = bind list * sfunc_decl list

  (* Pretty-printing functions *)

let rec string_of_expr = function
    Number(n) -> string_of_int n
  | Float(f) -> string_of_float f
  | String(s) -> string_of_int s
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
