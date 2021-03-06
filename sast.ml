(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

(* spcaving issues with float*)
type sexpr = typ * xpr
and xpr =
    SNumber of int
| SFliteral of float
| SSliteral of string
| SBoolean of bool
| SId of string
| SNull
| SBinop of sexpr * op * sexpr
| SUnop of uop * sexpr
| SPostop of sexpr * pop
| SAssign of typ * string * sexpr 
| SReassign of string * sexpr
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

let rec string_of_sexpr (t, e) = (*Ask John about t, e *)
 "(" ^ string_of_typ t ^ " : " ^ (match e with
    SNumber(n) -> string_of_int n
  | SFliteral(f) -> string_of_float f
  | SSliteral(s) -> s
  | SBoolean(true) -> "true"
  | SBoolean(false) -> "false"
  | SNull -> "null"
  | SId(i) -> i
  | SBinop(e1, o, e2) ->
      string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
  | SUnop(o, e) -> string_of_uop o ^ string_of_sexpr e
  | SPostop(e, o) -> string_of_sexpr e ^ string_of_pop o
  | SAssign(t, v, e) -> string_of_typ t ^ " " ^ v ^ " = " ^ string_of_sexpr e
  | SReassign(v, e) -> v ^ "=" ^ string_of_sexpr e
  | SCall(f, el) -> f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  | SNoexpr -> "" 
          ) ^ ")"


let rec string_of_sstmt = function
    SBlock(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n";
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ ";\n";
  | SIf(e, s, SBlock([])) -> "if (" ^ string_of_sexpr e ^ ")\n" ^ string_of_sstmt s
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
      string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SFor(e1, e2, e3, s) ->
      "for (" ^ string_of_sexpr e1  ^ " ; " ^ string_of_sexpr e2 ^ " ; " ^
      string_of_sexpr e3  ^ ") " ^ string_of_sstmt s
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s

let string_of_svdecl (t, id) = 
    string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_sfdecl fdecl =
  "def " ^ string_of_typ fdecl.sfunction_typ ^ " " ^
  fdecl.sfunction_name ^ "(" ^ String.concat ", " (List.map snd fdecl.sparameters) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_svdecl fdecl.slocal_variables) ^
  String.concat "" (List.map string_of_sstmt fdecl.scode_block) ^
  "}\n"

let string_of_sprogram (vars, funcs) =
  String.concat "" (List.map string_of_svdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs)
