(* Semantic Checker for text++ Programming Language *)

open Ast
(* open Sast*)

module StringMap = Map.Make(String)

(* Checks the globals and function in program *)
let check (globals, functions) = 

  (* Function to check for duplicate function names *)
  let rec dups = function
  [] -> ()
|	((_,n1) :: (_,n2) :: _) when n1 = n2 ->
raise (Failure ("Duplicate variable " ^ n1))
| _ :: t -> dups t   
in 

(* Calls the duplicates (dups) function on globals *)
dups (List.sort (fun (_,a) (_,b) -> compare a b) globals);;

(* Adds built in functions to StringMap *)

let built_in_functions =  

  StringMap.add "prints"
  { function_typ = Void;
    function_name = "prints";
    parameters = [(String, "x")];
    local_variables = [];
    code_block = [] }

  (StringMap.add "printi"
  { function_typ = Void;
    function_name = "printi";
    parameters = [(Int, "x")];
    local_variables = [];
    code_block = [] }

  (StringMap.add "printf"
   { function_typ = Void;
     function_name = "printf";
     parameters = [(Float, "x")];
     local_variables = [];
     code_block = [] }

  (StringMap.add "printb"
  { function_typ = Void;
    function_name = "printb";
    parameters = [(Boolean, "x")];
    local_variables = [];
    code_block = [] }


    StringMap.empty
  ))) 

  in 

  (* Add function name to symbol table *)

  let add_func map fd = 
    let built_in_err = "" ^ fd.function_name ^ " is a built in function. Cannot be defined."
    and dup_err = "" ^ fd.function_name ^ " already exits. Cannot be redefined."
    and make_err er = raise (Failure er)
    and n = fd.function_name (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
         _ when StringMap.mem n built_in_functions -> make_err built_in_err
       | _ when StringMap.mem n map -> make_err dup_err  
       | _ ->  StringMap.add n fd map  (*  if no errors, adds function name symbol map*)
  in

  (* Collect all function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_functions functions

  in

  (* Return a function from our symbol table *)
  let find_func s = 
    (* Checks to see if function is in function declaration map *)
    try StringMap.find s function_decls
    (* Throws an error if not found *)
    with Not_found -> raise (Failure ("Function undefined " ^ s))

  in

  let _ = function_decl "main" in (* Ensure "main" is defined *)

  let check_function func =
    (* Make sure no formals or locals already defined  *)
    dups "formal" func.parameters;
    dups "local" func.local_variables;


    (* Build local symbol table of variables for this function *)
    let symbol_table = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
	                StringMap.empty (globals @ func.parameters )
    in

    (* Return a variable from our local symbol table *)
    let type_of_identifier s =
      try StringMap.find s symbol_table
      with Not_found -> raise (Failure ("Variable is undeclared " ^ s))

    in

    let check_assign lvaluet rvaluet err =
       if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in   

    (* Return a semantically-checked expression, i.e., with a type *)
    let rec expr = function

        Number  l -> (Int, SNumber l)
      | Float l -> (Float, SFloat l)
      | Boolean l  -> (Bool, SBoolean l)
      | String l -> (String, SString)
      | Noexpr     -> (Void, SNoexpr)
      | Id s       -> (type_of_identifier s, SId s)
      | Noexpr -> Void
      | Assign(id, expression) as ex -> 
          let lt = type_of_identifier id
          and (rt, expression') = expr expression in
          let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^ 
            string_of_typ rt ^ " in " ^ string_of_expr ex
          in (check_assign lt rt err, SAssign(id, (rt, expression')))

      | Binop(expression1, op, expression2) as e -> 
          let (t1, expression1') = expr expression1
           and (t2, expression2') = expr expression2 in
          (* Ask Edwards about how to make it such that you can add
          strings and numbers together// also floats and integers *)
          let same = t1 = t2 in
          (* Determine expression type based on operator and operand types *)
          let ty = match op with
            Add | Sub | Mult | Div | Mod  when same && t1 = Int   -> Int
          | Add | Sub | Mult | Div | Mod when same && t1 = Float -> Float
          | Equal | Neq   when same  -> Bool
          | Less | Leq | Greater | Geq
                     when same && (t1 = Int || t1 = Float) -> Bool
          | And | Or when same && t1 = Bool -> Bool
          | _ -> raise (
	      Failure ("illegal binary operator " ^
                       string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                       string_of_typ t2 ^ " in " ^ string_of_expr e))
          in (ty, SBinop((t1, e1'), op, (t2, e2')))

      | Unop(op, e) as ex -> 
          let (t, e') = expr e in
          let ty = match op with
            Neg when t = Int || t = Float -> t
          | Not when t = Bool -> Bool
          | _ -> raise (Failure ("illegal unary operator " ^ 
                                 string_of_uop op ^ string_of_typ t ^
                                 " in " ^ string_of_expr ex))
          in (ty, SUnop(op, (t, e')))

      | Postop (e, op) as p ->
        let (t, e')  = expr e in
          let ty = match op with
          Incr when t = Int -> Int
        | Decr when t = Int -> Int
        | _ -> raise (Failure("illegal postunary operator " ^
              string_of_pop op ^ " on " ^ string_of_expr p)))

      | Call(fname, args) as call -> 
          let fd = find_func fname in
          let param_length = List.length fd.formals in
          if List.length args != param_length then
            raise (Failure ("expecting " ^ string_of_int param_length ^ 
                            " arguments in " ^ string_of_expr call))
          else let check_call (ft, _) e = 
            let (et, e') = expr e in 
            let err = "illegal argument found " ^ string_of_typ et ^
              " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
            in (check_assign ft et err, e')
          in 
          let args' = List.map2 check_call fd.formals args
          in (fd.typ, SCall(fname, args'))
    in
