(* Semantic Checker for text++ Programming Language *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Checks the globals and function in program *)
let check (globals, functions) = 

  (* Function to check for duplicate function names *)
  let check_binds (kind : string) (binds : bind list) =

    List.iter (function
  (Void, b) -> raise (Failure ("illegal void " ^ kind ^ " " ^ b))
      | _ -> ()) binds;
    let rec dups = function
        [] -> ()
      | ((_,n1) :: (_,n2) :: _) when n1 = n2 ->
    raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
  in

  (**** Check global variables ****)

  check_binds "global" globals;


(* Adds built in functions to StringMap *)

(* Collect function declarations for built-in functions: no bodies *)
  let built_in_functions = 
    let add_bind map (name, ty) = StringMap.add name {
      function_typ = Void;
      function_name = name; 
      parameters = [(ty, "x")];
      local_variables = []; code_block = [] } map
    in List.fold_left add_bind StringMap.empty [ ("hello", Int);
  ]
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

  let _ = (* function_decls *) find_func "main" in (* Ensure "main" is defined *)

  let check_function func =

    (* Make sure no formals or locals already defined  *)
    check_binds "formal" func.parameters;
    check_binds "local" func.local_variables;


    let check_assign lvaluet rvaluet err =
       if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in   


    (* Build local symbol table of variables for this function *)
    let symbol_table = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
	                StringMap.empty (globals @ func.parameters )
    in


    (* Return a variable from our local symbol table *)
    let type_of_identifier s =
      try StringMap.find s symbol_table
      with Not_found -> raise (Failure ("Variable is undeclared " ^ s))

    in
  
   (*let check_var_decl var_name err =
      if StringMap.mem var_name (!variables)
      then raise err
    in *)


    (* Return a semantically-checked expression, i.e., with a type *)
    let rec expr = function

        Number  l -> (Int, SNumber l)
      | Fliteral l -> (Float, SFliteral l)
      | Boolean l  -> (Bool, SBoolean l)
      | Sliteral l -> (String, SSliteral l)
      | Noexpr     -> (Void, SNoexpr)
      | Id s       -> (type_of_identifier s, SId s)
      | Assign(typ, var, e) as ex ->
         let lt = type_of_identifier var
          and (rt, e') = expr e in
          let err = "illegal assignment " ^ string_of_typ typ ^ " = " ^ 
            string_of_typ rt ^ " in " ^ string_of_expr ex
          in (check_assign typ rt err, SAssign(typ, var, (rt, e')))

     (* | Reassign(var, e) as ex ->
          let rt = expr e and lt = type_of_identifier var in
          check_assign lt rt (Failure ("illegal assignment " ^ string_of_typ lt ^ 
                              " = " ^ string_of_typ rt ^ " in " ^ string_of_expr ex))
      *)
      | Binop(e1, op, e2) as e -> 
          let (t1, e1') = expr e1 
          and (t2, e2') = expr e2 in
          (* All binary operators require operands of the same type *)
          let same = t1 = t2 in
          (* Determine expression type based on operator and operand types *)
          let ty = match op with
            Add | Sub | Mult | Div | Mod when same && t1 = Int   -> Int
          | Add | Sub | Mult | Div when same && t1 = Float -> Float
          | Equal | Neq            when same               -> Bool
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
           Not when t = Bool -> Bool
          | _ -> raise (Failure ("illegal unary operator " ^ 
                                 string_of_uop op ^ string_of_typ t ^
                                 " in " ^ string_of_expr ex))
          in (ty, SUnop(op, (t, e')))

 (*     | Postop (e, op) as p ->
>>>>>>> 910c7a475c0d01de2af602bd4366c2a2d4021874
        let (t, e')  = expr e in
          let ty = match op with
          Incr when t = Int -> Int
        | Decr when t = Int -> Int
        | _ -> raise (Failure("illegal postunary operator " ^
              string_of_pop op ^ " on " ^ string_of_expr p)) *)

      | Call(function_name, parameters) as call -> 
          let fd = find_func function_name in
          let param_length = List.length fd.parameters in
          if List.length parameters != param_length then
            raise (Failure ("Incorrect number of parameters. Expected " ^ string_of_int param_length ^ 
                            " arguments in " ^ string_of_expr call))
          else let check_call (ft, _) e = 
            let (et, e') = expr e in 
            let err = "illegal argument found " ^ string_of_typ et ^
              " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
            in (check_assign ft et err, e')
          in 
          let parameters' = List.map2 check_call fd.parameters parameters
          in (fd.function_typ, SCall(function_name, parameters')) 
    in

    let check_bool_expr e = 
      let (t', e') = expr e
      and err = "expected Boolean expression in " ^ string_of_expr e
      in if t' != Bool then raise (Failure err) else (t', e') 
    in

    (* Return a semantically-checked statement i.e. containing sexprs *)
    let rec check_stmt = function

        Expr e -> SExpr (expr e)

      | If(p, b1, b2) -> 
        SIf(check_bool_expr p, check_stmt b1, check_stmt b2)

      | For(e1, e2, e3, st) ->
        SFor( expr e1, check_bool_expr e2, expr e3, check_stmt st)

      | While(p, s) -> SWhile(check_bool_expr p, check_stmt s)

      | Return e -> let (t, e') = expr e in
        if t = func.function_typ then SReturn (t, e') 
          else raise ( Failure ("Incorrect return type. Return gives " ^ string_of_typ t ^ " expected " ^
          string_of_typ func.function_typ ^ " in " ^ string_of_expr e))

      | Block sl -> 
          let rec check_stmt_list = function
              [Return _ as s] -> [check_stmt s]
            | Return _ :: _   -> raise (Failure "nothing may follow a return")
            | Block sl :: ss  -> check_stmt_list (sl @ ss) (* Flatten blocks *)
            | s :: ss         -> check_stmt s :: check_stmt_list ss
            | []              -> []
          in SBlock(check_stmt_list sl)

    in (* body of check_function *)
    { sfunction_typ = func.function_typ;
      sfunction_name = func.function_name;
      sparameters = func.parameters;
      slocal_variables  = func.local_variables;
      scode_block = match check_stmt (Block func.code_block) with
  SBlock(sl) -> sl
      | _ -> raise (Failure ("internal error: block didn't become a block?"))
    }

  in (globals, List.map check_function functions)
