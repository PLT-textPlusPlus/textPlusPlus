(* Code Generation *)

module L = Llvm
module A = Ast
open Sast 

module StringMap = Map.Make(String)

let translate (globals, functions) = 
	let context    = L.global_context () in

	(* Create the LLVM compilation module into which
     we will generate code *)
	let the_module = L.create_module context "textPlusPlus" in

	(* Get types from the context *)
	let i32_t      = L.i32_type    context
	and print_t    = L.i8_type     context (* print type *)
	and bool_t     = L.i1_type     context (* bool type *)
	and float_t    = L.double_type context (* float type *)
  and void_t     = L.void_type   context (* void type *)
  in 

let (* rec *) ltype_of_typ = function (* LLVM type for AST type *)
    A.Int -> i32_t
  | A.Bool -> bool_t
  | A.Void -> void_t
 (* | A.Float -> float_t *)
  | _ -> raise(Failure("Invalid Data Type"))

in 

(* Create a map of global variables after creating each *)
let global_vars : L.llvalue StringMap.t =
	let global_var m (t, n) = 
  		let init = match t with
     		(* A.Float -> L.const_float (ltype_of_typ t) 0.0 *)
      	(*	| A.String -> L.build_global_stringptr (ltype_of_typ t) "Null" *)
      	(* | A.Bool -> const_int (ltype_of_type t)  false *)
      	(*	| A.Void -> L.builder_ret_void builder (ltype_of_typ t) "" *)
      		| A.Int -> L.const_int (ltype_of_typ t) 0 
  		in StringMap.add n (L.define_global n init the_module) m in 
		List.fold_left global_var StringMap.empty globals in


let hello_t : L.lltype =
    L.function_type i32_t [| i32_t |] in
let hello_func : L.llvalue =
    L.declare_function "hello" hello_t the_module in

(* Define each function (arguments and return type) so we can 
     call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfunction_name
      and formal_types = 
	Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sparameters)
      in let ftype = L.function_type (ltype_of_typ fdecl.sfunction_typ) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in

(* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfunction_name function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and float_format_str = L.build_global_stringptr "%g\n" "fmt" builder 
	and string_format_str = L.build_global_stringptr "%s\n" "fmt" builder 
	(*and boolean_fromat_str = L.build_global_stringptr "" "fmt" builder ***Need to fix this*)
	in

	(* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p = 
        L.set_value_name n p;
	let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
	StringMap.add n local m 

      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *)
      and add_local m (t, n) =
	let local_var = L.build_alloca (ltype_of_typ t) n builder
	in StringMap.add n local_var m 
      in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.sparameters
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.slocal_variables
    in

     (* Return the value for a variable or formal argument.
       Check local names first, then global names *)
    let lookup n = try StringMap.find n local_vars
                   with Not_found -> StringMap.find n global_vars
    in

    (* Construct code for an expression; return its value *)
    let rec expr builder ((_, e) : sexpr) = match e with
	SNumber i  -> L.const_int i32_t i
      | SBoolean b  -> L.const_int i1_type (if b then 1 else 0)
      | SFloat l -> L.const_float_of_string float_t l
      | SString l -> L.build_global_stringptr s "string" llbuilder
      | SNoexpr     -> L.const_int i32_t 0
      | SId s       -> L.build_load (lookup s) s builder
      | SAssign (v, s, e) -> let e' = expr builder e in
                          ignore(L.build_store e' (lookup s) builder); e'
(*      | SReasign (s, e) -> let e' = expr builder e in
                          ignore(L.build_store e' (lookup s) builder); e' *)
	  | SBinop ((A.Float,_ ) as e1, op, e2) ->
	  let e1' = expr builder e1
	  and e2' = expr builder e2 in
	  (match op with 
	    A.Add     -> L.build_fadd
	  | A.Sub     -> L.build_fsub
	  | A.Mult    -> L.build_fmul
	  | A.Div     -> L.build_fdiv 
	  | A.Mod 	  -> L.build_frem
	  | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
	  | A.Neq     -> L.build_fcmp L.Fcmp.One
	  | A.Less    -> L.build_fcmp L.Fcmp.Olt
	  | A.Leq     -> L.build_fcmp L.Fcmp.Ole
	  | A.Greater -> L.build_fcmp L.Fcmp.Ogt
	  | A.Geq     -> L.build_fcmp L.Fcmp.Oge
	  | A.And | A.Or ->
	      raise (Failure "internal error: semant should have rejected and/or on float")
	  ) e1' e2' "tmp" builder
      | SBinop (e1, op, e2) ->
	  let e1' = expr builder e1
	  and e2' = expr builder e2 in
	  (match op with
	    A.Add     -> L.build_add
	  | A.Sub     -> L.build_sub
	  | A.Mult    -> L.build_mul
      | A.Div     -> L.build_sdiv
	  | A.And     -> L.build_and
	  | A.Or      -> L.build_or
	  | A.Mod     -> L.build_srem
	  | A.Equal   -> L.build_icmp L.Icmp.Eq
	  | A.Neq     -> L.build_icmp L.Icmp.Ne
	  | A.Less    -> L.build_icmp L.Icmp.Slt
	  | A.Leq     -> L.build_icmp L.Icmp.Sle
	  | A.Greater -> L.build_icmp L.Icmp.Sgt
	  | A.Geq     -> L.build_icmp L.Icmp.Sge
	  ) e1' e2' "tmp" builder
	  | SUnop(op, ((t, _) as e)) ->
          let e' = expr builder e in
	  (match op with
      A.Not                  -> L.build_not) e' "tmp" builder
      | SCall ("hello", [e]) ->
    L.build_call hello_func [| (expr builder e) |] "hello" builder
      | SCall (f, args) ->
         let (fdef, fdecl) = StringMap.find f function_decls in
   let llargs = List.rev (List.map (expr builder) (List.rev args)) in
   let result = (match fdecl.sfunction_typ with 
                        A.Void -> ""
                      | _ -> f ^ "_result") in
         L.build_call fdef (Array.of_list llargs) result builder
    in


    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
	Some _ -> ()
      | None -> ignore (instr builder) in

    let rec stmt builder = function
	SBlock sl -> List.fold_left stmt builder sl
      | SExpr e -> ignore(expr builder e); builder 
      | SReturn e -> ignore(match fdecl.sfunction_typ with
                              (* Special "return nothing" instr *)
                              A.Void -> L.build_ret_void builder 
                              (* Build return statement *)
                            | _ -> L.build_ret (expr builder e) builder );
                     builder
      | SIf (predicate, then_stmt, else_stmt) ->
         let bool_val = expr builder predicate in
	 let merge_bb = L.append_block context "merge" the_function in
         let build_br_merge = L.build_br merge_bb in 

	 let then_bb = L.append_block context "then" the_function in
	 add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
	   build_br_merge;

	 let else_bb = L.append_block context "else" the_function in
	 add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
	   build_br_merge;

	 ignore(L.build_cond_br bool_val then_bb else_bb builder);
	 L.builder_at_end context merge_bb

      | SWhile (predicate, body) ->
	  let pred_bb = L.append_block context "while" the_function in
	  ignore(L.build_br pred_bb builder);

	  let body_bb = L.append_block context "while_body" the_function in
	  add_terminal (stmt (L.builder_at_end context body_bb) body)
	    (L.build_br pred_bb);

	  let pred_builder = L.builder_at_end context pred_bb in
	  let bool_val = expr pred_builder predicate in

	  let merge_bb = L.append_block context "merge" the_function in
	  ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
	  L.builder_at_end context merge_bb

      | SFor (e1, e2, e3, body) -> stmt builder
	    ( SBlock [SExpr e1 ; SWhile (e2, SBlock [body ; SExpr e3]) ] )
    in

    (* Build the code for each statement in the function *)
    let builder = stmt builder (SBlock fdecl.scode_block) in
  
  	(* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.sfunction_typ with
        A.Void -> L.build_ret_void
      | A.Float -> L.build_ret (L.const_float float_t 0.0)
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

List.iter build_function_body functions;
the_module

