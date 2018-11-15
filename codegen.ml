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
    and str_t      = L.pointer_type (L.i8_type context) (* string type*)
in 

let rec ltype_of_typ = function (* LLVM type for AST type *)
    A.Int -> i32_t
  | A.String -> str_t
  | A.Bool -> bool_t
  | A.Void -> void_t
  | A.Float -> float_t
  | _ -> raise(Failure("Invalid Data Type"))

in 

(* Create a map of global variables after creating each *)
let global_vars : L.llvalue StringMap.t =
	let global_var m (t, n) = 
  		let init = match t with
      		A.Float -> L.const_float (ltype_of_typ t) 0.0
      		| A.String -> L.build_global_stringptr (ltype_of_typ t) "string"
      		| A.Bool -> const_int (ltype_of_type t)  false 
      		| A.Void -> L.builder_ret_void builder (ltype_of_typ t) ""
      		| A.Int -> L.const_int (ltype_of_typ t) 0
  		in StringMap.add n (L.define_global n init the_module) m in
		List.fold_left global_var StringMap.empty globals in

let printf_t : L.lltype = 
      L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue = 
      L.declare_function "printf" printf_t the_module in

  let printbig_t : L.lltype =
      L.function_type i32_t [| i32_t |] in
  let printbig_func : L.llvalue =
      L.declare_function "printbig" printbig_t the_module in

(* Define each function (arguments and return type) so we can 
     call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfunction_name
      and formal_types = 
	Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sparameters)
      in let ftype = L.function_type (ltype_of_typ fdecl.sfunction_type) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in

(* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfunction_name function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and float_format_str = L.build_global_stringptr "%g\n" "fmt" builder 
	and string_fromat_str = L.build_global_stringptr "%s\n" "fmt" builder 
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
      | SBoolean b  -> L.const_int i1_t (if b then 1 else 0)
      | SFloat l -> L.const_float_of_string float_t l
      | SString l -> L.build_global_stringptr s "string" llbuilder
      | SNoexpr     -> L.const_int i32_t 0
      | SId s       -> L.build_load (lookup s) s builder
      | SAssign (s, e) -> let e' = expr builder e in
                          ignore(L.build_store e' (lookup s) builder); e'
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
      	| A.Not   -> L.build_not) e' "tmp" builder
	  
      | SPostop (e, op) ->
          let e' = expr builder e in
          let val = (match e with
            SId(s) -> s
          | _ -> raise (Failure("Value cannot be incremented or decremented"))
          )
          and op_typ = (match op with
            A.Incr -> A.Add
          | A.Decr -> A.Sub
          )
          and num_typ = if ((ltype_of_typ e' = float_t))
          then A.Float(1.0)
          else A.Int(1) in

          expr builder (A.Assign(val, A.Binop(e, op_typ, num_typ)))





let build_function_body fdecl = (* Something *)
 in 


(*Something*)

List.iter build_function_body functions;
  the_module















(* Create a map of global variables after creating each 
  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) = 
      let init = match t with
          A.Float -> L.const_float (ltype_of_typ t) 0.0
        | _ -> L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals *) 


(* (* LLVM Types *)
    and i1_t   = L.i1_type context
    and i32_t  = L.i32_type context
    and i8_t   = L.i8_type context
    and b_t    = L.i8_type context
    and str_t  = L.pointer_type (L.i8_type context)
    and flt_t  = L.double_type context
    and void_t = L.void_type context in 

in

let printf_t : L.lltype = 
      L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
let printf_func : L.llvalue = 
      L.declare_function "printf" printf_t the_module in

let printbig_t : L.lltype =
      L.function_type i32_t [| i32_t |] in
let printbig_func : L.llvalue =
      L.declare_function "printbig" printbig_t the_module in

(* Declare printf(), which the print built-in function will call *)
let printf_t = L.var_arg_function_type i32_t [| str_t |] in
let printf_func = L.declare_function "printf" printf_t the_module in
*)










