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
  let function_decls = List.fold_left add_func built_in_decls functions
