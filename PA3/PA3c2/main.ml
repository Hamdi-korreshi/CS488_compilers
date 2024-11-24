(* Keywords:
TODO
generating *)
open Printf
type static_type =  (*static type of cool expression*)
  | Class of string
  | SELF_TYPE of string

type tac_expr =
  | TAC_Variable of string
  | TAC_Int of int
  | TAC_String of string
  | TAC_Bool of bool

let metho_count = ref 0 
let safe_head lst =
  try Some (List.hd (List.rev lst))
  with Failure _ -> None

type cool_prog = cool_class list
and loc = string
and id = loc * string
and cool_type = id
and cool_class = id * (id option) * feature list
and feature =
  | Attribute of id * cool_type * (exp option)
  | Method of id * (formal list) * cool_type * exp
and formal = id * cool_type
and exp = 
  {
            loc:loc; 
            exp_kind: exp_kind;
    mutable static_type: static_type option; (*mutable means can change later on,
    every exp has this mutable static type, will uncover using the typechecking*)
  }
and case = id * id * exp
and exp_kind =
  | Integer of string (* doesn't need to be an Int until the next PA *)
  | Bool of string
  | String of string
  | Plus of exp * exp
  | Minus of exp * exp
  | Times of exp * exp
  | Divide of exp * exp
  | Let of (id * id * (exp option)) list * exp
  | New of cool_type
  | Block of exp list
  | Identifier of id
  | Case of exp * case list
  | If of exp * exp * exp
  | While of exp * exp
  | Isvoid of exp
  | Assign of id * exp
  | Dynamic_Dispatch of exp * id * exp list
  | Static_Dispatch of exp * cool_type *id * exp list
  | Self_Dispatch of id * exp list
  | LT of exp * exp
  | LE of exp * exp
  | EQ of exp * exp
  | Negate of exp
  | Not of exp
  | Internal of cool_type * string * string

(* class_map type = hashtable of class_map *)
type class_map = (string, (string * string * exp option) list option) Hashtbl.t
type implementation_map = (loc, (loc * (loc) list * loc * exp) list) Hashtbl.t
type parent_map = (loc,loc) Hashtbl.t

let empty_map () = Hashtbl.create 128

type tac_instr =
| TAC_Assign_Int of string * int
| TAC_Assign_Bool of string * bool
| TAC_Assign_String of string * string
| TAC_Assign_Var of string * string
| TAC_Assign_Plus of string * tac_expr * tac_expr
| TAC_Assign_Minus of string * tac_expr * tac_expr
| TAC_Assign_Times of string * tac_expr * tac_expr
| TAC_Assign_Divide of string * tac_expr * tac_expr
| TAC_Cnd_LessThan of string * tac_expr * tac_expr
| TAC_Cnd_LessEqual of string * tac_expr * tac_expr
| TAC_Cnd_Equal of string * tac_expr * tac_expr
| TAC_Cnd_Not of string * tac_expr
| TAC_Negate of string * tac_expr
| TAC_New of string * tac_expr
| TAC_Default of tac_expr * string
| TAC_isvoid of string * tac_expr
| TAC_call_out of string * string * string (* out_string, out_int *)
| TAC_call_in of string * string (* in_string, in_int *)
| TAC_Let of (string * string * tac_expr option ) list * tac_expr
| TAC_Jump of string              (* Unconditional jump to a label *)
| TAC_Jump_If_Not of tac_expr * string  (* Conditional jump if expr is false *)
| TAC_Label of string 
| TAC_Self_Dispatch of string * id * tac_expr list
| TAC_Return of string
let match_exp exp1 = 
  let jack = 
    (match exp1 with
              | TAC_Variable v -> v
              | TAC_Int i -> "int " ^ string_of_int i 
              | TAC_Bool i -> "bool " ^ string_of_bool i 
              | TAC_String i -> "string\n" ^ i 
     ) in 
    jack

let debug tac = 
  match tac with
    | Some (TAC_Assign_Int (var, _)
          | TAC_Assign_Bool (var, _) 
          | TAC_Assign_Var (var, _)
          | TAC_Assign_Plus (var, _, _)
          | TAC_Assign_Minus (var, _, _)
          | TAC_Assign_Times (var, _, _)
          | TAC_Assign_Divide (var, _, _)
          | TAC_Cnd_LessThan (var, _, _)
          | TAC_Cnd_LessEqual (var, _, _)
          | TAC_Cnd_Equal (var, _, _)
          | TAC_Cnd_Not (var, _)
          | TAC_Negate (var, _)
          | TAC_New (var, _)
          | TAC_isvoid (var, _)
          | TAC_call_out (var, _, _)
          | TAC_call_in (var, _)) ->
        printf "return %s\n" var
    | _ -> ()

let output_id (loc, name) =
  printf "%s\n%s\n" loc name

let rec output_exp exp =
  printf "%s\n" exp.loc;
  (match exp.static_type with
  | None -> failwith "Expression missing static type"
  | Some (Class c) -> printf "%s\n" c
  | Some (SELF_TYPE _) -> printf "SELF_TYPE\n");
  match exp.exp_kind with
  | Integer ival -> printf "integer\n%s\n" ival
  | String sval -> printf "string\n%s\n" sval
  | Bool(ival) ->  (
              match ival with 
              | "true" -> 
                printf "bool\ntrue\n"
              | "false" -> 
                printf "bool\nfalse\n"
              | _ ->  (printf ""))
  | Plus (e1, e2) ->
      printf "plus\n";
      output_exp e1;
      output_exp e2
  | Minus (e1, e2) ->
      printf "minus\n";
      output_exp e1;
      output_exp e2
  | Times (e1, e2) ->
      printf "times\n";
      output_exp e1;
      output_exp e2
  | Divide (e1, e2) ->
      printf "divide\n";
      output_exp e1;
      output_exp e2
  | Let (bindings, let_body) ->
      printf "let\n";
      printf "%d\n" (List.length bindings);
      List.iter (fun (let_vare, var_type, init_opt) ->
        match init_opt with
        | None ->
            printf "let_binding_no_init\n";
            output_id let_vare;
            output_id var_type
        | Some init_exp ->
            printf "let_binding_init\n";
            output_id let_vare;
            output_id var_type;
            output_exp init_exp
      ) bindings;
      output_exp let_body
  | Block exprs ->
      printf "block\n";
      printf "%d\n" (List.length exprs);
      List.iter output_exp exprs
  | Case (test_exp, case_list) ->
      printf "case\n";
      output_exp test_exp;
      printf "%d\n" (List.length case_list);
      List.iter (fun (var, var_type, case_body) ->
        output_id var;
        output_id var_type;
        output_exp case_body
      ) case_list
  | Identifier id ->
      printf "identifier\n";
      output_id id
  | New id ->
      printf "new\n";
      output_id id
  | If (if_exp, then_exp, else_exp) ->
      printf "if\n";
      output_exp if_exp;
      output_exp then_exp;
      output_exp else_exp
  | While (loop_cond, loop_body) ->
      printf "while\n";
      output_exp loop_cond;
      output_exp loop_body
  | Assign (var, rhs_exp) ->
      printf "assign\n";
      output_id var;
      output_exp rhs_exp
  | Isvoid exp ->
      printf "isvoid\n";
      output_exp exp
  | Dynamic_Dispatch (e, metho, args) ->
      printf "dynamic_dispatch\n";
      output_exp e;
      output_id metho;
      printf "%d\n" (List.length args);
      List.iter output_exp args
  | Static_Dispatch (e, ftype, metho, args) ->
      printf "static_dispatch\n";
      output_exp e;
      output_id ftype;
      output_id metho;
      printf "%d\n" (List.length args);
      List.iter output_exp args
  | Self_Dispatch (metho, args) ->
      printf "self_dispatch\n";
      output_id metho;
      printf "%d\n" (List.length args);
      List.iter output_exp args
  | LT (e1, e2) ->
      printf "lt\n";
      output_exp e1;
      output_exp e2
  | LE (e1, e2) ->
      printf "le\n";
      output_exp e1;
      output_exp e2
  | EQ (e1, e2) ->
      printf "eq\n";
      output_exp e1;
      output_exp e2
  | Negate e ->
      printf "negate\n";
      output_exp e
  | Not e ->
      printf "not\n";
      output_exp e
  | Internal ((type_loc, typ_name), class_name, metho_name) ->
      printf "internal\n";
      printf "%s.%s\n" class_name metho_name

let fresh_variable =
  let counter = ref 0 in
  fun () ->
    let var_name = "t" ^ string_of_int !counter in
    incr counter;
    var_name


let rec convert_id (loc,iname) =
  [], TAC_Variable iname


(* let rec convert_expr (e : exp) : (tac_instr list * tac_expr) =
  (* TODO: match method *)
  match e.exp_kind with
  | Integer value ->
      let new_var = fresh_variable () in
      let int_value = int_of_string value in
      [TAC_Assign_Int (new_var, int_value)], TAC_Variable new_var

  | Identifier (_, name) ->
      [], TAC_Variable name

  | Plus (e1, e2) ->
      let instrs1, temp1 = convert_expr e1 in
      let instrs2, temp2 = convert_expr e2 in
      let new_var = fresh_variable () in
      let to_output = TAC_Assign_Plus (new_var, temp1, temp2) in
      instrs1 @ instrs2 @ [to_output], TAC_Variable new_var

  | Minus (e1, e2) ->
      let instrs1, temp1 = convert_expr e1 in
      let instrs2, temp2 = convert_expr e2 in
      let new_var = fresh_variable () in
      let to_output = TAC_Assign_Minus (new_var, temp1, temp2) in
      instrs1 @ instrs2 @ [to_output], TAC_Variable new_var

  | Times (e1, e2) ->
      let instrs1, temp1 = convert_expr e1 in
      let instrs2, temp2 = convert_expr e2 in
      let new_var = fresh_variable () in
      let to_output = TAC_Assign_Times (new_var, temp1, temp2) in
      instrs1 @ instrs2 @ [to_output], TAC_Variable new_var

  | Divide (e1, e2) ->
      let instrs1, temp1 = convert_expr e1 in
      let instrs2, temp2 = convert_expr e2 in
      let new_var = fresh_variable () in
      let to_output = TAC_Assign_Divide (new_var, temp1, temp2) in
      instrs1 @ instrs2 @ [to_output], TAC_Variable new_var
  | LT (e1, e2) ->
    let instrs1, temp1 = convert_expr e1 in
    let instrs2, temp2 = convert_expr e2 in
    let new_var = fresh_variable () in
    let to_output = TAC_Cnd_LessThan (new_var, temp1, temp2) in
    instrs1 @ instrs2 @ [to_output], TAC_Variable new_var
  | LE (e1, e2) ->
    let instrs1, temp1 = convert_expr e1 in
    let instrs2, temp2 = convert_expr e2 in
    let new_var = fresh_variable () in
    let to_output = TAC_Cnd_LessEqual (new_var, temp1, temp2) in
    instrs1 @ instrs2 @ [to_output], TAC_Variable new_var

  | EQ (e1, e2) ->
    let instrs1, temp1 = convert_expr e1 in
    let instrs2, temp2 = convert_expr e2 in
    let new_var = fresh_variable () in
    let to_output = TAC_Cnd_Equal (new_var, temp1, temp2) in
    instrs1 @ instrs2 @ [to_output], TAC_Variable new_var
  | Not (e1) -> 
    let instrs1, temp1 = convert_expr e1 in
    let new_var = fresh_variable () in
    let to_output = TAC_Cnd_Not (new_var, temp1) in
    instrs1 @ [to_output], TAC_Variable new_var
  
  | Negate (e1) ->
    let instrs1, temp1 = convert_expr e1 in
    let new_var = fresh_variable () in
    let to_output = TAC_Negate (new_var, temp1) in
    instrs1 @ [to_output], TAC_Variable new_var

  | New (e1) ->
    (* TODO *)
    let instrs1, temp1 = convert_id e1 in
    let new_var = fresh_variable () in
    let to_output = TAC_New (new_var, temp1) in
    instrs1 @ [to_output], TAC_Variable new_var

  | _ -> [], TAC_Variable "" *)
type attr_table = (string, string) Hashtbl.t 
(* let is_digit num = 
  let jack = Str.string_match (Str.regexp "123456789") num 0 in
  jack  *)

type var_to_rsp = (string, string) Hashtbl.t (* variable name, rsp w/ offset*)
type var_to_reg = (string, string) Hashtbl.t (* variable name, register name *)
let curr_rsp_max_offset = ref 0 (* The current method's stack's allocation for vars *)
(*  Note
Use stack for local vars
any arguments for a function will be the according regs (rdi, rsi, etc. until six then think from there)
Assign the last / method in the method to %rax then pop off the stack to release the stack allocation
Refer to the takeout.s for the correct placement of .globl Main.main / Main.main.end and Main.main in accordance to asm
We might be able to change the positioning of these tables and still be fine
*)

let print_cool_type (loc, tname) =
  Printf.printf "Cool_Type (location: %s, type: %s)\n" loc tname
let rec print_id (loc, name) =
  Printf.printf "ID (location: %s, name: %s)\n" loc name

let metho_count = ref 0
let curr_class = ref ""
let curr_method = ref ""
let main () = begin
  (* printf "start main \n"; *)
  (*deserialzing the CL-AST file*)
  let fname = Sys.argv.(1) in
  let fin = open_in fname in

  let read () =
    input_line fin (* may need to make it cross compatibile*)
  in

  (*many mutually-recursive procedures to rea d in the CL-AST file *)
          let rec read_exp read =
            (* Read location *)
            let eloc = read () in
            let stat =
              match read () with
              | "SELF_TYPE" -> Some (SELF_TYPE "SELF_TYPE") (* Replace "SELF_TYPE" with context if needed *)
              | typ -> Some (Class typ)
            in
            let ekind =
              match read () with
              | "integer" ->
                  let ival = read () in
                  Integer ival
              | "string" ->
                  let sval = read () in
                  String sval
              | "bool" ->
                  let bval = read () in
                  Bool bval
              | "plus" ->
                  let left = read_exp read in
                  let right = read_exp read in
                  Plus (left, right)
              | "times" ->
                  let left = read_exp read in
                  let right = read_exp read in
                  Times (left, right)
              | "divide" ->
                  let left = read_exp read in
                  let right = read_exp read in
                  Divide (left, right)
              | "minus" ->
                  let left = read_exp read in
                  let right = read_exp read in
                  Minus (left, right)
              | "let" ->
                  let num_bindings = int_of_string (read ()) in
                  let bindings =
                    let rec parse_bindings n acc =
                      if n = 0 then List.rev acc
                      else
                        let binding_type = read () in
                        let binding =
                          match binding_type with
                          | "let_binding_no_init" ->
                              let let_var = (read (), read ()) in
                              let var_type = (read (), read ()) in
                              (let_var, var_type, None)
                          | "let_binding_init" ->
                              let let_var = (read (), read ()) in
                              let var_type = (read (), read ()) in
                              let init_exp = read_exp read in
                              (let_var, var_type, Some init_exp)
                          | _ -> failwith ("Invalid let binding type: " ^ binding_type)
                        in
                        parse_bindings (n - 1) (binding :: acc)
                    in
                    parse_bindings num_bindings []
                  in
                  let let_body = read_exp read in
                  Let (bindings, let_body)
              | "block" ->
                  let num_exps = int_of_string (read ()) in
                  let rec parse_block n acc =
                    if n = 0 then List.rev acc
                    else
                      let expr = read_exp read in
                      parse_block (n - 1) (expr :: acc)
                  in
                  let exprs = parse_block num_exps [] in
                  Block (exprs)
              | "case" ->
                  let test_exp = read_exp read in
                  let num_cases = int_of_string (read ()) in
                  let rec parse_cases n acc =
                    if n = 0 then List.rev acc
                    else
                      let var = (read (), read ()) in
                      let var_type = (read (), read ()) in
                      let case_body = read_exp read in
                      parse_cases (n - 1) ((var, var_type, case_body) :: acc)
                  in
                  let cases = parse_cases num_cases [] in
                  Case (test_exp, cases)
              | "identifier" ->
                  let id = (read (), read ()) in
                  Identifier id
              | "new" ->
                  let id = (read (), read ()) in
                  New id
              | "if" ->
                  let if_exp = read_exp read in
                  let then_exp = read_exp read in
                  let else_exp = read_exp read in
                  If (if_exp, then_exp, else_exp)
              | "while" ->
                  let loop_exp = read_exp read in
                  let pool_exp = read_exp read in
                  While (loop_exp, pool_exp)
              | "assign" ->
                  let var = (read (), read ()) in
                  let rhs = read_exp read in
                  Assign (var, rhs)
              | "isvoid" ->
                  let isvoid_exp = read_exp read in
                  Isvoid (isvoid_exp)
              | "dynamic_dispatch" ->
                  let expr = read_exp read in
                  let metho = (read (), read ()) in
                  let num_args = int_of_string (read ()) in
                  let rec parse_args n acc =
                    if n = 0 then List.rev acc
                    else
                      let arg = read_exp read in
                      parse_args (n - 1) (arg :: acc)
                  in
                  let args = parse_args num_args [] in
                  Dynamic_Dispatch (expr, metho, args)
              | "static_dispatch" ->
                  let expr = read_exp read in
                  let ftype = (read (), read ()) in
                  let metho = (read (), read ()) in
                  let num_args = int_of_string (read ()) in
                  let rec parse_args n acc =
                    if n = 0 then List.rev acc
                    else
                      let arg = read_exp read in
                      parse_args (n - 1) (arg :: acc)
                  in
                  let args = parse_args num_args [] in
                  Static_Dispatch (expr, ftype, metho, args)
              | "self_dispatch" ->
                  let metho = (read (), read ()) in
                  let num_args = int_of_string (read ()) in
                  let rec parse_args n acc =
                    if n = 0 then List.rev acc
                    else
                      let arg = read_exp read in
                      parse_args (n - 1) (arg :: acc)
                  in
                  let args = parse_args num_args [] in
                  Self_Dispatch (metho, args)
              | "lt" ->
                  let left = read_exp read in
                  let right = read_exp read in
                  LT (left, right)
              | "le" ->
                  let left = read_exp read in
                  let right = read_exp read in
                  LE (left, right)
              | "eq" ->
                  let left = read_exp read in
                  let right = read_exp read in
                  EQ (left, right)
              | "negate" ->
                  let negate_exp = read_exp read in
                  Negate (negate_exp)
              | "not" ->
                  let not_exp = read_exp read in
                  Not (not_exp)
              | "internal" ->
                  let dot_name = read () in
                  let split_dot name = 
                    (match String.split_on_char '.' name with 
                    | [class_na;metho_na] -> (class_na,metho_na)
                    | _ -> failwith "something went wrong with the internal definiton")
                  in 
                  let class_name, metho_name = split_dot dot_name in
                  Internal (("NOT IMPORTANT", "NOT IMPORTANT"), class_name, metho_name)
              | x -> failwith ("Unrecognized expression kind " ^ x)
            in
            {
              loc = eloc;
              exp_kind = ekind;
              static_type = stat;
            }
            in
            let read_class_map =
              let class_map : class_map = Hashtbl.create 10 in
              let _ = read () in
              let num_classes = int_of_string (read ()) in
              for _ = 1 to num_classes do
                let class_name = read () in
                let num_attributes = int_of_string (read ()) in
                let attributes =
                  if num_attributes = 0 then None
                  else
                    let rec parse_attributes n acc =
                      if n = 0 then List.rev acc
                      else
                        let attr_initializer_flag = read () in
                        let attr =
                          match attr_initializer_flag with
                          | "initializer" ->
                              let aname = read () in
                              let atype = read () in
                              let init_exp = read_exp read in
                              (aname, atype, Some init_exp)
                          | "no_initializer" ->
                              let aname = read () in
                              let atype = read () in
                              (aname, atype, None)
                          | x -> failwith ("Invalid attribute flag: " ^ x)
                        in
                        parse_attributes (n - 1) (attr :: acc)
                    in
                    Some (parse_attributes num_attributes [])
                in
                Hashtbl.add class_map class_name attributes
              done;
              class_map
          in
          let read_implementation_map =
            let implementation_map: implementation_map = Hashtbl.create 128 in
            let _ = read () in
            let num_classes = int_of_string (read ()) in
            for _ = 1 to num_classes do
              let class_name = read () in
              let num_methods = int_of_string (read ()) in
              let rec parse_methods n acc =
                if n = 0 then List.rev acc
                else
                  let method_name = read () in
                  let num_formals = int_of_string (read ()) in
                  let rec parse_formals f acc_formals =
                    if f = 0 then List.rev acc_formals
                    else
                      let formal_name = read () in
                      parse_formals (f - 1) ((formal_name) :: acc_formals)
                  in
                  let formals = parse_formals num_formals [] in
                  let defined_in_class = read () in
                  let body_exp = read_exp read in
                  parse_methods (n - 1) ((method_name, formals, defined_in_class, body_exp) :: acc)
              in
              let methods = parse_methods num_methods [] in
              Hashtbl.add implementation_map class_name methods
            done;
            implementation_map  
          in 
          let read_parent_map = 
            let _ = read () in 
            let pmap: parent_map = Hashtbl.create 128 in
            let num_classes = int_of_string (read ()) in 
            let rec grab_parents n = 
              (if n <= 0 then ()
              else begin
                let child = read () in 
                let parent = read () in 
                Hashtbl.add pmap child parent;
                grab_parents (n-1)
              end;)
            in
            grab_parents num_classes;
            pmap
          in
          let rec read_cool_prog () =
            let num_classes = int_of_string (read ()) in
            let rec read_classes n acc =
              if n = 0 then List.rev acc
              else
                let class_name = read_id () in
                let parent_opt =
                  match read () with
                  | "no_inherits" -> None
                  | "inherits" -> Some (read_id ())
                  | _ -> failwith "Invalid inheritance specifier"
                in
                let num_features = int_of_string (read ()) in
                let features = read_features num_features [] in
                read_classes (n - 1) ((class_name, parent_opt, features) :: acc)
            in
            read_classes num_classes []
          
          and read_features n acc =
            if n = 0 then List.rev acc
            else
              let feature =
                match read () with
                | "attribute_no_init" ->
                    let attr_name = read_id () in
                    let attr_type = read_id () in
                    Attribute(attr_name, attr_type, None)
                | "attribute_init" ->
                    let attr_name = read_id () in
                    let attr_type = read_id () in
                    let init_exp = read_exp () in
                    Attribute(attr_name, attr_type, Some init_exp)
                | "method" ->
                    let metho_name = read_id () in
                    let num_formals = int_of_string (read ()) in
                    let formals = read_formals num_formals [] in
                    let metho_type = read_id () in
                    let body_exp = read_exp () in
                    Method(metho_name, formals, metho_type, body_exp)
                | _ -> failwith "Unknown feature type"
              in
              read_features (n - 1) (feature :: acc)
          
          and read_formals n acc =
            if n = 0 then List.rev acc
            else
              let formal_name = read_id () in
              let formal_type = read_id () in
              read_formals (n - 1) ((formal_name, formal_type) :: acc)
          
          and read_exp () =
            let eloc = read () in
            let stat =
              match read () with
              | "SELF_TYPE" -> Some (SELF_TYPE "SELF_TYPE") (* Replace "SELF_TYPE" with context if needed *)
              | typ -> Some (Class typ)
            in
            let ekind =
              match read () with
              | "integer" ->
                  let ival = read () in
                  Integer ival
              | "string" ->
                  let sval = read () in
                  String sval
              | "bool" ->
                  let bval = read () in
                  Bool bval
              | "plus" ->
                  let left = read_exp () in
                  let right = read_exp () in
                  Plus (left, right)
              | "times" ->
                  let left = read_exp () in
                  let right = read_exp () in
                  Times (left, right)
              | "divide" ->
                  let left = read_exp () in
                  let right = read_exp () in
                  Divide (left, right)
              | "minus" ->
                  let left = read_exp () in
                  let right = read_exp () in
                  Minus (left, right)
              | "let" ->
                  let num_bindings = int_of_string (read ()) in
                  let bindings =
                    let rec parse_bindings n acc =
                      if n = 0 then List.rev acc
                      else
                        let binding_type = read () in
                        let binding =
                          match binding_type with
                          | "let_binding_no_init" ->
                              let let_var = (read (), read ()) in
                              let var_type = (read (), read ()) in
                              (let_var, var_type, None)
                          | "let_binding_init" ->
                              let let_var = (read (), read ()) in
                              let var_type = (read (), read ()) in
                              let init_exp = read_exp () in
                              (let_var, var_type, Some init_exp)
                          | _ -> failwith ("Invalid let binding type: " ^ binding_type)
                        in
                        parse_bindings (n - 1) (binding :: acc)
                    in
                    parse_bindings num_bindings []
                  in
                  let let_body = read_exp () in
                  Let (bindings, let_body)
              | "block" ->
                  let num_exps = int_of_string (read ()) in
                  let rec parse_block n acc =
                    if n = 0 then List.rev acc
                    else
                      let expr = read_exp () in
                      parse_block (n - 1) (expr :: acc)
                  in
                  let exprs = parse_block num_exps [] in
                  Block (exprs)
              | "case" ->
                  let test_exp = read_exp () in
                  let num_cases = int_of_string (read ()) in
                  let rec parse_cases n acc =
                    if n = 0 then List.rev acc
                    else
                      let var = (read (), read ()) in
                      let var_type = (read (), read ()) in
                      let case_body = read_exp () in
                      parse_cases (n - 1) ((var, var_type, case_body) :: acc)
                  in
                  let cases = parse_cases num_cases [] in
                  Case (test_exp, cases)
              | "identifier" ->
                  let id = (read (), read ()) in
                  Identifier id
              | "new" ->
                  let id = (read (), read ()) in
                  New id
              | "if" ->
                  let if_exp = read_exp () in
                  let then_exp = read_exp () in
                  let else_exp = read_exp () in
                  If (if_exp, then_exp, else_exp)
              | "while" ->
                  let loop_exp = read_exp () in
                  let pool_exp = read_exp () in
                  While (loop_exp, pool_exp)
              | "assign" ->
                  let var = (read (), read ()) in
                  let rhs = read_exp () in
                  Assign (var, rhs)
              | "isvoid" ->
                  let isvoid_exp = read_exp () in
                  Isvoid (isvoid_exp)
              | "dynamic_dispatch" ->
                  let expr = read_exp () in
                  let metho = (read (), read ()) in
                  let num_args = int_of_string (read ()) in
                  let rec parse_args n acc =
                    if n = 0 then List.rev acc
                    else
                      let arg = read_exp () in
                      parse_args (n - 1) (arg :: acc)
                  in
                  let args = parse_args num_args [] in
                  Dynamic_Dispatch (expr, metho, args)
              | "static_dispatch" ->
                  let expr = read_exp () in
                  let ftype = (read (), read ()) in
                  let metho = (read (), read ()) in
                  let num_args = int_of_string (read ()) in
                  let rec parse_args n acc =
                    if n = 0 then List.rev acc
                    else
                      let arg = read_exp () in
                      parse_args (n - 1) (arg :: acc)
                  in
                  let args = parse_args num_args [] in
                  Static_Dispatch (expr, ftype, metho, args)
              | "self_dispatch" ->
                  let metho = (read (), read ()) in
                  let num_args = int_of_string (read ()) in
                  let rec parse_args n acc =
                    if n = 0 then List.rev acc
                    else
                      let arg = read_exp () in
                      parse_args (n - 1) (arg :: acc)
                  in
                  let args = parse_args num_args [] in
                  Self_Dispatch (metho, args)
              | "lt" ->
                  let left = read_exp () in
                  let right = read_exp () in
                  LT (left, right)
              | "le" ->
                  let left = read_exp () in
                  let right = read_exp () in
                  LE (left, right)
              | "eq" ->
                  let left = read_exp () in
                  let right = read_exp () in
                  EQ (left, right)
              | "negate" ->
                  let negate_exp = read_exp () in
                  Negate (negate_exp)
              | "not" ->
                  let not_exp = read_exp () in
                  Not (not_exp)
              | x -> failwith ("Unhandled expression kind " ^x)
            in
            { loc = eloc; static_type = stat; exp_kind = ekind}
          
          and read_id () =
            let loc = read () in
            let name = read () in
            (loc, name)
          in
          let cmap: class_map = read_class_map in 
          let imp_map: implementation_map = read_implementation_map in
          let pmap: parent_map = read_parent_map in 
          let ast = read_cool_prog () in 
          (* let rec output_id bruh_val = 
            printf "%s\n%s\n" (fst bruh_val) (snd bruh_val) 
          in
          let rec output_ints sval = 
            match sval with 
            | Identifier(ival) ->
              printf "%s\n" (snd ival)
            | Integer(ival) -> printf "int %s\n" ival
            | _ -> printf "unmatch case\n";
          in
          let counter = ref 0 in
          let rec output_tac e =
            match e.exp_kind with
            | Identifier(ival) ->
              printf "%s\n" (snd ival);
              print_id ival;
            | Integer(ival) -> printf "int %s\n" ival
            | String(ival) -> printf "string\n%s\n" ival
            | Plus(e1,e2) ->
              let t3 = "t$" ^ string_of_int !counter in 
              let () = counter <- !counter + 1  in
              let t1 = "t$" ^ string_of_int !counter in 
              printf "%s\n"  t1;
              printf "%s <- " t1;
              output_tac e1;
              let () = counter <- !counter + 1  in
              let t2 = "t$" ^ string_of_int !counter in 
              printf "%s <- " t2;
              output_tac e2;
              printf "%s <- + %s %s\n" t3 t1 t2
            | _ -> printf ""
        in *)
        (* let metho_count = ref 0 in
        printf "comment start\n";
        List.iter (fun ((cloc,cname),inherits, feats) ->
          List.iter (fun feat ->
          match feat with 
          | Attribute((name_loc, name),(dt_loc,dt_type), (Some init_exp)) ->
            printf "%s <- " name;
            output_tac init_exp
          | Method((metho_loc,metho_name), forms, (metho_type_loc,metho_type), metho_bod) ->
            printf "label %s_%s_%d\n" cname metho_name !metho_count;
            output_tac metho_bod
          | Attribute((_, _),(_,_), None) ->
            printf "bruh";
          ) feats; 
        ) ast; *)

        let rec convert_expr (e : exp) (target : string option) : (tac_instr list * tac_expr) =
          match e.exp_kind with
          | Integer value ->
              let int_value = int_of_string value in
              (match target with
               | Some var -> [TAC_Assign_Int (var, int_value)], TAC_Variable var
               | None ->
                  let new_var = fresh_variable () in
                  [TAC_Assign_Int (new_var, int_value)], TAC_Variable new_var)
          | String value ->
              (match target with
               | Some var -> [TAC_Assign_String (var, value)], TAC_Variable var
               | None ->
                  let new_var = fresh_variable () in
                  [TAC_Assign_String (new_var, value)], TAC_Variable new_var)
          | Identifier (_, name) ->
            [], TAC_Variable name
          | Bool (value) ->
            let bool_value = bool_of_string value in
            (match target with
              | Some var -> 
                [TAC_Assign_Bool (var, bool_value)], TAC_Variable var
              | None -> 
                let new_var = fresh_variable () in 
                [TAC_Assign_Bool (new_var, bool_value)], TAC_Variable new_var)
          | Assign (var, rhs_exp) ->
            (* Step 1: Generate TAC for the right-hand side expression *)
            let rhs_instrs, rhs_result = convert_expr rhs_exp None in
        
            (* Step 2: Create an assignment instruction to update var with the result *)
            let assign_instr = 
              match rhs_result with
              | TAC_Variable v -> TAC_Assign_Var (snd var, v)
              | _ -> failwith "Unexpected TAC expression for assignment"
            in
        
            (* Combine the instructions for the right-hand side and the assignment *)
            rhs_instrs @ [assign_instr], TAC_Variable (snd var)
          | Plus (e1, e2) ->
            let instrs1, temp1 = convert_expr e1 None in
            let instrs2, temp2 = convert_expr e2 None in
            let new_var = fresh_variable () in
            let to_output = TAC_Assign_Plus (new_var, temp1, temp2) in
            instrs1 @ instrs2 @ [to_output], TAC_Variable new_var
          | Minus (e1, e2) ->
              let instrs1, temp1 = convert_expr e1 None in
              let instrs2, temp2 = convert_expr e2 None in
              let new_var = fresh_variable () in
              let to_output = TAC_Assign_Minus (new_var, temp1, temp2) in
              instrs1 @ instrs2 @ [to_output], TAC_Variable new_var
          | Times (e1, e2) ->
              let instrs1, temp1 = convert_expr e1 None in
              let instrs2, temp2 = convert_expr e2 None in
              let new_var = fresh_variable () in
              let to_output = TAC_Assign_Times (new_var, temp1, temp2) in
              instrs1 @ instrs2 @ [to_output], TAC_Variable new_var
          | Divide (e1, e2) ->
              let instrs1, temp1 = convert_expr e1 None in
              let instrs2, temp2 = convert_expr e2 None in
              let new_var = fresh_variable () in
              let to_output = TAC_Assign_Divide (new_var, temp1, temp2) in
              instrs1 @ instrs2 @ [to_output], TAC_Variable new_var
          | If (if_exp, then_exp, else_exp) ->
            (* Generate unique labels for then, else, and end of the if statement *)
            metho_count := !metho_count + 1;
            let then_label = "then_label_" ^ string_of_int !metho_count in
            let else_label = "else_label_" ^ string_of_int !metho_count in
            let end_label = "end_label_" ^ string_of_int !metho_count in
        
            (* Step 1: Process the condition (if_exp) *)
            let cond_instrs, cond_result = convert_expr if_exp None in
        
            (* Step 2: Generate the conditional jump to the else branch if condition is false *)
            let negated_var = fresh_variable () in
            let negate_instr = TAC_Cnd_Not (negated_var, cond_result) in
            let jump_to_else = TAC_Jump_If_Not (TAC_Variable negated_var, else_label) in
        
            (* Step 3: Process the 'then' branch and store its result in a new variable *)
            let result_var = fresh_variable () in
            let then_instrs, then_result = convert_expr then_exp (Some result_var) in
            let then_branch = [TAC_Label then_label] @ then_instrs @ [TAC_Assign_Var (result_var, match then_result with
                                                              | TAC_Variable v -> v
                                                              | _ -> failwith "Unexpected TAC expression")] @ [TAC_Jump end_label] in
        
            (* Step 4: Process the 'else' branch and store its result in the same variable *)
            let else_instrs, else_result = convert_expr else_exp (Some result_var) in
            let else_branch = [TAC_Label else_label] @ else_instrs @ [TAC_Assign_Var (result_var, match else_result with
                                                              | TAC_Variable v -> v
                                                              | _ -> failwith "Unexpected TAC expression")] in
        
            (* Step 5: Combine all instructions and return the result *)
            let combined_instrs = 
                cond_instrs
                @ [negate_instr]                       (* Negate the condition *)
                @ [jump_to_else]                       (* Jump to else if negated condition is true *)
                @ then_branch                          (* Then branch instructions *)
                @ else_branch                          (* Else branch instructions *)
                @ [TAC_Label end_label]                (* End label for the if statement *)
            in
            combined_instrs, TAC_Variable result_var

          | While (pool, loop) ->
            let classmethod_label = !curr_class ^ "_" ^ !curr_method ^ "_" in
            metho_count := !metho_count + 1;
            let main_pred_label = classmethod_label ^ (string_of_int !metho_count) in
            metho_count := !metho_count + 1;
            let main_body_label = classmethod_label ^ (string_of_int !metho_count) in
            metho_count := !metho_count + 1;
            let main_join_label = classmethod_label ^ (string_of_int !metho_count) in

            let cond_instrs, cond_result = convert_expr pool None in

            let negated_var = fresh_variable () in
            let negate_instr = TAC_Cnd_Not (negated_var, cond_result) in

            let jump_to_else = TAC_Jump_If_Not (TAC_Variable negated_var, main_body_label) in
            let jump_to_then = TAC_Jump_If_Not (cond_result, main_join_label) in

            (* Step 5: Process the 'else' branch *)
            let body_instrs, _ = convert_expr loop None in
            let jmp_pred_label = TAC_Jump main_pred_label in
            let body_branch = [TAC_Label main_join_label] @ body_instrs @ [jmp_pred_label] in (* This is the issue with the comps. *)


            (* make the default_obj_var  *)
            let def_obj = TAC_Default (TAC_Variable (fresh_variable()), "Object") in  


            (* Step 6: Combine all instructions with labels *)
            (* @ [TAC_Label main_pred_label] *)
            [jmp_pred_label]
            @ [TAC_Label main_pred_label]
            @ cond_instrs
            @ [negate_instr]                       (* Negate the condition after it's evaluated *)
            (* @ [TAC_Label main_body_label] *)
            @ [jump_to_else; jump_to_then]         (* Both condition jumps *)
            (* @ [TAC_Label main_join_label] *)
            @ body_branch                          (* Body branch instructions *)
            @ [TAC_Label main_body_label] @ [def_obj] ,TAC_Variable main_body_label
          | LT (e1, e2) ->
            let instrs1, temp1 = convert_expr e1 None in
            let instrs2, temp2 = convert_expr e2 None in
            let new_var = fresh_variable () in
            let to_output = TAC_Cnd_LessThan (new_var, temp1, temp2) in
            instrs1 @ instrs2 @ [to_output], TAC_Variable new_var
          | LE (e1, e2) ->
            let instrs1, temp1 = convert_expr e1 None in
            let instrs2, temp2 = convert_expr e2 None in
            let new_var = fresh_variable () in
            let to_output = TAC_Cnd_LessEqual (new_var, temp1, temp2) in
            instrs1 @ instrs2 @ [to_output], TAC_Variable new_var
          | EQ (e1, e2) ->
            let instrs1, temp1 = convert_expr e1 None in
            let instrs2, temp2 = convert_expr e2 None in
            let new_var = fresh_variable () in
            let to_output = TAC_Cnd_Equal (new_var, temp1, temp2) in
            instrs1 @ instrs2 @ [to_output], TAC_Variable new_var
          | Not (e1) -> 
            let instrs1, temp1 = convert_expr e1 None in
            let new_var = fresh_variable () in
            let to_output = TAC_Cnd_Not (new_var, temp1) in
            instrs1 @ [to_output], TAC_Variable new_var
          | Negate (e1) ->
            let instrs1, temp1 = convert_expr e1 None in
            let new_var = fresh_variable () in
            let to_output = TAC_Negate (new_var, temp1) in
            instrs1 @ [to_output], TAC_Variable new_var
          | New (e1) ->
            let instrs1, temp1 = convert_id e1 in
            let new_var = fresh_variable () in
            let to_output = TAC_New (new_var, temp1) in
            instrs1 @ [to_output], TAC_Variable new_var
          | Block exp_list ->
            let rec process_block exprs acc_instrs =
              match exprs with
              | [] -> failwith "Exit 1" (* Return empty if no expressions *)
              | [last] ->  (* Last expression in the block *)
                  let instrs, last_result = convert_expr last None in
                  acc_instrs @ instrs, last_result
              | exp :: rest ->  (* Process each expression sequentially *)
                  let instrs, _ = convert_expr exp None in
                  process_block rest (acc_instrs @ instrs)
            in
            process_block exp_list []
          | Self_Dispatch (method_id, args) ->
            (* Step 1: Generate TAC for each argument *)
            let rec process_args args acc_instrs acc_args =
              match args with
              | [] -> acc_instrs, List.rev acc_args  (* Return accumulated instructions and argument expressions *)
              | arg :: rest ->
                  let instrs, arg_result = convert_expr arg None in
                  process_args rest (acc_instrs @ instrs) (arg_result :: acc_args)
            in
            let arg_instrs, arg_exprs = process_args args [] [] in
        
            (* Step 2: Generate a new variable for the result *)
            let new_var = fresh_variable () in
        
            (* Step 3: Create the TAC for the self-dispatch call *)
            let to_output = TAC_Self_Dispatch (new_var, method_id, arg_exprs) in
            arg_instrs @ [to_output], TAC_Variable new_var
            | Dynamic_Dispatch (e, method_id, args) ->
              (* Step 1: Generate TAC for the arguments first, then the object *)
              let arg_instrs, arg_vars = 
                match args with
                | [arg] -> 
                    let arg_instrs, arg_var = convert_expr arg None in
                    arg_instrs, [arg_var]
                | _ -> failwith "Expected a single argument for init_age"
              in
          
              (* Step 2: Generate TAC for the dispatch object *)
              let obj_instrs, obj_var = convert_expr e None in
          
              (* Step 3: Generate a new temporary for the result of the dynamic dispatch *)
              let new_var = fresh_variable () in
          
              (* Step 4: Create the dynamic dispatch call with args and the object *)
              let call_instr = TAC_Self_Dispatch (new_var, method_id, arg_vars @ [obj_var]) in
          
              (* Step 5: Assign the result to a new variable, if needed *)
              let result_var = fresh_variable () in
              let assign_instr = TAC_Assign_Var (result_var, new_var) in
          
              (* Step 6: Return instructions *)
              arg_instrs @ obj_instrs @ [call_instr; assign_instr], TAC_Variable result_var
          
          | Let (bindings, let_body) ->
            (* Process each binding in sequence, accumulating TAC instructions *)
            let rec process_bindings bindings acc_instrs =
              match bindings with
              | [] -> acc_instrs  (* No more bindings; return accumulated instructions *)
              | ((let_var_loc, let_var_name), (let_type_loc, let_type_name), init_opt) :: rest ->
                  (* Generate TAC for the initializer, if it exists *)
                  let init_instrs =
                    match init_opt with
                    | Some init_exp ->
                        let expr_instrs, expr_result = convert_expr init_exp None in
                        let assign_instr = TAC_Assign_Var (let_var_name, match expr_result with
                                                              | TAC_Variable v -> v
                                                              | _ -> failwith "Unexpected TAC expression for init") in
                        expr_instrs @ [assign_instr]
                    | None ->
                        (* Default initialization if no initializer is provided *)
                        [TAC_Assign_Int (let_var_name, 0)]
                  in
                  (* Process remaining bindings with accumulated instructions *)
                  process_bindings rest (acc_instrs @ init_instrs)
            in

            (* Generate TAC for all bindings first *)
            let binding_instrs = process_bindings bindings [] in

            (* Generate TAC for the let body, using the initialized bindings *)
            let body_instrs, body_result = convert_expr let_body target in

            (* Combine binding and body instructions *)
            binding_instrs @ body_instrs, body_result
          | _ -> 
            printf "";
            [], TAC_Variable "something went wrong"
          in
        let print_tac_expr expr =
          match expr with
          | TAC_Variable var_name -> printf "%s" var_name
          | TAC_Int int_val -> printf "%d" int_val
          | TAC_String str_val -> printf "\"%s\"" str_val
          | TAC_Bool bool_val -> printf "%b" bool_val
        in
        (* Printing *)
        (* let rec print_tac_instr instr =
          match instr with
          | TAC_Assign_String (var, value) ->
            printf "%s <- string \n%s\n" var value
          | TAC_Jump_If_Not (cond_expr, label) ->
            printf "bt ";
            print_tac_expr cond_expr;
            printf " %s\n" label
          | TAC_Default (varname, sometype) ->
            print_tac_expr varname;
            printf " <- default %s\n" sometype
          | TAC_Jump label ->
              printf "jmp %s\n" label
          | TAC_Label label ->
            printf "label %s\n" label
          | TAC_Assign_Int (var, value) ->
              printf "%s <- int %d\n" var value
          | TAC_Assign_Bool (var, value) ->
                if value = true then
                  printf "%s <- bool true\n" var
                else 
                  printf "%s <- bool false\n" var
          | TAC_Assign_Var (var, src_var) ->
              printf "%s <- %s\n" var src_var
          | TAC_Assign_Plus (var, e1, e2) ->
              let e1_str = match_exp e1 in
              let e2_str = match_exp e2 in
              printf "%s <- + %s %s\n" var e1_str e2_str
          | TAC_Assign_Minus (var, e1, e2) ->
            let e1_str = match_exp e1 in
            let e2_str = match_exp e2 in
              printf "%s <- - %s %s\n" var e1_str e2_str
          | TAC_Assign_Times (var, e1, e2) ->
            let e1_str = match_exp e1 in
            let e2_str = match_exp e2 in
              printf "%s <- * %s %s\n" var e1_str e2_str
          | TAC_Assign_Divide (var, e1, e2) ->
            let e1_str = match_exp e1 in
            let e2_str = match_exp e2 in
              printf "%s <- / %s %s\n" var e1_str e2_str
          | TAC_Cnd_LessThan (var, e1, e2) ->
              let e1_val = match e1 with
                          | TAC_Variable v -> v
                          | TAC_String i -> "string" 
                          | TAC_Int i -> "int"
                          | TAC_Bool i -> "bool" ^ string_of_bool i in
              let e2_val = match e2 with
                          | TAC_Variable v -> v
                          | TAC_String i -> "string" 
                          | TAC_Int i -> "int" ^ string_of_int i
                          | TAC_Bool i -> "bool" ^ string_of_bool i in
              printf "%s <- < %s %s\n" var e1_val e2_val
          | TAC_Cnd_LessEqual (var, e1, e2) ->
              let e1_val = match e1 with
                        | TAC_Variable v -> v
                        | TAC_String i -> "string" 
                        | TAC_Int i -> "int" ^ string_of_int i
                        | TAC_Bool i -> "bool" ^ string_of_bool i in
              let e2_val = match e2 with
                        | TAC_Variable v -> v
                        | TAC_String i -> "string" 
                        | TAC_Int i -> "int" ^ string_of_int i
                        | TAC_Bool i -> "bool" ^ string_of_bool i in
              printf "%s <- <= %s  %s\n" var e1_val e2_val
          | TAC_Cnd_Equal (var, e1, e2) ->
            let e1_val = match e1 with
                      | TAC_Variable v -> v
                      | TAC_String i -> "string" 
                      | TAC_Int i -> "int" ^ string_of_int i
                      | TAC_Bool i -> "bool" ^ string_of_bool i in
            let e2_val = match e2 with
                      | TAC_Variable v -> v
                      | TAC_String i -> "string" 
                      | TAC_Int i -> "int" ^ string_of_int i
                      | TAC_Bool i -> "bool" ^ string_of_bool i in
            printf "%s <- = %s %s\n" var e1_val e2_val
          | TAC_Cnd_Not (var, e1) ->
            (* Just for Booleans *)
            let e1_val = match e1 with
                      | TAC_Variable v -> v
                      | TAC_String i -> "string" 
                      | TAC_Int i -> "int" ^ string_of_int i
                      | TAC_Bool i -> "bool" ^ string_of_bool i in
            printf "%s <- not %s\n" var e1_val
          | TAC_Negate (var, e1) ->
            (* Only for ints *)
            let e1_val = match e1 with
                      | TAC_Variable v -> v
                      | TAC_String i -> "string"
                      | TAC_Int i -> "int" ^ string_of_int i
                      | TAC_Bool i -> "bool" ^ string_of_bool i in
            printf "%s <- ~ %s\n" var e1_val
          | TAC_New (var, e1) ->
              let e1_val = match e1 with
                      | TAC_Variable v -> v
                      | TAC_String i -> "string"
                      | TAC_Int i -> "int" ^ string_of_int i 
                      | TAC_Bool i -> "bool" ^ string_of_bool i in
                printf "%s <- new %s\n" var e1_val
          | TAC_isvoid (var, e1) ->
              let e1_val = match e1 with
                      | TAC_Variable v -> v
                      | TAC_String i -> "string"
                      | TAC_Int i -> "int"
                      | TAC_Bool i -> "bool" in
                printf "%s <- isvoid %s\n" var e1_val
          | TAC_call_out (var, e1, e2) ->
                printf "%s <- call %s %s\n" var e1 e2
          | TAC_call_in (var, e1) ->
                printf "%s <- call %s\n" var e1
          | TAC_Self_Dispatch (result_var, (loc, method_name), args) ->
            printf "%s <- call %s " result_var method_name;
            List.iteri (fun i arg ->
              if i > 0 then printf " ";
              print_tac_expr arg
            ) args;
            printf "\n"
          | TAC_Return result_var ->
            printf "return %s\n" result_var
          | TAC_Let (bingings, let_body) ->
            printf ""
        in *)
        (* TODO: Print asm instead of tac code *)
        let rec tac_to_asm instr =
        match instr with
        | TAC_Assign_String (var, value) ->
          printf "%s <- string \n%s\n" var value
        | TAC_Jump_If_Not (cond_expr, label) ->
          printf "bt ";
          print_tac_expr cond_expr;
          printf " %s\n" label
        | TAC_Default (varname, sometype) ->
          print_tac_expr varname;
          printf " <- default %s\n" sometype
        | TAC_Jump label ->
            printf "jmp %s\n" label
        | TAC_Label label ->
          printf "label %s\n" label
        | TAC_Assign_Int (var, value) ->
            printf "%s <- int %d\n" var value
        | TAC_Assign_Bool (var, value) ->
              if value = true then
                printf "%s <- bool true\n" var
              else 
                printf "%s <- bool false\n" var
        | TAC_Assign_Var (var, src_var) ->
            printf "%s <- %s\n" var src_var
        | TAC_Assign_Plus (var, e1, e2) ->
            let e1_str = match_exp e1 in
            let e2_str = match_exp e2 in
            printf "%s <- + %s %s\n" var e1_str e2_str
        | TAC_Assign_Minus (var, e1, e2) ->
          let e1_str = match_exp e1 in
          let e2_str = match_exp e2 in
            printf "%s <- - %s %s\n" var e1_str e2_str
        | TAC_Assign_Times (var, e1, e2) ->
          let e1_str = match_exp e1 in
          let e2_str = match_exp e2 in
            printf "%s <- * %s %s\n" var e1_str e2_str
        | TAC_Assign_Divide (var, e1, e2) ->
          let e1_str = match_exp e1 in
          let e2_str = match_exp e2 in
            printf "%s <- / %s %s\n" var e1_str e2_str
        | TAC_Cnd_LessThan (var, e1, e2) ->
            let e1_val = match e1 with
                        | TAC_Variable v -> v
                        | TAC_String i -> "string" 
                        | TAC_Int i -> "int"
                        | TAC_Bool i -> "bool" ^ string_of_bool i in
            let e2_val = match e2 with
                        | TAC_Variable v -> v
                        | TAC_String i -> "string" 
                        | TAC_Int i -> "int" ^ string_of_int i
                        | TAC_Bool i -> "bool" ^ string_of_bool i in
            printf "%s <- < %s %s\n" var e1_val e2_val
        | TAC_Cnd_LessEqual (var, e1, e2) ->
            let e1_val = match e1 with
                      | TAC_Variable v -> v
                      | TAC_String i -> "string" 
                      | TAC_Int i -> "int" ^ string_of_int i
                      | TAC_Bool i -> "bool" ^ string_of_bool i in
            let e2_val = match e2 with
                      | TAC_Variable v -> v
                      | TAC_String i -> "string" 
                      | TAC_Int i -> "int" ^ string_of_int i
                      | TAC_Bool i -> "bool" ^ string_of_bool i in
            printf "%s <- <= %s  %s\n" var e1_val e2_val
        | TAC_Cnd_Equal (var, e1, e2) ->
          let e1_val = match e1 with
                    | TAC_Variable v -> v
                    | TAC_String i -> "string" 
                    | TAC_Int i -> "int" ^ string_of_int i
                    | TAC_Bool i -> "bool" ^ string_of_bool i in
          let e2_val = match e2 with
                    | TAC_Variable v -> v
                    | TAC_String i -> "string" 
                    | TAC_Int i -> "int" ^ string_of_int i
                    | TAC_Bool i -> "bool" ^ string_of_bool i in
          printf "%s <- = %s %s\n" var e1_val e2_val
        | TAC_Cnd_Not (var, e1) ->
          (* Just for Booleans *)
          let e1_val = match e1 with
                    | TAC_Variable v -> v
                    | TAC_String i -> "string" 
                    | TAC_Int i -> "int" ^ string_of_int i
                    | TAC_Bool i -> "bool" ^ string_of_bool i in
          printf "%s <- not %s\n" var e1_val
        | TAC_Negate (var, e1) ->
          (* Only for ints *)
          let e1_val = match e1 with
                    | TAC_Variable v -> v
                    | TAC_String i -> "string"
                    | TAC_Int i -> "int" ^ string_of_int i
                    | TAC_Bool i -> "bool" ^ string_of_bool i in
          printf "%s <- ~ %s\n" var e1_val
        | TAC_New (var, e1) ->
            let e1_val = match e1 with
                    | TAC_Variable v -> v
                    | TAC_String i -> "string"
                    | TAC_Int i -> "int" ^ string_of_int i 
                    | TAC_Bool i -> "bool" ^ string_of_bool i in
              printf "%s <- new %s\n" var e1_val
        | TAC_isvoid (var, e1) ->
            let e1_val = match e1 with
                    | TAC_Variable v -> v
                    | TAC_String i -> "string"
                    | TAC_Int i -> "int"
                    | TAC_Bool i -> "bool" in
              printf "%s <- isvoid %s\n" var e1_val
        | TAC_call_out (var, e1, e2) ->
              printf "%s <- call %s %s\n" var e1 e2;
        | TAC_call_in (var, e1) ->
              printf "%s <- call %s\n" var e1;
        | TAC_Self_Dispatch (result_var, (method_name, loc), args) ->
          printf "%s <- call %s " result_var method_name;
          List.iteri (fun i arg ->
            if i > 0 then printf " ";
            print_tac_expr arg
          ) args;
          printf "\n"
        | TAC_Return result_var ->
          printf "return %s\n" result_var
        | TAC_Let (bingings, let_body) ->
          printf ""
      in
        (* Function to output the full list of TAC instructions for a method body *)
        let output_tac target e =
          let tac_instrs, _ = convert_expr e target in
          List.iter (tac_to_asm) tac_instrs;
          safe_head tac_instrs
        in
        (* Main program to iterate over the classes and features *)
        printf "comment start\n";
        List.iter (fun ((cloc, cname), inherits, feats) ->
          curr_class := cname;
          List.iter (fun feat ->
            match feat with
            | Attribute ((name_loc, name), (dt_loc, dt_type), Some init_exp) ->
                let last = output_tac (Some name) init_exp in 
                printf ""
            | Method ((metho_loc, metho_name), forms, (metho_type_loc, metho_type), metho_bod) ->
                curr_method := metho_name ;
                printf "label %s_%s_%d\n" !curr_class !curr_method !metho_count;
                let last = output_tac None metho_bod in 
                (match last with
                | Some (TAC_Assign_Int (var, _)
                      | TAC_Assign_Bool (var, _) 
                      | TAC_Assign_Var (var, _)
                      | TAC_Assign_Plus (var, _, _)
                      | TAC_Assign_Minus (var, _, _)
                      | TAC_Assign_Times (var, _, _)
                      | TAC_Assign_Divide (var, _, _)
                      | TAC_Cnd_LessThan (var, _, _)
                      | TAC_Cnd_LessEqual (var, _, _)
                      | TAC_Cnd_Equal (var, _, _)
                      | TAC_Cnd_Not (var, _)
                      | TAC_Negate (var, _)
                      | TAC_New (var, _)
                      | TAC_isvoid (var, _)
                      | TAC_call_out (var, _, _)
                      | TAC_call_in (var, _)
                      | TAC_Self_Dispatch (var,_,_)) ->
                    printf "return %s\n" var
                 | _ -> printf "")
            | Attribute ((_, _), (_, _), None) ->
                printf ""
          ) feats;
        ) ast;
        let sorted_keys ht =
          let keys = Hashtbl.fold (fun key _ acc -> key :: acc) ht [] in
          List.sort String.compare keys
        in
        let print_int_new () =
          printf "\t\t\t\t\t\tpushq %%rbp\n";
          printf "\t\t\t\t\t\tmovq %%rsp, %%rbp\n";
          printf "\t\t\t\t\t\t## stack room for temporaries: 1\n";
          printf "\t\t\t\t\t\tmovq $8, %%r14\n";
          printf "\t\t\t\t\t\tsubq %%r14, %%rsp\n";
          printf "\t\t\t\t\t\t## return address handling\n";
          printf "\t\t\t\t\t\tmovq $4, %%r12\n";
          printf "\t\t\t\t\t\tmovq $8, %%rsi\n";
          printf "\t\t\t\t\t\tmovq %%r12, %%rdi\n";
          printf "\t\t\t\t\t\tcall calloc\n";
          printf "\t\t\t\t\t\tmovq %%rax, %%r12\n";
          printf "\t\t\t\t\t\t## store class tag, object size and vtable pointer\n";
          printf "\t\t\t\t\t\tmovq $1, %%r14\n";
          printf "\t\t\t\t\t\tmovq %%r14, 0(%%r12)\n";
          printf "\t\t\t\t\t\tmovq $4, %%r14\n";
          printf "\t\t\t\t\t\tmovq %%r14, 8(%%r12)\n";
          printf "\t\t\t\t\t\tmovq $Int..vtable, %%r14\n";
          printf "\t\t\t\t\t\tmovq %%r14, 16(%%r12)\n";
          printf "\t\t\t\t\t\t## initialize attributes\n";
          printf "\t\t\t\t\t\t## self[3] holds field (raw content) (Int)\n";
          printf "\t\t\t\t\t\tmovq $0, %%r13\n";
          printf "\t\t\t\t\t\tmovq %%r13, 24(%%r12)\n";
          printf "\t\t\t\t\t\t## self[3] (raw content) initializer -- none \n";
          printf "\t\t\t\t\t\tmovq %%r12, %%r13\n";
          printf "\t\t\t\t\t\t## return address handling\n";
          printf "\t\t\t\t\t\tmovq %%rbp, %%rsp\n";
          printf "\t\t\t\t\t\tpopq %%rbp\n";
          printf "\t\t\t\t\t\tret\n";
        in
        let print_object_new () =
          printf "\t\t\t\t\t\t## constructor for Object\n";
          printf "\t\t\t\t\t\tpushq %%rbp\n";
          printf "\t\t\t\t\t\tmovq %%rsp, %%rbp\n";
          printf "\t\t\t\t\t\t## stack room for temporaries: 1\n";
          printf "\t\t\t\t\t\tmovq $8, %%r14\n";
          printf "\t\t\t\t\t\tsubq %%r14, %%rsp\n";
          printf "\t\t\t\t\t\t## return address handling\n";
          printf "\t\t\t\t\t\tmovq $3, %%r12\n";
          printf "\t\t\t\t\t\tmovq $8, %%rsi\n";
          printf "\t\t\t\t\t\tmovq %%r12, %%rdi\n";
          printf "\t\t\t\t\t\tcall calloc\n";
          printf "\t\t\t\t\t\tmovq %%rax, %%r12\n";
          printf "\t\t\t\t\t\t## store class tag, object size and vtable pointer\n";
          printf "\t\t\t\t\t\tmovq $12, %%r14\n";
          printf "\t\t\t\t\t\tmovq %%r14, 0(%%r12)\n";
          printf "\t\t\t\t\t\tmovq $3, %%r14\n";
          printf "\t\t\t\t\t\tmovq %%r14, 8(%%r12)\n";
          printf "\t\t\t\t\t\tmovq $Object..vtable, %%r14\n";
          printf "\t\t\t\t\t\tmovq %%r14, 16(%%r12)\n";
          printf "\t\t\t\t\t\tmovq %%r12, %%r13\n";
          printf "\t\t\t\t\t\t## return address handling\n";
          printf "\t\t\t\t\t\tmovq %%rbp, %%rsp\n";
          printf "\t\t\t\t\t\tpopq %%rbp\n";
          printf "\t\t\t\t\t\tret\n";
        in 
        let print_io_new () =
          printf "\t\t\t\t\t\tpushq %%rbp\n";
          printf "\t\t\t\t\t\tmovq %%rsp, %%rbp\n";
          printf "\t\t\t\t\t\t## stack room for temporaries: 1\n";
          printf "\t\t\t\t\t\tmovq $8, %%r14\n";
          printf "\t\t\t\t\t\tsubq %%r14, %%rsp\n";
          printf "\t\t\t\t\t\t## return address handling\n";
          printf "\t\t\t\t\t\tmovq $3, %%r12\n";
          printf "\t\t\t\t\t\tmovq $8, %%rsi\n";
          printf "\t\t\t\t\t\tmovq %%r12, %%rdi\n";
          printf "\t\t\t\t\t\tcall calloc\n";
          printf "\t\t\t\t\t\tmovq %%rax, %%r12\n";
          printf "\t\t\t\t\t\t## store class tag, object size and vtable pointer\n";
          printf "\t\t\t\t\t\tmovq $10, %%r14\n";
          printf "\t\t\t\t\t\tmovq %%r14, 0(%%r12)\n";
          printf "\t\t\t\t\t\tmovq $3, %%r14\n";
          printf "\t\t\t\t\t\tmovq %%r14, 8(%%r12)\n";
          printf "\t\t\t\t\t\tmovq $IO..vtable, %%r14\n";
          printf "\t\t\t\t\t\tmovq %%r14, 16(%%r12)\n";
          printf "\t\t\t\t\t\tmovq %%r12, %%r13\n";
          printf "\t\t\t\t\t\t## return address handling\n";
          printf "\t\t\t\t\t\tmovq %%rbp, %%rsp\n";
          printf "\t\t\t\t\t\tpopq %%rbp\n";
          printf "\t\t\t\t\t\tret\n";
        in
        let print_bool_new () =
          printf "\t\t\t\t\t\tpushq %%rbp\n";
          printf "\t\t\t\t\t\tmovq %%rsp, %%rbp\n";
          printf "\t\t\t\t\t\t## stack room for temporaries: 1\n";
          printf "\t\t\t\t\t\tmovq $8, %%r14\n";
          printf "\t\t\t\t\t\tsubq %%r14, %%rsp\n";
          printf "\t\t\t\t\t\t## return address handling\n";
          printf "\t\t\t\t\t\tmovq $4, %%r12\n";
          printf "\t\t\t\t\t\tmovq $8, %%rsi\n";
          printf "\t\t\t\t\t\tmovq %%r12, %%rdi\n";
          printf "\t\t\t\t\t\tcall calloc\n";
          printf "\t\t\t\t\t\tmovq %%rax, %%r12\n";
          printf "\t\t\t\t\t\t## store class tag, object size and vtable pointer\n";
          printf "\t\t\t\t\t\tmovq $0, %%r14\n";
          printf "\t\t\t\t\t\tmovq %%r14, 0(%%r12)\n";
          printf "\t\t\t\t\t\tmovq $4, %%r14\n";
          printf "\t\t\t\t\t\tmovq %%r14, 8(%%r12)\n";
          printf "\t\t\t\t\t\tmovq $Bool..vtable, %%r14\n";
          printf "\t\t\t\t\t\tmovq %%r14, 16(%%r12)\n";
          printf "\t\t\t\t\t\t## initialize attributes\n";
          printf "\t\t\t\t\t\t## self[3] holds field (raw content) (Int)\n";
          printf "\t\t\t\t\t\tmovq $0, %%r13\n";
          printf "\t\t\t\t\t\tmovq %%r13, 24(%%r12)\n";
          printf "\t\t\t\t\t\t## self[3] (raw content) initializer -- none \n";
          printf "\t\t\t\t\t\tmovq %%r12, %%r13\n";
          printf "\t\t\t\t\t\t## return address handling\n";
          printf "\t\t\t\t\t\tmovq %%rbp, %%rsp\n";
          printf "\t\t\t\t\t\tpopq %%rbp\n";
          printf "\t\t\t\t\t\tret\n";
        in
        let print_string_new () =
          printf "\t\t\t\t\t\tpushq %%rbp\n";
          printf "\t\t\t\t\t\tmovq %%rsp, %%rbp\n";
          printf "\t\t\t\t\t\t## stack room for temporaries: 1\n";
          printf "\t\t\t\t\t\tmovq $8, %%r14\n";
          printf "\t\t\t\t\t\tsubq %%r14, %%rsp\n";
          printf "\t\t\t\t\t\t## return address handling\n";
          printf "\t\t\t\t\t\tmovq $4, %%r12\n";
          printf "\t\t\t\t\t\tmovq $8, %%rsi\n";
          printf "\t\t\t\t\t\tmovq %%r12, %%rdi\n";
          printf "\t\t\t\t\t\tcall calloc\n";
          printf "\t\t\t\t\t\tmovq %%rax, %%r12\n";
          printf "\t\t\t\t\t\t## store class tag, object size and vtable pointer\n";
          printf "\t\t\t\t\t\tmovq $3, %%r14\n";
          printf "\t\t\t\t\t\tmovq %%r14, 0(%%r12)\n";
          printf "\t\t\t\t\t\tmovq $4, %%r14\n";
          printf "\t\t\t\t\t\tmovq %%r14, 8(%%r12)\n";
          printf "\t\t\t\t\t\tmovq $String..vtable, %%r14\n";
          printf "\t\t\t\t\t\tmovq %%r14, 16(%%r12)\n";
          printf "\t\t\t\t\t\t## initialize attributes\n";
          printf "\t\t\t\t\t\t## self[3] holds field (raw content) (String)\n";
          printf "\t\t\t\t\t\tmovq $the.empty.string, %%r13\n";
          printf "\t\t\t\t\t\tmovq %%r13, 24(%%r12)\n";
          printf "\t\t\t\t\t\t## self[3] (raw content) initializer -- none \n";
          printf "\t\t\t\t\t\tmovq %%r12, %%r13\n";
          printf "\t\t\t\t\t\t## return address handling\n";
          printf "\t\t\t\t\t\tmovq %%rbp, %%rsp\n";
          printf "\t\t\t\t\t\tpopq %%rbp\n";
          printf "\t\t\t\t\t\tret\n";
        in
        (* For now we will print the raw assemble for thing that never change*)
        let new_base_class_build class_name = 
          match class_name with
          | "Int" ->
            printf "\t\t\t\t\t\t## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n";
            printf ".globl Int..new\n";
            printf "Int..new:\t\t\t\t##constructor for Int\n";
            print_int_new ();
          | "Object" ->
            printf "\t\t\t\t\t\t## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n";
            printf ".globl Object..new\n";
            printf "Object..new:\t\t\t\t##constructor for Object\n";
            print_object_new ();
          | "IO" ->
            printf "\t\t\t\t\t\t## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n";
            printf ".globl IO..new\n";
            printf "IO..new:\t\t\t\t##constructor for IO\n";
            print_io_new ();
          | "Bool" ->
            printf "\t\t\t\t\t\t## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n";
            printf ".globl Bool..new\n";
            printf "Bool..new:\t\t\t\t##constructor for Bool\n";
            print_bool_new ();
          | "String" -> 
            printf "\t\t\t\t\t\t## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n";
            printf ".globl String..new\n";
            printf "String..new:\t\t\t\t##constructor for String\n";
            print_string_new ();
          | x -> 
            printf "\t\t\t\t\t\t## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n";
            printf ".globl %s..new\n" x;
            printf "%s..new:\t\t\t\t##constructor for %s\n" x x;
            printf " implement this %s later on\n" x; 
        in
        (*TODO for next PA3full, change the hard set string to a referenced 
        mutable type and increment it*)
        let build_vtable class_name metho_definition = 
          printf "\t\t\t\t\t## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n";
          printf ".globl %s..vtable\n" class_name;
          printf "%s..vtable:\t\t ## virtual function table for %s\n" class_name class_name;
          (match class_name with 
          | "Bool" ->
            printf "\t\t\t\t\t.quad string1\n";
          | "IO" ->
            printf "\t\t\t\t\t.quad string2\n";
          | "Int" ->
            printf "\t\t\t\t\t.quad string3\n";
          | "Main" ->
            printf "\t\t\t\t\t.quad string4\n";
          | "Object" -> 
            printf "\t\t\t\t\t.quad string5\n";
          | "String" -> 
            printf "\t\t\t\t\t.quad string6\n";
          | x -> printf "Boss something happened with %s\n" x;);
          printf "\t\t\t\t\t.quad %s..new\n" class_name;
          List.iter (fun (method_name,formals, return_type, body_exp) ->
            match body_exp.exp_kind with 
            | Internal (_, class_name, method_name) -> 
              printf "\t\t\t\t\t.quad %s.%s\n" class_name method_name;
            | _ ->
              printf "\t\t\t\t\t.quad %s.%s\n" class_name method_name;
          ) metho_definition;
        in
        let grab_first_value_string class_map key =
          match (Hashtbl.find_opt class_map) key with
          | Some (Some ((str1, _, _) :: _)) -> str1
          | Some (Some []) -> failwith "Value list is empty"
          | Some None -> failwith "Key exists but value is None"
          | None -> failwith "Key not found in class_map"
        in
        let hashed_keys = sorted_keys imp_map in 
        List.iter ( fun class_name ->
          build_vtable class_name (Hashtbl.find imp_map class_name))
        hashed_keys;
        List.iter ( fun class_name ->
          new_base_class_build class_name)
        hashed_keys;
        printf "HERE IS IT %s \n"  (grab_first_value_string cmap "Object");

        end ;;
main();;
(* Create a hashtbl for class_tag per class name *)
let global_start_comment = printf "\t\t\t\t\t## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n" in
let global_setup classfunc = printf ".globl %s\n" classfunc in
let constructor_setup classfunc classname = printf "%s\t\t## constructor for %s\n"  classfunc classname in
let custom_setup classfunc msg = printf "%s:\t\t%s\n" classfunc msg in
let ret_addr_handling = printf "## return address handling\n" in
let store_c_tag = printf "## store class tag, object size and vtable pointer\n" in
let stack_temps number = printf "## stack room for temporaries: %s\n" number in
let init_attrs = printf "## initialize attributes\n" in
let self_holds pos varname vartype = printf "## self[%s] holds field %s (%s)\n" pos varname vartype in
let push_stack reg = printf "push %s\n" reg in
let pop_stack reg = printf "pop %s\n" reg in
let mov_op src dest = printf "movq %s, %s\n" src dest in
let add_op src dest = printf "addq %s, %s\n" src dest in
let sub_op src dest = printf "subq %s, %s\n" src dest in
let multi_op src dest = printf "imul %s, %s\n" src dest in
let div_op src dest = printf "testing" in
let and_op src dest = printf "and %s, %s\n" src dest in
let or_op src dest = printf "or %s, %s\n" src dest in
let cmp_op src dest = printf "cmpq %s, %s\n" src dest in 
let test_op src dest = printf "test %s, %s\n" src dest in 
let jmp_op label = printf "jmp %s\n" label in
let je_op label = printf "je %s\n" label in
let jne_op label = printf "jne %s\n" label in
let jns_op label = printf "jns %s\n" label in
let jl_op label = printf "jl %s\n" label in
let jle_op label = printf "jle %s\n" label in
let call_op op = printf "call %s\n" op in
let return_op = printf "ret\n" in
let new_class_instance class_name stack_loc = printf "## new %s\n" class_name;
        if (class_name == "Int" || class_name == "Bool" || class_name = "String") then 
          (
        push_stack "%rbp";
        push_stack "%r12";
        mov_op ("$"^class_name^"..new") "%r14";
        call_op "*%r14";
        pop_stack "%r12";
        pop_stack "%rbp";
        mov_op "%r13" (stack_loc ^ "(%r12)");
          )
      else
          mov_op "$0" (stack_loc^"(%r12)");
      in
  let self_non_std stack_loc = mov_op "$0" (stack_loc ^ "(%r12)") in 
  let self_init pos varname varinfo = 
    if varinfo == "none" then
        printf "## self[%s] %s initializer -- none\n" pos varname
    else 
      printf "## self[%s] %s initializer <- %s\n" pos varname varinfo
    in
  let custom_comment msg = printf "## %s\n" msg in
  let actual_tbl classfunc = printf "%s:\t\t\t\t" classfunc in
  
(* generating vtables
    read imp_map and extract all functions of class
    place string of map and class's new 
 *)

let initial_vtable_before_main = "                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl Bool..vtable
Bool..vtable:           ## virtual function table for Bool
                        .quad string2
                        .quad Bool..new
                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl IO..vtable
IO..vtable:             ## virtual function table for IO
                        .quad string3
                        .quad IO..new
                        .quad IO.in_int
                        .quad IO.out_int
                        .quad IO.out_string
                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl Int..vtable
Int..vtable:            ## virtual function table for Int
                        .quad string4
                        .quad Int..new
                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl Main..vtable
Main..vtable:           ## virtual function table for Main
                        .quad string5
                        .quad Main..new
                        .quad IO.in_int
                        .quad IO.out_int
                        .quad Main.main
                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl Bool..new
Bool..new:              ## constructor for Bool
                        pushq %rbp
                        movq %rsp, %rbp
                        ## stack room for temporaries: 1
                        movq $8, %r14
                        subq %r14, %rsp
                        ## return address handling
                        movq $4, %r12
                        movq $8, %rsi
			movq %r12, %rdi
			call calloc
			movq %rax, %r12
                        ## store class tag, object size and vtable pointer
                        movq $0, %r14
                        movq %r14, 0(%r12)
                        movq $4, %r14
                        movq %r14, 8(%r12)
                        movq $Bool..vtable, %r14
                        movq %r14, 16(%r12)
                        ## initialize attributes
                        ## self[3] holds field (raw content) (Int)
                        movq $0, %r13
                        movq %r13, 24(%r12)
                        ## self[3] (raw content) initializer -- none 
                        movq %r12, %r13
                        ## return address handling
                        movq %rbp, %rsp
                        popq %rbp
                        ret
                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl IO..new
IO..new:                ## constructor for IO
                        pushq %rbp
                        movq %rsp, %rbp
                        ## stack room for temporaries: 1
                        movq $8, %r14
                        subq %r14, %rsp
                        ## return address handling
                        movq $3, %r12
                        movq $8, %rsi
			movq %r12, %rdi
			call calloc
			movq %rax, %r12
                        ## store class tag, object size and vtable pointer
                        movq $11, %r14
                        movq %r14, 0(%r12)
                        movq $3, %r14
                        movq %r14, 8(%r12)
                        movq $IO..vtable, %r14
                        movq %r14, 16(%r12)
                        movq %r12, %r13
                        ## return address handling
                        movq %rbp, %rsp
                        popq %rbp
                        ret
                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl Int..new
Int..new:               ## constructor for Int
                        pushq %rbp
                        movq %rsp, %rbp
                        ## stack room for temporaries: 1
                        movq $8, %r14
                        subq %r14, %rsp
                        ## return address handling
                        movq $4, %r12
                        movq $8, %rsi
			movq %r12, %rdi
			call calloc
			movq %rax, %r12
                        ## store class tag, object size and vtable pointer
                        movq $1, %r14
                        movq %r14, 0(%r12)
                        movq $4, %r14
                        movq %r14, 8(%r12)
                        movq $Int..vtable, %r14
                        movq %r14, 16(%r12)
                        ## initialize attributes
                        ## self[3] holds field (raw content) (Int)
                        movq $0, %r13
                        movq %r13, 24(%r12)
                        ## self[3] (raw content) initializer -- none 
                        movq %r12, %r13
                        ## return address handling
                        movq %rbp, %rsp
                        popq %rbp
                        ret" in
(* generating ..new classes
    read class_map and extract all attributes for classes and push new types into them
    read class_map and inits on everything w/ $0s (not prims (string, bool, int))
*)
      let initial_main_new =
        (* PA3-full take this code, use parameter cname and replace every instance to be modular *)
        let myclass = "Main" in
        let number_of_feats = 3 in (* Dynamically set *)
        global_start_comment;
        global_setup (myclass^"..new");
        custom_setup (myclass^"..new") ("constructor for "^myclass);
        push_stack "%rbp";
        stack_temps "1";
        sub_op "$8" "%rsp";
        ret_addr_handling;
        mov_op (string_of_int (3 + number_of_feats)) "%rsi" ;
        mov_op "$8" "%rdi";
        call_op "calloc";
        mov_op "%rax" "%r12";
        store_c_tag;
        let c_tag = 10 in (* Dynamically set || Have some sort of hashtbl that holds these tags *)
        mov_op ("$" ^ string_of_int c_tag) "%r12"; 
        mov_op (string_of_int number_of_feats) "8(%r12)";
        let vtbl = (myclass^"..vtable") in (* Dynamically set *)
        mov_op ("$" ^ vtbl) "16(%r12)";
        init_attrs;
        let selfcount = 3 in
        custom_comment ("self["^(string_of_int selfcount) ^ "] holds field variable (type)");
        (* 
            Read through each feature in class in class_map
            create the self comment with the info then either make an init or assign $0
        *)
        (* Once every feature for class is read
          Read through each feature in class in class_map
          Create comments if init'r is none or new Type
              if none then keep going
              else it is new type then create a new type
        *)
        ret_addr_handling;
        mov_op "%rbp" "%rsp";
        pop_stack "%rbp";
        return_op;

    in
  let io_table = "                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl IO.in_int
IO.in_int:              ## method definition
                        pushq %rbp
                        movq %rsp, %rbp
                        movq 16(%rbp), %r12
                        ## stack room for temporaries: 1
                        movq $8, %r14
                        subq %r14, %rsp
                        ## return address handling
                        ## method body begins
                        ## new Int
                        pushq %rbp
                        pushq %r12
                        movq $Int..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq %r13, %r14
                        movl	$1, %esi
			movl $4096, %edi
			call calloc
			pushq %rax
			movq %rax, %rdi
			movq $4096, %rsi 
			movq stdin(%rip), %rdx
			call fgets 
			popq %rdi 
			movl $0, %eax
			pushq %rax
			movq %rsp, %rdx
			movq $percent.ld, %rsi
			call sscanf
			popq %rax
			movq $0, %rsi 
			cmpq $2147483647, %rax 
			cmovg %rsi, %rax
			cmpq $-2147483648, %rax 
			cmovl %rsi, %rax
			movq %rax, %r13
                        movq %r13, 24(%r14)
                        movq %r14, %r13
.globl IO.in_int.end
IO.in_int.end:          ## method body ends
                        ## return address handling
                        movq %rbp, %rsp
                        popq %rbp
                        ret
                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl IO.out_int
IO.out_int:             ## method definition
                        pushq %rbp
                        movq %rsp, %rbp
                        movq 16(%rbp), %r12
                        ## stack room for temporaries: 1
                        movq $8, %r14
                        subq %r14, %rsp
                        ## return address handling
                        ## fp[3] holds argument x (Int)
                        ## method body begins
                        movq 24(%rbp), %r14
                        movq 24(%r14), %r13
                        movq $percent.d, %rdi
		movl %r13d, %eax
		cdqe
		movq %rax, %rsi
			movl $0, %eax
			call printf
                        movq %r12, %r13
.globl IO.out_int.end
IO.out_int.end:         ## method body ends
                        ## return address handling
                        movq %rbp, %rsp
                        popq %rbp
                        ret" in
(* generating class functions
    setup the foundational asm cmds
    read through class_map and print out the self comments
    read through AST as normally, rewrite tac_to_asm to accommadate the correct asm instead of TAC
*)
let initial_Main_main = (* Check the global table work *)
let classname = "Main" in
let classfunc = "main" in
global_setup (classname^"."^classfunc);
actual_tbl (classname^"."^classfunc);
custom_comment "method definition";
push_stack "%rbp";
mov_op "16(%rsp)" "%r12"; (* might need re-work *)
stack_temps "x"; (* dynamic setup *)
sub_op "$16" "%rsp";
ret_addr_handling;
(* Read through the class_map
    print out the self comments
*)
custom_comment "method body begins";
(* Read the AST and continue accordingly *)
(* Do body work *)
(* push new int, value and move to the stack *)
(* when doing conditional or methods go to a l3 *)
in

let initial_Main_main_end = 
  let classname = "Main" in
  let classfunc = "main" in
  global_setup (classname^"."^classfunc^".end");
  custom_setup (classname^"."^classfunc^".end") "## method body ends";
  ret_addr_handling;
  mov_op "%rbp" "%rsp";
  pop_stack "%rbp";
  return_op;
in

let initial_vtable_after_main = "                        ## global string constants
.globl string2
string2:                # 'Bool'
.byte  66 # 'B'
.byte 111 # 'o'
.byte 111 # 'o'
.byte 108 # 'l'
.byte 0

.globl string3
string3:                # 'IO'
.byte  73 # 'I'
.byte  79 # 'O'
.byte 0

.globl string4
string4:                # 'Int'
.byte  73 # 'I'
.byte 110 # 'n'
.byte 116 # 't'
.byte 0

.globl string5
string5:                # 'Main'
.byte  77 # 'M'
.byte  97 # 'a'
.byte 105 # 'i'
.byte 110 # 'n'
.byte 0

                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl eq_handler
eq_handler:             ## helper function for =
                        pushq %rbp
                        movq %rsp, %rbp
                        movq 32(%rbp), %r12
                        ## return address handling
                        movq 32(%rbp), %r13
                        movq 24(%rbp), %r14
                        cmpq %r14, %r13
			je eq_true
                        movq $0, %r15
                        cmpq %r15, %r13
			je eq_false
                        cmpq %r15, %r14
			je eq_false
                        movq 0(%r13), %r13
                        movq 0(%r14), %r14
                        ## place the sum of the type tags in r1
                        addq %r14, %r13
                        movq $0, %r14
                        cmpq %r14, %r13
			je eq_bool
                        movq $2, %r14
                        cmpq %r14, %r13
			je eq_int
                        movq $6, %r14
                        cmpq %r14, %r13
			je eq_true
.globl eq_false
eq_false:               ## not equal
                        ## new Bool
                        pushq %rbp
                        pushq %r12
                        movq $Bool..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        jmp eq_end
.globl eq_true
eq_true:                ## equal
                        ## new Bool
                        pushq %rbp
                        pushq %r12
                        movq $Bool..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq $1, %r14
                        movq %r14, 24(%r13)
                        jmp eq_end
.globl eq_bool
eq_bool:                ## two Bools
.globl eq_int
eq_int:                 ## two Ints
                        movq 32(%rbp), %r13
                        movq 24(%rbp), %r14
                        movq 24(%r13), %r13
                        movq 24(%r14), %r14
                        cmpq %r14, %r13
			je eq_true
                        jmp eq_false
.globl eq_end
eq_end:                 ## return address handling
                        movq %rbp, %rsp
                        popq %rbp
                        ret
                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl le_handler
le_handler:             ## helper function for <=
                        pushq %rbp
                        movq %rsp, %rbp
                        movq 32(%rbp), %r12
                        ## return address handling
                        movq 32(%rbp), %r13
                        movq 24(%rbp), %r14
                        cmpq %r14, %r13
			je le_true
                        movq $0, %r15
                        cmpq %r15, %r13
			je le_false
                        cmpq %r15, %r14
			je le_false
                        movq 0(%r13), %r13
                        movq 0(%r14), %r14
                        ## place the sum of the type tags in r1
                        addq %r14, %r13
                        movq $0, %r14
                        cmpq %r14, %r13
			je le_bool
                        movq $2, %r14
                        cmpq %r14, %r13
			je le_int
                        movq $6, %r14
                        cmpq %r14, %r13
			je le_true
.globl le_false
le_false:               ## not less-than-or-equal
                        ## new Bool
                        pushq %rbp
                        pushq %r12
                        movq $Bool..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        jmp le_end
.globl le_true
le_true:                ## less-than-or-equal
                        ## new Bool
                        pushq %rbp
                        pushq %r12
                        movq $Bool..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq $1, %r14
                        movq %r14, 24(%r13)
                        jmp le_end
.globl le_bool
le_bool:                ## two Bools
.globl le_int
le_int:                 ## two Ints
                        movq 32(%rbp), %r13
                        movq 24(%rbp), %r14
                        movq 24(%r13), %r13
                        movq 24(%r14), %r14
                        cmpl %r14d, %r13d
			jle le_true
                        jmp le_false
.globl le_end
le_end:                 ## return address handling
                        movq %rbp, %rsp
                        popq %rbp
                        ret
                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl lt_handler
lt_handler:             ## helper function for <
                        pushq %rbp
                        movq %rsp, %rbp
                        movq 32(%rbp), %r12
                        ## return address handling
                        movq 32(%rbp), %r13
                        movq 24(%rbp), %r14
                        movq $0, %r15
                        cmpq %r15, %r13
			je lt_false
                        cmpq %r15, %r14
			je lt_false
                        movq 0(%r13), %r13
                        movq 0(%r14), %r14
                        ## place the sum of the type tags in r1
                        addq %r14, %r13
                        movq $0, %r14
                        cmpq %r14, %r13
			je lt_bool
                        movq $2, %r14
                        cmpq %r14, %r13
			je lt_int
                        movq $6, %r14
                        cmpq %r14, %r13
.globl lt_false
lt_false:               ## not less than
                        ## new Bool
                        pushq %rbp
                        pushq %r12
                        movq $Bool..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        jmp lt_end
.globl lt_true
lt_true:                ## less than
                        ## new Bool
                        pushq %rbp
                        pushq %r12
                        movq $Bool..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq $1, %r14
                        movq %r14, 24(%r13)
                        jmp lt_end
.globl lt_bool
lt_bool:                ## two Bools
.globl lt_int
lt_int:                 ## two Ints
                        movq 32(%rbp), %r13
                        movq 24(%rbp), %r14
                        movq 24(%r13), %r13
                        movq 24(%r14), %r14
                        cmpl %r14d, %r13d
			jl lt_true
                        jmp lt_false
.globl lt_end
lt_end:                 ## return address handling
                        movq %rbp, %rsp
                        popq %rbp
                        ret
                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl start
start:                  ## program begins here
                        .globl main
			.type main, @function" in
printf "sample text";