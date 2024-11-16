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
type cmap = (string, int) Hashtbl.t
type implementation_map = (string, method_entry list) Hashtbl.t 
and method_entry = method_name * num_formals * formal_names * defining_class * method_body
and method_name = string
and num_formals = int
and formal_names = string list option
and defining_class = string
and method_body = string
type pmap = (string,string) Hashtbl.t

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

let print_class_map class_map =
  printf "got to print\n";
  Printf.printf "class_map\n";
  Printf.printf "%d\n" (Hashtbl.length class_map);
  Hashtbl.iter (fun class_name attributes ->
    Printf.printf "%s\n" class_name;
    Printf.printf "%d\n" attributes
  ) class_map
    

let print_implementation_map imp_map =
  Printf.printf "implementation_map\n";
  Printf.printf "%d\n" (Hashtbl.length imp_map);
  Hashtbl.iter (fun class_name methods ->
    Printf.printf "%s\n" class_name;
    Printf.printf "%d\n" (List.length methods);
    List.iter (fun (method_name, num_formals, formals, defining_class, method_body) ->
      Printf.printf "%s\n" method_name;
      Printf.printf "%d\n" num_formals;
      (* Print formal parameters only if there are any *)
      (match formals with
      | Some formal_names -> List.iter (fun formal_name -> Printf.printf "%s\n" formal_name) formal_names
      | None -> ());  (* No formal parameters, do nothing *)
      Printf.printf "%s\n" defining_class;
      Printf.printf "%s\n" method_body;
    ) methods
  ) imp_map
  
let print_parent_map parent_map =
  Printf.printf "parent_map\n";
  Printf.printf "%d\n" (Hashtbl.length parent_map);
  Hashtbl.iter (fun child_class parent_class ->
    Printf.printf "%s\n%s\n" child_class parent_class
  ) parent_map
  
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
let rec print_exp exp =
  Printf.printf "Expression (location: %s)\n" exp.loc;
  (match exp.static_type with
    | None -> Printf.printf "Static type: None\n"
    | Some(Class(c)) -> Printf.printf "Static type: Class %s\n" c
    | Some(SELF_TYPE(c)) -> Printf.printf "Static type: SELF_TYPE %s\n" c);
  match exp.exp_kind with
  | Not value ->
    Printf.printf "  Not: \n";
    print_exp value
  | Integer value -> Printf.printf "  Integer: %s\n" value
  | Bool value -> Printf.printf "  Bool: %s\n" value
  | String value -> Printf.printf "  String: %s\n" value
  | Plus(exp1, exp2) ->
      Printf.printf "plus\n";
      print_exp exp1;
      print_exp exp2
  | Minus(exp1, exp2) ->
      Printf.printf "minus\n";
      print_exp exp1;
      print_exp exp2
  | Times(exp1, exp2) ->
      Printf.printf "times\n";
      print_exp exp1;
      print_exp exp2
  | Divide(exp1, exp2) ->
      Printf.printf "divide\n";
      print_exp exp1;
      print_exp exp2
  | Block exp_list ->
      Printf.printf "block\n";
      List.iter print_exp exp_list
  | Assign (var, rhs_exp) ->
      print_id var;
      print_exp rhs_exp
  | Isvoid void_exp -> 
      print_exp void_exp
  | If (if_exp, then_exp, else_exp) ->
      print_exp if_exp;
      print_exp then_exp;
      print_exp else_exp
  | While (loop, pool) ->
      print_exp loop; 
      print_exp pool
  | Let (bindings, let_body) ->
      Printf.printf "let\n";
      List.iter (fun ((vloc, vname), (typeloc, typename), init_exp) ->
        Printf.printf " Bindings: %s: %s\n" vname typename;
        (match init_exp with
          | None -> Printf.printf " No init\n"
          | Some init -> Printf.printf " Init:\n"; print_exp init)
      ) bindings;
      Printf.printf "in\n";
      print_exp let_body
  | Identifier ival ->
      print_id ival
  | Case (test_exp, case_list) ->
      Printf.printf "First exp:\n";
      print_exp test_exp;
      List.iter (fun ((vloc, vname), (tloc, tname), rest_exp) ->
        Printf.printf "  Case: %s: %s\n" vname tname;
        print_exp rest_exp
      ) case_list
  | New ((loc_ival, ival_name)) ->
      Printf.printf "new\n";
      Printf.printf "  New Object: %s\n" ival_name
  | Dynamic_Dispatch (e, metho, args) ->
      Printf.printf "Dynamic Dispatch:\n";
      print_exp e;
      print_id metho;
      Printf.printf "  Args:\n";
      List.iter print_exp args
  | Static_Dispatch (e, stat_type, metho, args) ->
      Printf.printf "Static Dispatch:\n";
      print_exp e;
      print_cool_type stat_type;
      print_id metho;
      Printf.printf "  Args:\n";
      List.iter print_exp args
  | Self_Dispatch (metho, args) ->
      Printf.printf "Self Dispatch:\n";
      print_id metho;
      Printf.printf "  Args:\n";
      List.iter print_exp args
  | LT (exp1, exp2) ->
      Printf.printf "LT\n";
      print_exp exp1;
      print_exp exp2
  | LE (exp1, exp2) ->
      Printf.printf "LE\n";
      print_exp exp1;
      print_exp exp2
  | EQ (exp1, exp2) ->
      Printf.printf "EQ\n";
      print_exp exp1;
      print_exp exp2
  | Negate exp1 ->
      Printf.printf "Negate\n";
      print_exp exp1

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

  let rec range k =
    if k <= 0 then []
    else k :: (range (k -1 ))
  in

  let read_list worker =
    let k = int_of_string ( read ()) in
    (* printf "read_list of %d\n" k; *)
    let lst = range k in
    List.map (fun _ -> worker ()) lst
  in

  (*many mutually-recursive procedures to rea d in the CL-AST file *)
  let rec read_cool_program () =
      read_list read_cool_class

      and read_id () =
          let loc = read () in
          let name = read() in
          (loc, name)

      and read_cool_class () = (* CLASS *)
          let cname = read_id () in
          let inherits = match read() with
          | "no_inherits" -> None
          | "inherits" ->
            let super = read_id () in
            Some(super)
            (* features =  inherited class features*)
            (* features PLUS= current class features*)
          | x ->
            failwith ("cannot happen: " ^ x)
          in
          let features = read_list read_feature in 
          (cname, inherits, features)

      and read_feature () =
        match read() with
        | "attribute_no_init" ->
          let fname = read_id () in
          let ftype = read_id () in
          Attribute (fname, ftype, None)
        | "attribute_init" ->
          let fname = read_id () in
          let ftype = read_id () in
          let finit = read_exp () in
          Attribute(fname, ftype, (Some finit))
        | "method" ->
          let mname = read_id () in
          let formals = read_list read_formal in
          let mtype = read_id () in
          let mbody = read_exp () in
          Method(mname, formals, mtype, mbody)
        | x ->
          printf "%s\n" fname;
          failwith ("cannot happen: " ^ x)

      and read_formal () =
          let fname = read_id () in
          let ftype = read_id () in
          (fname, ftype)
      and read_exp () =
          let eloc = read () in
          let ekind = match read () with
          | "not" ->
            let e = read_exp () in 
            Not(e)
          | "integer" ->
              let ival = read () in 
              Integer(ival)
          | "string" ->
              let ival = read () in 
              String(ival)
          | "true" ->
              Bool("true")
          | "false" ->
              Bool("false")
          | "negate" ->
            let ival = read_exp() in
            Negate(ival)
          | "lt" ->
            (* Get the type of the datatype then push into bool_error *)
            let ival = read_exp() in
            let xval = read_exp() in
            LT(ival, xval)
          | "le" ->
            (* Get the type of the datatype then push into bool_error *)
            let ival = read_exp() in
            let xval = read_exp() in 
            LE(ival, xval)
          | "eq" ->
            (* Get the type of the datatype then push into bool_error *)
            let ival = read_exp() in
            let xval = read_exp() in 
            EQ(ival, xval)
          | "assign" ->
            let var = read_id () in 
            let rhs_exp = read_exp () in 
            Assign(var,rhs_exp)
          | "isvoid" -> 
            let void = read_exp () in 
            Isvoid(void)
          | "if" ->
            let if_exp = read_exp () in 
            let then_exp = read_exp () in 
            let else_exp = read_exp () in 
            If(if_exp,then_exp,else_exp)
          | "while" ->
            let loop = read_exp () in 
            let pool = read_exp () in 
            While(loop,pool)
          | "block" ->
            let amount_to_read = int_of_string(read ()) in
            let rec read_block n acc =
              if n <= 0 then List.rev acc
              else (
                let expr = read_exp () in 
                read_block (n-1) (expr :: acc)
              ) in 
            let exp_list = read_block amount_to_read [] in
            Block(exp_list)
          | "case" ->
            let test_exp = read_exp () in 
            let read_case () = 
              let case_var = read_id () in 
              let case_type = read_id () in 
              let case_exp = read_exp () in 
              (case_var, case_type, case_exp)
            in 
            let case_list = read_list read_case in
            Case(test_exp,case_list)
          | "dynamic_dispatch" ->
            let e = read_exp () in
            let metho = read_id () in 
            let args = read_list read_exp in 
            Dynamic_Dispatch(e,metho,args)
          | "static_dispatch" ->
            let e = read_exp () in
            let t_static = read_id () in
            let metho = read_id () in 
            let args = read_list read_exp in 
            Static_Dispatch(e,t_static,metho,args)
          | "self_dispatch" ->
            let metho = read_id () in 
            let args = read_list read_exp in 
            Self_Dispatch(metho,args)
          | "let" ->
              let num_bindings = int_of_string (read ()) in
              let rec binding_list n acc =
                if n <= 0 then List.rev acc
                else (
                  let lbni = read () in 
                  let let_var = read_id () in 
                  let let_type = read_id () in 
                  let binding = 
                    match lbni with
                    | "let_binding_no_init" -> (let_var, let_type, None)
                    | "let_binding_init" ->
                      let init_exp = read_exp () in 
                      (let_var, let_type, Some init_exp)
                    | _ -> (let_var, let_type, None) (* failwith "binding failed, invalid let" *)
                    in 
                    binding_list (n-1) (binding :: acc)
                ) in
              let bindings = binding_list num_bindings [] in 
              let let_body = read_exp () in
              Let(bindings, let_body)
          | "plus" -> (* might have to change all of these*)
              let ival = read_exp() in
              let xval = read_exp() in
              Plus(ival, xval)
          | "minus" ->
              let ival = read_exp() in
              let xval = read_exp() in
              Minus(ival, xval)
          | "times" -> 
            let ival = read_exp() in
            let xval = read_exp() in
            Times(ival, xval)
          | "divide" -> 
            let ival = read_exp() in
            let xval = read_exp() in
            Divide(ival, xval)
          | "new" -> (*have to change this*)
            let ival = read_id() in
            New(ival)
          | "identifier" ->
            let ival = read_id () in
            Identifier(ival)
          | x -> (* Fixme: do all of the others*)
            printf "%s\n" eloc;
            failwith ("expression kind unhandled: " ^ x)
          in
          {
            loc = eloc;
            exp_kind = ekind;
            static_type = None;
          }
          in
          
          let read_class_map () = 
            let class_map: cmap = empty_map () in
            let not_need =  read () in 
            let num_classes = int_of_string (read ()) in 
            printf "%d\n" num_classes;
            let rec parse_class_map n = 
              if n > 0 then begin
                let class_name = read () in 
                let num_attr = int_of_string (read ()) in 
                Hashtbl.add class_map class_name num_attr;
                parse_class_map (n-1)
              end;
            in parse_class_map num_classes;
            class_map
          in
          
          let read_imp_map () =
            let imp_map: implementation_map = empty_map () in 
            let not_need =  read () in
            let num_classes = int_of_string (read ()) in  (* Number of classes in the implementation map *)
            
            (* Helper function to read a method entry as a tuple *)
            let read_method () : method_entry =
              let method_name = read () in
              printf "metho name: %s\n" method_name;
              let num_formals = int_of_string (read ()) in
              printf "# forms: %d\n" num_formals;
              let formals =
                if num_formals > 0 then
                  Some (List.init num_formals (fun _ -> read ()))
                else
                  None  (* No formal parameters *)
              in
              let defining_class = read () in
              printf "defining class: %s\n" defining_class;
              let method_body = read () in
              (method_name, num_formals, formals, defining_class, method_body)
            in
          
            (* Helper function to read all methods for a class *)
            let read_class_methods () =
              let class_name = read () in
              printf "class name: %s\n" class_name;
              let num_methods = int_of_string (read ()) in
              let methods = List.init num_methods (fun _ -> read_method ()) in
              Hashtbl.add imp_map class_name methods
            in
          
            (* Read each class in the implementation map *)
            for _ = 1 to num_classes do
              read_class_methods ()
            done;
            
            imp_map
          in
          
          let read_parent_map () = 
            let parent_map: pmap = empty_map () in
            let not_need =  read () in 
            let num_classes = int_of_string (read ()) in 
            let rec parse_class_map n = 
              if n > 0 then begin
                let class_name = read () in 
                let parent_class = read () in 
                Hashtbl.add parent_map class_name parent_class;
                parse_class_map (n-1)
              end;
            in parse_class_map num_classes;
            parent_map
          in
          printf "Starting class map \n";
          let class_map = read_class_map () in
          printf "\nfinished class map \n";
          printf "starting imp map \n";
          let imp_map = read_imp_map () in
          printf "\nfinished imp map \n";
          let par_map = read_parent_map () in
          close_in fin ;
          
          print_class_map class_map;
          printf "Starting imp map \n";
          printf "finished class map \n";
          print_implementation_map imp_map;
          printf "Starting parent map \n";
          print_parent_map par_map;
          
          exit 1;
          let ast = read_cool_program () in
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
        end ;;
main();;

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
                        ret
                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.globl Main..new
Main..new:              ## constructor for Main
                        pushq %rbp
                        movq %rsp, %rbp
                        ## stack room for temporaries: 1
                        movq $8, %r14
                        subq %r14, %rsp
                        ## return address handling
                        movq $5, %r12
                        movq $8, %rsi
			movq %r12, %rdi
			call calloc
			movq %rax, %r12
                        ## store class tag, object size and vtable pointer
                        movq $12, %r14
                        movq %r14, 0(%r12)
                        movq $5, %r14
                        movq %r14, 8(%r12)
                        movq $Main..vtable, %r14
                        movq %r14, 16(%r12)
                        ## initialize attributes
                        ## self[3] holds field y (Int)
                        ## new Int
                        pushq %rbp
                        pushq %r12
                        movq $Int..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq %r13, 24(%r12)
                        ## self[4] holds field x (Int)
                        ## new Int
                        pushq %rbp
                        pushq %r12
                        movq $Int..new, %r14
                        call *%r14
                        popq %r12
                        popq %rbp
                        movq %r13, 32(%r12)
                        ## self[3] y initializer -- none 
                        ## self[4] x initializer -- none 
                        movq %r12, %r13
                        ## return address handling
                        movq %rbp, %rsp
                        popq %rbp
                        ret
                        ## ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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