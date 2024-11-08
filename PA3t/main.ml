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

  (* TODO
    Branches, Labels, and Control Flow:
    jmp label - this happens right after the conditional body finishes
    label label - this is the "function Label" - after the conditional is done then write this Label
    return x - only occurs once per method, somewhat handled by current program
    comment ...text until end of line... - some arbrituary program that gives context
    bt x label - if a conditional appears then there is a contradiction variable (that acts as the false) and then the bt's are listed right after that

    Implement way for methods
    Handle not and negate
    Handle new and default
    Research new type and default type
    Stack the methods into one program
  *)

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
          let ast = read_cool_program () in
          close_in fin ;
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
          (* TODO *)
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
            (* Generate unique labels *)
            let classmethod_label = !curr_class ^ "_" ^ !curr_method ^ "_" in
            metho_count := !metho_count + 1;
            (* Format is Classname_funcname_ *)
            let then_label = classmethod_label ^ (string_of_int !metho_count) in
            metho_count := !metho_count + 1;
            let else_label = classmethod_label ^ (string_of_int !metho_count) in
            metho_count := !metho_count + 1;
            let end_label = classmethod_label ^ (string_of_int !metho_count) in
        
            (* Generate a unique variable to hold the if expression result *)
            let if_result = fresh_variable () in
        
            (* Step 1: Process the condition (if_exp) *)
            let cond_instrs, cond_result = convert_expr if_exp None in
        
            (* Step 2: Negate the condition *)
            let negated_var = fresh_variable () in
            let negate_instr = TAC_Cnd_Not (negated_var, cond_result) in
        
            (* Step 3: Generate the jump to the else branch using the negated condition *)
            let jump_to_else = TAC_Jump_If_Not (TAC_Variable negated_var, else_label) in
        
            (* Step 4: Process the 'then' branch, storing the result in if_result *)
            let then_instrs, then_result = convert_expr then_exp (Some if_result) in
            let then_branch = [TAC_Label then_label] @ then_instrs @ [TAC_Assign_Var (if_result, match then_result with
                                                    | TAC_Variable v -> v
                                                    | _ -> failwith "Unexpected TAC expression")] @ [TAC_Jump end_label] in
        
            (* Step 5: Process the 'else' branch, also storing the result in if_result *)
            let else_instrs, else_result = convert_expr else_exp (Some if_result) in
            let else_branch = [TAC_Label else_label] @ else_instrs @ [TAC_Assign_Var (if_result, match else_result with
                                                    | TAC_Variable v -> v
                                                    | _ -> failwith "Unexpected TAC expression")] in
        
            (* Step 6: Combine all instructions with labels and add a return statement *)
            cond_instrs
            @ [negate_instr]                      (* Negate the condition *)
            @ [jump_to_else]                      (* Jump to else if negated condition is true *)
            @ then_branch                         (* Then branch instructions *)
            @ else_branch                         (* Else branch instructions *)
            @ [TAC_Label end_label]               (* End label *)
            @ [TAC_Return if_result], TAC_Variable if_result
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
        let rec print_tac_instr instr =
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
              let e1_str = match e1 with
                           | TAC_Variable v -> v
                           | TAC_Int i -> "int " ^ string_of_int i in
              let e2_str = match e2 with
                           | TAC_Variable v -> v
                           | TAC_Int i -> "int " ^ string_of_int i in
              printf "%s <- + %s %s\n" var e1_str e2_str
          | TAC_Assign_Minus (var, e1, e2) ->
              let e1_str = match e1 with
                           | TAC_Variable v -> v
                           | TAC_Int i -> "int " ^ string_of_int i in
              let e2_str = match e2 with
                           | TAC_Variable v -> v
                           | TAC_Int i -> "int " ^ string_of_int i in
              printf "%s <- - %s %s\n" var e1_str e2_str
          | TAC_Assign_Times (var, e1, e2) ->
              let e1_str = match e1 with
                           | TAC_Variable v -> v
                           | TAC_Int i -> "int " ^ string_of_int i in
              let e2_str = match e2 with
                           | TAC_Variable v -> v
                           | TAC_Int i -> "int " ^ string_of_int i in
              printf "%s <- * %s %s\n" var e1_str e2_str
          | TAC_Assign_Divide (var, e1, e2) ->
              let e1_str = match e1 with
                           | TAC_Variable v -> v
                           | TAC_Int i -> "int " ^ string_of_int i in
              let e2_str = match e2 with
                           | TAC_Variable v -> v
                           | TAC_Int i -> "int " ^ string_of_int i in
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
          List.iter (print_tac_instr) tac_instrs;
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