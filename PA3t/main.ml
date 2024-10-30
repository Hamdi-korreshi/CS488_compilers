type static_type =  (*static type of cool expression*)
  | Class of string
  | SELF_TYPE of string

type tac_expr =
  | TAC_Variable of string
  | TAC_Int of int
  | TAC_String of string
  | TAC_Bool of bool

  let safe_head lst =
    try Some (List.hd (List.rev lst))
    with Failure _ -> None
  
  type tac_instr =
  | TAC_Assign_Int of string * int
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
  | TAC_Default of string * tac_expr
  | TAC_isvoid of string * tac_expr
  | TAC_call_out of string * string * string (* out_string, out_int *)
  | TAC_call_in of string * string (* in_string, in_int *)
  | TAC_Let of (string * string * tac_expr option ) list * tac_expr

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
open Printf
let rec convert_id ((loc,iname)) =
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
              Bool("true")
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
          let cname = (Filename.chop_extension fname) ^ ".cl-tactest" in 
          let fout = open_out cname in
          (* let rec output_id bruh_val = 
            fprintf fout "%s\n%s\n" (fst bruh_val) (snd bruh_val) 
          in
          let rec output_ints sval = 
            match sval with 
            | Identifier(ival) ->
              fprintf fout "%s\n" (snd ival)
            | Integer(ival) -> fprintf fout "int %s\n" ival
            | _ -> printf "unmatch case\n";
          in
          let counter = ref 0 in
          let rec output_tac e =
            match e.exp_kind with
            | Identifier(ival) ->
              fprintf fout "%s\n" (snd ival);
              print_id ival;
            | Integer(ival) -> fprintf fout "int %s\n" ival
            | String(ival) -> fprintf fout "string\n%s\n" ival
            | Plus(e1,e2) ->
              let t3 = "t$" ^ string_of_int !counter in 
              let () = counter <- !counter + 1  in
              let t1 = "t$" ^ string_of_int !counter in 
              printf "%s\n"  t1;
              fprintf fout "%s <- " t1;
              output_tac e1;
              let () = counter <- !counter + 1  in
              let t2 = "t$" ^ string_of_int !counter in 
              fprintf fout "%s <- " t2;
              output_tac e2;
              fprintf fout "%s <- + %s %s\n" t3 t1 t2
            | _ -> fprintf fout ""
        in *)
        (* let metho_count = ref 0 in
        fprintf fout "comment start\n";
        List.iter (fun ((cloc,cname),inherits, feats) ->
          List.iter (fun feat ->
          match feat with 
          | Attribute((name_loc, name),(dt_loc,dt_type), (Some init_exp)) ->
            fprintf fout "%s <- " name;
            output_tac init_exp
          | Method((metho_loc,metho_name), forms, (metho_type_loc,metho_type), metho_bod) ->
            fprintf fout "label %s_%s_%d\n" cname metho_name !metho_count;
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
        
          | Identifier (_, name) ->
              [], TAC_Variable name
        
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
          | Let (bindings, let_body) ->
            (* Process each binding in the let expression *)
            let rec process_bindings bindings acc_instrs =
              match bindings with
              | [] -> acc_instrs  (* No more bindings; return accumulated instructions *)
              | ((let_var_loc, let_var_name), (let_type_loc, let_type_name), init_opt) :: rest ->
                  let init_instrs =
                    match init_opt with
                    | Some init_exp -> 
                        let expr_instrs, expr_result = convert_expr init_exp (Some let_var_name) in
                        acc_instrs @ expr_instrs
                    | None -> acc_instrs  (* No initialization needed *)
                  in
                  process_bindings rest init_instrs
            in
            (* Generate instructions for all bindings *)
            let binding_instrs = process_bindings bindings [] in
            (* Convert the body expression with the bindings in effect *)
            let body_instrs, body_tac_expr = convert_expr let_body target in
            binding_instrs @ body_instrs, body_tac_expr
            
          | _ -> 
            fprintf fout "hit the let";
            [], TAC_Variable "yeah uhhhh"
          in
        (* Function to print a single TAC instruction *)
        let rec print_tac_instr fout instr =
          match instr with
          | TAC_Assign_Int (var, value) ->
              fprintf fout "%s <- int %d\n" var value
          | TAC_Assign_Var (var, src_var) ->
              fprintf fout "%s <- %s\n" var src_var
          | TAC_Assign_Plus (var, e1, e2) ->
              let e1_str = match e1 with
                           | TAC_Variable v -> v
                           | TAC_Int i -> "int " ^ string_of_int i in
              let e2_str = match e2 with
                           | TAC_Variable v -> v
                           | TAC_Int i -> "int " ^ string_of_int i in
              fprintf fout "%s <- + %s %s\n" var e1_str e2_str
          | TAC_Assign_Minus (var, e1, e2) ->
              let e1_str = match e1 with
                           | TAC_Variable v -> v
                           | TAC_Int i -> "int " ^ string_of_int i in
              let e2_str = match e2 with
                           | TAC_Variable v -> v
                           | TAC_Int i -> "int " ^ string_of_int i in
              fprintf fout "%s <- - %s %s\n" var e1_str e2_str
          | TAC_Assign_Times (var, e1, e2) ->
              let e1_str = match e1 with
                           | TAC_Variable v -> v
                           | TAC_Int i -> "int " ^ string_of_int i in
              let e2_str = match e2 with
                           | TAC_Variable v -> v
                           | TAC_Int i -> "int " ^ string_of_int i in
              fprintf fout "%s <- * %s %s\n" var e1_str e2_str
          | TAC_Assign_Divide (var, e1, e2) ->
              let e1_str = match e1 with
                           | TAC_Variable v -> v
                           | TAC_Int i -> "int " ^ string_of_int i in
              let e2_str = match e2 with
                           | TAC_Variable v -> v
                           | TAC_Int i -> "int " ^ string_of_int i in
              fprintf fout "%s <- / %s %s\n" var e1_str e2_str
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
              fprintf fout "%s <- %s < %s\n" var e1_val e2_val
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
              fprintf fout "%s <- %s <= %s\n" var e1_val e2_val
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
            fprintf fout "%s <- %s = %s\n" var e1_val e2_val
          | TAC_Cnd_Not (var, e1) ->
            (* Just for Booleans *)
            let e1_val = match e1 with
                      | TAC_Variable v -> v
                      | TAC_String i -> "string" 
                      | TAC_Int i -> "int" ^ string_of_int i
                      | TAC_Bool i -> "bool" ^ string_of_bool i in
            fprintf fout "%s <- not %s\n" var e1_val
          | TAC_Negate (var, e1) ->
            (* Only for ints *)
            let e1_val = match e1 with
                      | TAC_Variable v -> v
                      | TAC_String i -> "string"
                      | TAC_Int i -> "int" ^ string_of_int i
                      | TAC_Bool i -> "bool" ^ string_of_bool i in
            fprintf fout "%s <- ~ %s\n" var e1_val
          | TAC_New (var, e1) ->
              let e1_val = match e1 with
                      | TAC_Variable v -> v
                      | TAC_String i -> "string"
                      | TAC_Int i -> "int" ^ string_of_int i 
                      | TAC_Bool i -> "bool" ^ string_of_bool i in
                fprintf fout "%s <- new %s\n" var e1_val
          | TAC_Default (var, e1) ->
              let e1_val = match e1 with
                      | TAC_Variable v -> v
                      | TAC_String i -> "string"  
                      | TAC_Int i -> "int"
                      | TAC_Bool i -> "bool" in
                fprintf fout "%s <- default %s\n" var e1_val
          | TAC_isvoid (var, e1) ->
              let e1_val = match e1 with
                      | TAC_Variable v -> v
                      | TAC_String i -> "string"
                      | TAC_Int i -> "int"
                      | TAC_Bool i -> "bool" in
                fprintf fout "%s <- isvoid %s\n" var e1_val
          | TAC_call_out (var, e1, e2) ->
                fprintf fout "%s <- call %s %s\n" var e1 e2
          | TAC_call_in (var, e1) ->
                fprintf fout "%s <- call %s\n" var e1
    in
        (* Function to output the full list of TAC instructions for a method body *)
        let output_tac fout target e =
          let tac_instrs, _ = convert_expr e target in
          List.iter (print_tac_instr fout) tac_instrs;
          safe_head tac_instrs
        in 
        (* Main program to iterate over the classes and features *)
        let metho_count = ref 0 in
        fprintf fout "comment start\n";
        List.iter (fun ((cloc, cname), inherits, feats) ->
          List.iter (fun feat ->
            match feat with
            | Attribute ((name_loc, name), (dt_loc, dt_type), Some init_exp) ->
                let last = output_tac fout (Some name) init_exp in 
                fprintf fout ""
            | Method ((metho_loc, metho_name), forms, (metho_type_loc, metho_type), metho_bod) ->
                fprintf fout "label %s_%s_%d\n" cname metho_name !metho_count;
                let last = output_tac fout None metho_bod in
                fprintf fout ""
            | Attribute ((_, _), (_, _), None) ->
                printf "bruh"
          ) feats;
        ) ast;
        close_out fout;
        end ;;
main();;