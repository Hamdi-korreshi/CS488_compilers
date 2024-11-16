open Printf
open Str
type static_type =  (*static type of cool expression*)
  | Class of string
  | SELF_TYPE of string

type tac_expr =
  | TAC_Variable of string
  | TAC_Int of int
  | TAC_String of string
  | TAC_Bool of bool

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

type class_map = (string, (string * string * exp option) list option) Hashtbl.t
type implementation_map = (loc, (loc * (loc) list * loc * exp) list) Hashtbl.t
type parent_map = (loc,loc) Hashtbl.t

  let rec output_cool_prog ast =
    output_classes ast
  
  and output_classes ast =
    printf "Annotated ast\n";
    printf "%d\n" (List.length ast);
    List.iter output_class ast
  
  and output_class ast =
    match ast with
    | (class_bruh, None, feats) ->
        output_id class_bruh;
        printf "no_inherits\n";
        printf "%d\n" (List.length feats);
        List.iter output_feature feats
    | (class_bruh, Some parent_class, feats) ->
        output_id class_bruh;
        printf "inherits\n";
        output_id parent_class;
        printf "%d\n" (List.length feats);
        List.iter output_feature feats
  
  and output_feature ast_feature =
    match ast_feature with
    | Attribute (attr_name, attr_type, None) ->
        printf "attribute_no_init\n";
        output_id attr_name;
        output_id attr_type
    | Attribute (attr_name, attr_type, Some init_exp) ->
        printf "attribute_init\n";
        output_id attr_name;
        output_id attr_type;
        output_exp init_exp
    | Method (metho_name, formals, metho_type, body_exp) ->
        printf "method\n";
        output_id metho_name;
        printf "%d\n" (List.length formals);
        List.iter output_formal formals;
        output_id metho_type;
        output_exp body_exp
  
  and output_formal (form_name, form_type) =
    output_id form_name;
    output_id form_type
  
  and output_exp exp =
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
  
  and output_id (loc, name) =
    printf "%s\n%s\n" loc name
  

let main () = begin
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
  let rec read_id () =
    let loc = read () in
    let name = read() in
    (loc, name)
  in
  let rec read_exp read =
    (* Read location *)
    let eloc = read () in
    let static_type =
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
      static_type = None;
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
  
  and read_exps n acc =
    if n = 0 then List.rev acc
    else
      let exp = read_exp () in
      read_exps (n - 1) (exp :: acc)
  
  and read_id () =
    let loc = read () in
    let name = read () in
    (loc, name)
  in
  let rec output_id bruh_val = 
    printf "%s\n%s\n" (fst bruh_val) (snd bruh_val) 
  in
  let rec output_exp e =
    (* output the type for class map *)
    printf "%s\n" e.loc;
    (match e.static_type with 
     | None -> printf "";
     | Some(Class(c)) -> printf "%s\n" c
     | Some(SELF_TYPE(c)) -> printf "SELF_TYPE\n");
    match e.exp_kind with
    | Integer(ival) -> printf "integer\n%s\n" ival
    | String(ival) -> printf "string\n%s\n" ival
    | Bool(ival) -> (
        match ival with 
        | "true" -> printf "bool\ntrue\n"
        | "false" -> printf "bool\nfalse\n"
        | _ -> printf "")
    | Plus(ival, xval) ->
        printf "plus\n"; output_exp ival; output_exp xval
    | Times(ival, xval) ->
        printf "times\n"; output_exp ival; output_exp xval
    | Divide(ival, xval) ->
        printf "divide\n"; output_exp ival; output_exp xval
    | Minus(ival, xval) ->
        printf "minus\n"; output_exp ival; output_exp xval
    | Let(bindings, let_body) ->
        printf "let\n";
        printf "%d\n" (List.length bindings);
        List.iter (fun bind ->
          match bind with
          | (let_vare, var_type, None) -> 
              printf "let_binding_no_init\n";
              output_id let_vare;
              output_id var_type
          | (let_vare, var_type, Some init_exp) -> 
              printf "let_binding_init\n";
              output_id let_vare;
              output_id var_type;
              output_exp init_exp
        ) bindings;
        output_exp let_body
    | Block(expr_list) ->
        printf "block\n";
        printf "%d\n" (List.length expr_list); 
        List.iter output_exp expr_list
    | Case(test_exp, case_list) ->
        printf "case\n";
        output_exp test_exp;
        printf "%d\n" (List.length case_list);  
        List.iter (fun (var, var_type, case_body) ->
          output_id var;
          output_id var_type;
          output_exp case_body
        ) case_list
    | Identifier(ival) ->
        printf "identifier\n";
        output_id ival
    | New(ival) ->
        printf "new\n";
        output_id ival
    | If(if_exp, then_exp, else_exp) ->
        printf "if\n";
        output_exp if_exp;
        output_exp then_exp;
        output_exp else_exp
    | While(loop, pool) ->
        printf "while\n";
        output_exp loop;
        output_exp pool
    | Assign(var, rhs_exp) ->
        printf "assign\n";
        output_id var;
        output_exp rhs_exp
    | Isvoid(void) ->
        printf "isvoid\n";
        output_exp void
    | Dynamic_Dispatch(e, metho, args) -> 
        printf "dynamic_dispatch\n";
        output_exp e;
        output_id metho;
        printf "%d\n" (List.length args);
        List.iter output_exp args
    | Static_Dispatch(e, ftype, metho, args) -> 
        printf "static_dispatch\n";
        output_exp e;
        output_id ftype;
        output_id metho;
        printf "%d\n" (List.length args);
        List.iter output_exp args
    | Self_Dispatch(metho, args) -> 
        printf "self_dispatch\n";
        output_id metho;
        printf "%d\n" (List.length args);
        List.iter output_exp args
    | LT(ival, xval) -> 
        printf "lt\n"; output_exp ival; output_exp xval
    | LE(ival, xval) -> 
        printf "le\n"; output_exp ival; output_exp xval
    | EQ(ival, xval) -> 
        printf "eq\n"; output_exp ival; output_exp xval
    | Negate(ival) -> 
        printf "negate\n"; output_exp ival
    | Not(ival) -> 
        printf "not\n"; output_exp ival
    | Internal((type_loc, typ_name), class_name, metho_name) -> 
        printf "internal\n";
        printf "%s.%s\n" class_name metho_name
    in
    let print_class_map (cmap: class_map) =
      printf "class_map\n";
      printf "%d\n" (Hashtbl.length cmap);
      Hashtbl.iter (fun class_name attributes_opt ->
        printf "%s\n" class_name;
        match attributes_opt with
        | None ->
            printf "0\n"
        | Some attributes ->
            printf "%d\n" (List.length attributes);
            List.iter (fun (aname, atype, attr_initializer_opt) ->
              match attr_initializer_opt with
              | None ->
                  printf "no_initializer\n";
                  printf "%s\n%s\n" aname atype
              | Some exp ->
                  printf "initializer\n";
                  printf "%s\n%s\n" aname atype;
                  output_exp exp
            ) attributes
      ) cmap    
  in
  let print_implementation_map imp_map =
    let print_formals formals_opt =
      match formals_opt with
      | None ->
          print_endline "No formals"
      | Some formals ->
          List.iter (fun formal_name ->
            Printf.printf "Formal: %s\n" formal_name
          ) formals
    in
    Hashtbl.iter (fun class_name methods ->
      Printf.printf "Class: %s\n" class_name;
      List.iter (fun (method_name, formals, defined_in_class, body_exp) ->
        Printf.printf "  Method: %s\n" method_name;
        Printf.printf "  Defined in: %s\n" defined_in_class;
        Printf.printf "  Formals:\n";
        print_formals (Some formals); (* Adjust as needed based on your data structure *)
        Printf.printf "  Body Expression:\n";
        output_exp body_exp; (* Assuming `output_exp` handles the printing of expressions *)
      ) methods
    ) imp_map
  in
  let print_parent_map pmap = 
    Hashtbl.iter ( fun child parent -> 
      printf "Child: %s, Parent: %s\n" child parent;) 
    pmap
  in
  let cmap: class_map = read_class_map in 
  let imp_map: implementation_map = read_implementation_map in
  let pmap: parent_map = read_parent_map in 
  printf "read ann_ast\n";
  let ann_ast = read_cool_prog () in 
  print_class_map cmap;
  print_implementation_map imp_map;
  print_parent_map pmap;
  printf "Annoated ast";
  output_cool_prog ann_ast;
end;;
main();;