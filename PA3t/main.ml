type static_type =  (*static type of cool expression*)
  | Class of string
  | SELF_TYPE of string

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

let method_tac (mymethod : feature) =
  printf "A"
  ;


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
          let user_classes = List.map (fun ((_, cname),_,_) -> cname) ast in
          (* Acquire the methods of said class *)
          let currClass = List.hd user_classes;
          let currMethods = 
            let _ , _, features = List.find (fun ((_, currClass), _, _) -> cname = currClass) ast in;
            List.filter(fun feat -> match feat with 
            | Attribute -> false
            | Method -> true
            ) features
          with Non_found -> []
        in
        
          while (List.length currMethods <> 0 & is_empty (user_classes) ) do
            user_class = List.tail user_classes
            currClass = List.hd user_classes
            _,_, features = List.find (fun ((_, currClass), _, _) -> = currClass)
            printf "a\n";
        done
        if (List.length currMethods > 0 ) then
          run_method_function

          (* user_classes = List.tail user_class *)
          (* List.iter(fun meth -> 
            match meth with
            | Method())
            | Attribute _ -> failwith "method unexpected"
            ) *)
          in ast
        end ;;