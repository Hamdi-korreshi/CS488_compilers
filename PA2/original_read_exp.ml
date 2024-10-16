     and read_exp () =
          let eloc = read () in
          let ekind = match read () with
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
            (
              match snd ival with
              | (Integer _) ->
                Negate(ival)
              | _ ->
                let iloc = fst ival in
                let itype = match snd ival with
                | Integer _ -> "Int"
                | Bool _ -> "Bool"
                | String _ -> "String"
                | _ -> "Object"
              in
              if itype = "Object" then 
                ()
              else
                Printf.printf "ERROR: %s: Type-Check: negate applied to type %s instead of Int\n" iloc itype;
                exit 1
              )
          | "lt" ->
            (* Get the type of the datatype then push into bool_error *)
            let ival = read_exp() in
            let xval = read_exp() in 
            (
              match (snd ival, snd xval) with
              | (Integer _, Integer _) ->
                LT(ival, xval)
              | (String _, String _) ->
                LT(ival, xval)
              | _ ->
                bool_error (ival, xval)  )
          | "le" ->
            (* Get the type of the datatype then push into bool_error *)
            let ival = read_exp() in
            let xval = read_exp() in 
            (
              match (snd ival, snd xval) with
              | (Integer _, Integer _) ->
                LE(ival, xval)
              | (String _, String _) ->
                LE(ival, xval)
              | _ ->
                bool_error (ival, xval)  )
          | "eq" ->
            (* Get the type of the datatype then push into bool_error *)
            let ival = read_exp() in
            let xval = read_exp() in 
            (
              match (snd ival, snd xval) with
              | (Integer _, Integer _) ->
                EQ(ival, xval)
              | (String _, String _) ->
                EQ(ival, xval)
              | _ ->
                bool_error (ival, xval)  )
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
            print_exp e;
            let metho = read_id () in 
            print_id metho;
            let args = read_list read_exp in 
            List.iter print_exp args;
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
                    | _ -> failwith "binding failed, invalid let"
                    in 
                    binding_list (n-1) (binding :: acc)
                ) in
              let bindings = binding_list num_bindings [] in 
              let let_body = read_exp () in
              Let(bindings, let_body)
          | "plus" -> (* might have to change all of these*)
              let ival = read_exp() in
              let xval = read_exp() in (
              match (snd ival, snd xval) with
              | (Integer _, Integer _) ->
                Plus(ival, xval)
              | _ ->
                (* arth_error (ival,xval) ) *) 
                Plus(ival, xval))
          | "minus" ->
              let ival = read_exp() in
              let xval = read_exp() in (
              match (snd ival, snd xval) with
              | (Integer _, Integer _) ->
                Minus(ival, xval)
              | _ ->
                (* arth_error (ival,xval) ) *) 
                Minus(ival, xval))
          | "times" -> 
              let ival = read_exp() in
              let xval = read_exp() in
              (
              match (snd ival, snd xval) with
              | (Integer _, Integer _) ->
                Times(ival, xval)
              | _ ->
                (* arth_error (ival,xval) ) *) 
                Times(ival, xval) )
          | "divide" -> 
              let ival = read_exp() in
              let xval = read_exp() in
              (
              match (snd ival, snd xval) with
              | (Integer _, Integer _) ->
                Divide(ival, xval)
              | _ ->
                (* arth_error (ival,xval) ) *) 
                Divide(ival, xval) )
          | "new" -> (*have to chage this*)
            let ival = read_id() in
            New(ival)
          | "identifier" ->
            let ival = read_id () in
            Identifier(ival)