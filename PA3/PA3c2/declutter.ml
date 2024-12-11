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
  let rec tac_to_asm instr =
    match instr with
    | TAC_Assign_String (var, value) ->
      let class_inst = new_class_instance "String" var in
      let stror_to_mem = 
        [Mov("\t\t\tmovq $String8, %r14\n");
        Mov("\t\t\tmovq %r14, 24(%r13)\n");
        Mov("\t\t\tmovq %r13, 0(%rbp)\n")] in
      class_inst @ stror_to_mem;
    | TAC_Jump_If_Not (cond_expr, label) ->
      [Mov("\t\t\tneed to fix jump if not\n")]
    | TAC_Default (varname, sometype) ->
      [Mov("\t\t\tneed to fix defaultt\n")]
    | TAC_Jump label ->
      [Jmp("\t\t\tneed to fix jump label\n")]
    | TAC_Label label ->
      [Comment(label^"\n")]
    | TAC_Assign_Int (var, value) ->
      let class_inst = new_class_instance "String" var in
      let stror_to_mem = 
        [Mov("\t\t\tmovq $"^string_of_int value^", %r14\n");
        Mov("\t\t\tmovq %r14, 24(%r13)\n");
        Mov("\t\t\tmovq %r13, 0(%rbp)\n")] in
      class_inst @ stror_to_mem;
    | TAC_Assign_Bool (var, value) ->
      let class_inst = new_class_instance "String" var in
      let stror_to_mem = 
        [Mov("\t\t\tmovq $String8, %r14\n");
        Mov("\t\t\tmovq %r14, 24(%r13)\n");
        Mov("\t\t\tmovq %r13, 0(%rbp)\n")] in
      class_inst @ stror_to_mem;
    | TAC_Assign_Var (var, src_var) ->
      [Mov("\t\t\tneed to fix assign\n")]
      (* printf "%s <- %s\n" var src_var *)
    | TAC_Assign_Plus (var, e1, e2) ->
      [Mov("\t\t\tmovq 0(%rbp), %r14\n");
      Add("\t\t\taddq %r14, %r13\n");
      Mov("\t\t\t %r13, 0(%rbp)\n")]
    | TAC_Assign_Times (var, e1, e2) ->
      [Mov("\t\t\tneed to fix times\n")]
    | TAC_Assign_Divide (var, e1, e2) ->
      [Mov("\t\t\tneed to fix divide\n")]
    | TAC_Cnd_LessThan (var, e1, e2) ->
      [Mov("\t\t\tneed to fix lees than\n")]
    | TAC_Negate (var, e1) ->
      [Mov("\t\t\tneed to fix the negatation\n")]
    | TAC_New (var, e1) ->
      [Mov("\t\t\tneed to fix the new\n")]
    | TAC_isvoid (var, e1) ->
      [Mov("\t\t\tneed to fix the isvoid\n")]
    | TAC_call_out (var, e1, e2) ->
      [Mov("\t\t\tneed to fix the call out\n")]
    | TAC_call_in (var, e1) ->
      [Mov("\t\t\tneed to fix the call in\n")]        
    | TAC_Self_Dispatch (result_var, (method_name, loc), args) ->
      [Mov("\t\t\tneed to fix the self dispatch\n")]
    | TAC_Return result_var ->
      [Mov("\t\t\tret")]
    | TAC_Let (bingings, let_body) ->
      [Mov("\t\t\tneed to fix the tac let")]
    | x ->
      [Mov("\t\t\tmissing case for x\n")]
  in

  type asm_tree = asm list
  and asm = 
    | Push of string
    | Pop of string
    | Add of string
    | Sub of string
    | Div of string
    | Mult of string
    | Mov of string
    | Call of string 
    | Ret of string 
    | Jmp of string
    | Start_label of string
    | End_label of string 
    | Comment of string