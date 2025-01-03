(* Hamdi Korreshi and Tomasz Brauntsch
  PA2 Semantic Analyzer FUll *)

  (* TODO: Give this a look
  * original_read_exp.ml has the previous structure of read_exp() 
  * Only read_exp() was edited due to other areas just need modifications, e.g read_feature()
  * Included struture and helper functions for (O)bject, (M)ethod, and (C)lass environments
  * Refer to my discord msg from last night (10/10/2024) talking about what environment is responsible for
  *)
  
  module StringMap = Map.Make(String)

  type static_type =  (*static type of cool expression*)
    | Class of string
    | SELF_TYPE of string
  
  let type_to_str t = match t with
    | Class(x) -> x
    | SELF_TYPE(x) -> "SELF_TYPE"
  
  let rec is_sub t1 t2 = 
    match t1,t2 with 
    | Class(x), Class(y) when x = y -> true
    | Class(x), Class("Object") -> true 
    | Class(x), Class(y) -> false (* treat later, check parent map *)
    | _, _ -> false (*check the class notes*)
  
  type attribute = {
    attr_name: string;
    attr_type: string;
  }
  
  type _method = {
    method_name: string;
    return_type: string;
    params: (string * string) list; (* list of (param_name, param_type) *)
  }
  
  type _class = {
    class_name: string;
    parent_class: string option;  (* Optional parent class for inheritance *)
    attributes: attribute list;   (* List of attributes *)
    methods: _method list;        (* List of methods *)
  }
  
  type class_env = _class StringMap.t
  
  (* Adding a new class *)
  let add_class (env: class_env) (cls: _class) : class_env =
    StringMap.add cls.class_name cls env
  
  (* Looking up a class *)
  let lookup_class (env: class_env) (class_name: string) : _class option =
    StringMap.find_opt class_name env
  
  (* Each class will have its own method environment *)
  type method_env = _method StringMap.t
  
  (* Adding a new method to a method environment *)
  let add_method (env: method_env) (meth: _method) : method_env =
    StringMap.add meth.method_name meth env
  
  (* Looking up a method *)
  let lookup_method (env: method_env) (method_name: string) : _method option =
    StringMap.find_opt method_name env
  
  
  
  type obj_env = string StringMap.t  (* Maps object names (identifiers) to their types *)
  
  (* Adding a new object (variable) to the object environment *)
  let add_object (env: obj_env) (obj_name: string) (obj_type: string) : obj_env =
    StringMap.add obj_name obj_type env
  
  (* Looking up an object (variable) *)
  let lookup_object (env: obj_env) (obj_name: string) : string option =
    StringMap.find_opt obj_name env
  
  (* Initialize a class environment with some built-in classes *)
  let initial_class_env : class_env =
    let object_class = {
      class_name = "Object";
      parent_class = None;
      attributes = [];
      methods = [
        {method_name = "abort"; return_type = "Object"; params = []};
        {method_name = "type_name"; return_type = "String"; params = []};
        {method_name = "copy"; return_type = "SELF_TYPE"; params = []};
      ]
    } in
    let io_class = {
      class_name = "IO";
      parent_class = Some "Object";
      attributes = [];
      methods = [
        {method_name = "out_string"; return_type = "SELF_TYPE"; params = [("x", "String")]};
        {method_name = "in_string"; return_type = "String"; params = []};
      ]
    } in
    StringMap.empty
    |> StringMap.add "Object" object_class
    |> StringMap.add "IO" io_class
  
  (* Adding classes, methods, and objects dynamically as you parse and type-check *)
  let new_class = {
    class_name = "Example";
    parent_class = Some "Object";
    attributes = [{attr_name = "attr1"; attr_type = "Int"}];
    methods = [{method_name = "example_method"; return_type = "Int"; params = [("x", "Int")]}];
  }
  
  let updated_class_env = add_class initial_class_env new_class
  
  (* In a method scope, we add objects to the obj_env *)
  let method_obj_env = add_object StringMap.empty "self" "Example"
  let method_obj_env = add_object method_obj_env "x" "Int"
  
  
  type cool_prog = cool_class list
  and loc = string
  and id = loc * string
  and cool_type = id
  and cool_class = id * (id option) * feature list
  and feature =
    | Attribute of id * cool_type * (exp option)
    | Method of id * (formal list) * cool_type * exp
  and formal = id * cool_type
  and exp = loc * exp_kind
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

let print_feature feature =
  match feature with
  | Attribute ((_, fname), (_, ftype), _) ->
      Printf.printf "    Attribute: %s of type %s\n" fname ftype
  | Method ((_, mname), formals, (_, mtype), _) ->
      Printf.printf "    Method: %s returning %s\n" mname mtype

let rec print_class ((loc, cname), parent_opt, features) =
  (* if inherits then run print_feature on inherits then continue with the regular print_feature  *)
  Printf.printf "Class (name: %s, location: %s)\n" cname loc;
  (match parent_opt with
  | Some (ploc, pname) -> Printf.printf "  Inherits from: %s\n" pname
  | None -> Printf.printf "  No Inheritance\n");
  List.iter print_feature features

let print_ast ast =
  Printf.printf "AST:\n";
  List.iter print_class ast

let base_classes = ["IO"; "Object"; "Int"; "Bool"; "String"]

let print_sorted_classes sorted_classes =
  List.iter (fun cname -> Printf.printf "%s\n" cname) sorted_classes

let build_graph ast = 
  let graph = Hashtbl.create 32 in 
  let feature_table = Hashtbl.create 16 in
  let add_edge parent child = 
    if Hashtbl.mem graph parent then 
      let children = Hashtbl.find graph parent in 
      Hashtbl.replace graph parent (child :: children)
    else
      Hashtbl.add graph parent [child]
  in 
  List.iter (fun ((cloc, cname), inherits, features) ->
    Hashtbl.add feature_table cname features;
    match inherits with 
    | Some (_iloc, iname) -> add_edge iname cname
    | None -> 
      Hashtbl.add graph cname []
  ) ast;
  (graph, feature_table)

let rec collect_feats feat_table graph cname =
  let cfeats = 
    try Hashtbl.find feat_table cname with Not_found -> []
  in
  let parent_class = 
    Hashtbl.fold (fun parent children acc -> 
      if List.mem cname children then Some parent else acc 
  ) graph None
  in
  match parent_class with 
  | Some parent ->
    let pfeats = collect_feats feat_table graph parent in 
    pfeats @ cfeats
  | None -> cfeats

let append_and_sort_classes sorted_class_names =
  let all_classes = List.fold_left (fun acc base_class ->
    if List.mem base_class sorted_class_names then acc else base_class :: acc
  ) sorted_class_names base_classes in
  List.sort String.compare all_classes

let rec dfs_cycle graph visited path curr =
  if List.mem curr path then
    let rec extract_cycle acc = function
      | [] -> acc
      | hd :: tl when hd = curr -> curr :: acc
      | hd :: tl -> extract_cycle (hd :: acc) tl
    in
    let cycle_path = extract_cycle [] path in
    printf "ERROR: 0: Type-Check: inheritance cycle: %s\n" (String.concat " " (List.rev cycle_path));
    exit 1
  else if not (Hashtbl.mem visited curr) then
    Hashtbl.add visited curr true;
    let path = curr :: path in
    let children = match Hashtbl.find_opt graph curr with
      | Some children -> children
      | None -> []
    in
    List.iter (fun child -> dfs_cycle graph visited path child) children;
    ()

let check_cycles graph =
  let visited = Hashtbl.create 10 in
  let path = [] in
  Hashtbl.iter (fun node _ ->
    if not (Hashtbl.mem visited node) then
      dfs_cycle graph visited path node
  ) graph

let rec extract_cycle acc curr = function
  | [] -> acc  
  | hd :: tl when hd = curr -> curr :: acc
  | hd :: tl -> extract_cycle (hd :: acc) curr tl

let topSort graph ast = 
  let visited = Hashtbl.create 10 in 
  let rec_stack = Hashtbl.create 10 in 
  let result = ref [] in
  let rec visit path node =
    if not (Hashtbl.mem visited node ) then (
      Hashtbl.add visited node true;
      Hashtbl.add rec_stack node true;

      let children = try Hashtbl.find graph node with Not_found -> [] in 
      List.iter (visit (node :: path)) children;

      Hashtbl.remove rec_stack node;
      result := node :: !result;
    )
  in
  let nodes = Hashtbl.fold (fun node _ acc -> node :: acc) graph [] in 
  let sorted_nodes = List.sort String.compare nodes in 
  List.iter (fun node -> if not (Hashtbl.mem visited node) then visit [] node) sorted_nodes;
  List.iter (fun node -> printf "%s -> " node) sorted_nodes;
  printf "ended \n";
  let topSorted = List.rev !result in
  let topSortedast = List.filter_map  (fun cname ->
    List.find_opt (fun ((_, cname'), _, _) -> cname = cname') ast
  ) topSorted in
  (topSortedast, topSorted)

let mod_ast graph feat_table ast = 
  (* Get topologically sorted AST *)
  let top_sorted_ast,sorted_classes = topSort graph ast in
  let new_ast = ref [] in 

  (* Modify each class in the topologically sorted order *)
  let mod_class ((loc, cname), inherits, _features) = 
    let all_feats = collect_feats feat_table graph cname in 
    let new_class = ((loc, cname), inherits, all_feats) in 
    new_ast := new_class :: !new_ast
  in 

  (* Directly iterate over the sorted AST entries and modify them *)
  List.iter mod_class top_sorted_ast;
  let all_sorted = append_and_sort_classes sorted_classes in

  (List.rev !new_ast, all_sorted)

let prep ast = 
  let (graph, feat_table) = build_graph ast in 
  check_cycles graph;
  let new_ast, classes = mod_ast graph feat_table ast in
  print_ast new_ast;
  printf "Top sort done\n";
  (new_ast, classes)

  let check_return_type ast all_classes =
    List.iter (fun ((cloc, cname), inherits, features) ->
      match List.rev features with
      | [] -> ()  (* No features in the class, nothing to check *)
      | last_feature :: _ ->
          let feature_type = match last_feature with
            | Attribute (_, (ftloc, ftype), _) -> ftype
            | Method (_, _, (mtloc, mtype), _) -> mtype
          in
          if not (List.mem feature_type all_classes) then (
            Printf.printf "ERROR: %s: Type-Check: Type %s in the last feature of class %s is not defined in all_classes\n" cloc feature_type cname;
            exit 1
          )
    ) ast
  
  let check_method_main ast =
    let main_class_found = ref false in
    let main_method_found = ref false in
  
    List.iter (fun ((cloc, cname), inherits, features) ->
      if cname = "Main" then (
        main_class_found := true;
        List.iter (fun feature ->
          match feature with
          | Method ((mloc, mname), formals, mtype, mbody) when mname = "main" -> 
              main_method_found := true;
  
              if List.length formals > 0 then (
                Printf.printf "ERROR: 0: Type-Check: class Main method main with 0 parameters not found\n";
                exit 1
              );
  
              let seen_formals = Hashtbl.create 32 in
              List.iter (fun ((floc, fname), ftype) ->
                if Hashtbl.mem seen_formals fname then (
                  Printf.printf "ERROR: %s: Type-Check: Duplicate formal parameter %s in method main\n" floc fname;
                  exit 1
                ) else
                  Hashtbl.add seen_formals fname floc
              ) formals
          | _ -> ()
        ) features
      )
    ) ast;
  
    if not !main_class_found then (
      Printf.printf "ERROR: 0: Type-Check: class Main not found\n";
      exit 1
    );
  
    if not !main_method_found then (
      Printf.printf "ERROR: 0: Type-Check: class Main method main not found\n";
      exit 1
    )
  
  
  let check_dup_param ast =
    List.iter (fun ((cloc, cname), inherits, features) ->
      List.iter (fun feature ->
          match feature with
          | Method ((mloc, mname), formals, mtype, mbody) ->
              let seen_formals = Hashtbl.create 32 in
              List.iter (fun ((floc, fname), ftype) ->
                  if Hashtbl.mem seen_formals fname then (
                    Printf.printf "ERROR: %s: Type-Check: class %s has method %s with duplicate formal parameter named %s\n" floc cname mname fname;
                    exit 1
                  ) else
                    Hashtbl.add seen_formals fname floc
                ) formals
          | _ -> ()
        ) features
    ) ast
  (*works for classes rn, maybe features*)
  let re_def ast =
    let seen_classes = Hashtbl.create 32 in
    List.iter (fun ((cloc, cname), inherits, features) ->
        match Hashtbl.find_opt seen_classes cname with
        | Some first_loc -> 
            Printf.printf "ERROR: %s: Type-Check: class %s redefined\n" cloc cname;
            exit 1
        | None ->
            Hashtbl.add seen_classes cname cloc
      ) ast
  
  let attr_name_self ast =
    List.iter (fun ((cloc, cname), inherits, features) ->
        List.iter (fun feature ->
            match feature with
            | Attribute ((floc, fname), ftype, _) ->
                if fname = "self" then (
                  printf "ERROR: %s: Type-Check: class %s has an attribute named self\n" floc cname;
                  exit 1
                )
            | Method _ -> ()
          ) features
      ) ast
  
  (*redefine attr needs to be fixed more robust*)
let re_def_feat ast = 
  List.iter (fun ((cloc, cname), inherits, features) ->
      let seen_attributes = Hashtbl.create 32 in
      List.iter (fun feature ->
          match feature with
          | Attribute ((floc, fname), ftype, _) ->
              begin
                match Hashtbl.find_opt seen_attributes fname with
                | Some first_loc -> 
                    Printf.printf "ERROR: %s: Type-Check: Redefining attribute %s in class %s\n" floc fname cname;
                    exit 1
                | None ->
                    Hashtbl.add seen_attributes fname floc
              end
          | Method _ -> ()
        ) features
    ) ast

let re_def_attr ast = 
  List.iter (fun ((cloc, cname), inherits, features) ->
    let seen_attributes = Hashtbl.create 32 in
    List.iter (fun feature ->
        match feature with
        | Attribute ((floc, fname), ftype, _) ->
            begin
              match Hashtbl.find_opt seen_attributes fname with
              | Some first_loc -> 
                  Printf.printf "ERROR: %s: Type-Check: Duplicate attribute %s in class %s\n" floc fname cname;
                  exit 1
              | None ->
                  Hashtbl.add seen_attributes fname floc
            end
        | Method _ -> ()
      ) features
  ) ast

let rec print_id (loc, name) =
  Printf.printf "ID (location: %s, name: %s)\n" loc name

let print_cool_type (loc, tname) =
  Printf.printf "Cool_Type (location: %s, type: %s)\n" loc tname

let arth_error (ival, xval) =
  let iloc = fst ival in
  let itype = match snd ival with
    | Integer _ -> "Int"
    | Bool _ -> "Bool"
    | String _ -> "String"
    | _ -> "Object"
  in
  let xtype = match snd xval with
    | Integer _ -> "Int"
    | Bool _ -> "Bool"
    | String _ -> "String"
    | _ -> "Object"
  in
    Printf.printf "ERROR: %s: Type-Check: arithmetic on %s %s instead of Ints\n" iloc itype xtype;
    exit 1

let bool_error (ival, xval) =
  let iloc = fst ival in
  let itype = match snd ival with
    | Integer _ -> "Int"
    | Bool _ -> "Bool"
    | String _ -> "String"
    | _ -> "Object"
  in
  let xtype = match snd xval with
    | Integer _ -> "Int"
    | Bool _ -> "Bool"
    | String _ -> "String"
    | _ -> "Object"
  in
  if itype = "Object" && xtype = "Object" then 
    ()
  else
    Printf.printf "ERROR: %s: Type-Check: comparison between %s and %s\n" iloc itype xtype;
    exit 1

let rec print_exp (loc, exp_kind) =
  Printf.printf "Expression (location: %s)\n" loc;
  match exp_kind with
  | Not value ->
    Printf.printf "  Not: \n";
    print_exp value
  | Integer value -> Printf.printf "  Integer: %s\n" value
  | Bool value -> 
    Printf.printf "  Bool: %s\n" value
  | String value -> Printf.printf "  String: %s\n" value
  | Plus((loc1, t1), (loc2, t2)) ->
    Printf.printf "plus\n";
    print_exp (loc1, t1);
    print_exp (loc2, t2)
  | Minus((loc1, t1), (loc2, t2)) ->
    Printf.printf "minus\n";
    print_exp (loc1, t1);
    print_exp (loc2, t2)
  | Times((loc1, t1), (loc2, t2)) ->
    Printf.printf "times\n";
    print_exp (loc1, t1);
    print_exp (loc2, t2)
  | Divide((loc1, t1), (loc2, t2)) ->
    Printf.printf "divide\n";
    print_exp (loc1, t1);
    print_exp (loc2, t2)
  | Block ival ->
      printf "block\n";
      List.iter print_exp ival;
  | Assign (var,rhs_exp) ->
    print_id var;
    print_exp rhs_exp;
  | Isvoid void -> 
    print_exp void;
  | If (if_exp,then_exp,else_exp) ->
    print_exp if_exp;
    print_exp then_exp;
    print_exp else_exp;
  | While (loop,pool) ->
    print_exp loop; 
    print_exp pool;
  | Let(bindings, let_body) ->
    printf "let\n";
    List.iter (fun ((vloc,vname), (typeloc,typename), init_exps) ->
      printf " Bindings: %s: %s \n" vname typename;
      (match init_exps with
      | None -> printf " No init "
      | Some init_exp ->
        printf " Init:\n";
        print_exp init_exp
          )
      ) bindings;
      printf "in\n";
      print_exp let_body
  | Identifier ival ->
    print_id ival
  | Case (test_exp, case_list ) -> 
    printf "First exp: ";
    print_exp test_exp;
    printf "\n";
    List.iter (fun ((vloc,vname), (tloc, tname), rest_exp ) -> 
      printf "  Case : %s : %s \n" vname tname;
      printf "  Case_exp:\n";
      print_exp rest_exp
      ) case_list
  | New ((loc_ival, ival_name)) ->
      Printf.printf "new\n";
      Printf.printf "  New Object: %s\n" ival_name
  | Dynamic_Dispatch (e,metho,args) ->
    printf "Dynamic Dispatch:\n";
    printf "  Exp : ";
    print_exp e;
    printf "\n";
    printf "  Id : ";
    print_id metho;
    printf "\n";
    printf "  Args :\n";
    List.iter print_exp args
  | Static_Dispatch (e,stat_type,metho,args) ->
    printf "Static Dispatch:\n";
    printf "  Exp : ";
    print_exp e;
    printf "\n";
    printf "  Type : ";
    print_cool_type stat_type;
    printf "  Id : ";
    print_id metho;
    printf "\n";
    printf "  Args :\n";
    List.iter print_exp args
  | Self_Dispatch (metho,args) ->
    printf "Self Dispatch:\n";
    printf "  Id : ";
    print_id metho;
    printf "\n";
    printf "  Args :\n";
    List.iter print_exp args
  | LT ((loc1, t1), (loc2, t2)) ->
    Printf.printf "LT\n";
    print_exp (loc1, t1);
    print_exp (loc2, t2)
  | LE ((loc1, t1), (loc2, t2)) ->
    Printf.printf "LE\n";
    print_exp (loc1, t1);
    print_exp (loc2, t2)
  | EQ ((loc1, t1), (loc2, t2)) ->
      Printf.printf "EQ\n";
      print_exp (loc1, t1);
      print_exp (loc2, t2)
  | Negate ((loc1, t1)) ->
    Printf.printf "Negate\n";
    print_exp (loc1, t1)

let print_formal ((loc, fname), (ftloc, ftype)) =
  Printf.printf "Formal (name: %s, type: %s)\n" fname ftype

let rec print_feature feature =
  match feature with
  | Attribute (id, cool_type, exp_opt) ->
      print_id id;
      print_cool_type cool_type;
      (match exp_opt with
      | Some exp -> print_exp exp
      | None -> Printf.printf "  No Initialization Expression\n")
  | Method (id, formals, cool_type, body) ->
      print_id id;
      List.iter print_formal formals;
      print_cool_type cool_type;
      print_exp body

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
              print_id cname;
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
              (
                match snd ival with
                | (Integer _) ->
                  Negate(ival)
                | _ ->
                  Negate(ival)
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
                  (* bool_error *)
                  LT(ival, xval)  )
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
                  (* bool_error *)
                  LE(ival, xval)  )
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
                  (* bool_error *)
                  EQ(ival, xval)  )
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
                      | _ -> (let_var, let_type, None) (* failwith "binding failed, invalid let" *)
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
            (eloc, ekind)
            in
            let ast = read_cool_program () in
            close_in fin ;
            (* printf "CL-AST de-serizlized, %d classes\n" (List.length ast); *)
            let base_classes = ["Int"; "String"; "Bool"; "IO"; "Object" ] in
            let user_classes = List.map (fun ((_, cname),_,_) -> cname) ast in
            let all_classes = base_classes @ user_classes in
            (* this is for redefining a class name*)
            re_def ast;
            re_def_feat ast;
            attr_name_self ast;
            re_def ast;
            check_method_main ast;
            check_return_type ast all_classes;
            check_dup_param ast;
            (* THEME IN PA4 -- you should make internal data structures to hold helper information so that you can do the checks more easily *)
            (*Look for Inheritance from Int
            Look for Inheritance from Undeclared class *)
            List.iter (fun ((cloc, cname), inherits, features) ->
              match  cname with
              | "String" | "Int" | "Bool" | "IO" -> 
                printf "ERROR: %s: Type-Check: cannot redefine the class %s\n" cloc cname;
              | x -> ();
              match inherits with
              | None -> ()
              | Some (iloc, iname) ->
              if iname = "Int" || iname = "Bool" || iname = "String" || iname = "SELF_TYPE" then begin
                printf "ERROR: %s: Type-Check: class %s inherits from %s\n" iloc cname iname;
              exit 1
              end ;
              if not (List.mem iname all_classes) then begin
                printf "ERROR: %s: Type-Check: inheriting from undefined class %s\n" iloc iname;
                exit 1
              end ;
            ) ast;
            
            (* Type-check is supposed to be here*)
            (* DONE WITH ERROR CHECKING *)
            (* Now we emit the CL-TYPE File *)
            (* For PA4_C_ -- we just do the class map *)
            let cname = (Filename.chop_extension fname) ^ ".cl-test" in 
            let fout = open_out cname in
            let output_id bruh_val = 
              fprintf fout "%s\n%s\n" (fst bruh_val) (snd bruh_val) 
            in
            let rec output_exp (eloc, ekind) =
              fprintf fout "%s\n" eloc ;
              match ekind with
              | Integer(ival) -> fprintf fout "integer\n%s\n" ival
              | String(ival) -> fprintf fout "string\n%s\n" ival
              | Bool(ival) ->  (
                match ival with 
                | "true" -> 
                  fprintf fout "bool\ntrue\n"
                | "false" -> 
                  fprintf fout "bool\nfalse\n"
                | _ ->  fprintf fout "")
              | Plus(ival, xval) ->
                fprintf fout "plus\n"; output_exp(ival); output_exp(xval)
              | Times(ival, xval) ->
                fprintf fout "times\n"; output_exp(ival); output_exp(xval)
              | Divide(ival, xval) ->
                fprintf fout "divide\n"; output_exp(ival); output_exp(xval)
              | Minus(ival, xval) ->
                fprintf fout "minus\n"; output_exp(ival); output_exp(xval)
              | Let(bindings, let_body) ->
                fprintf fout "let\n";
                List.iter (fun ((vloc, vname), (typeloc, typename), bExp) ->
                  fprintf fout "%s %s\n" vname typename;
                  (match bExp with
                  | None -> fprintf fout "no_initializer\n"
                  | Some init_exp -> output_exp init_exp
                  )
                ) bindings;
                fprintf fout "in\n";
                output_exp let_body
              | Block(expr_list) ->
                fprintf fout "block\n";
                fprintf fout "%d\n" (List.length expr_list); 
                List.iter output_exp expr_list
              | Case(test_exp, case_list) ->
                fprintf fout "case\n";
                output_exp test_exp;
                fprintf fout "%d\n" (List.length case_list);  
                List.iter (fun ((vloc, vname), (typeloc, typename), case_body) ->
                  fprintf fout "%s\n%s\n%s\n%s\n" vloc vname typeloc typename;
                  output_exp case_body
                ) case_list
              | Identifier(ival) ->
                fprintf fout "identifier\n";
                output_id ival
              | New(ival) ->
                fprintf fout "new\n";
                output_id ival;
              | If (if_exp,then_exp,else_exp) ->
                fprintf fout "if\n";
                output_exp if_exp;
                output_exp then_exp;
                output_exp else_exp;
              | While (loop,pool) ->
                fprintf fout "while\n";
                output_exp loop;
                output_exp pool;
              | Assign (var,rhs_exp) ->
                fprintf fout "assign\n";
                output_id var;
                output_exp rhs_exp;
              | Isvoid (void) ->
                fprintf fout "isvoid\n";
                output_exp void;
              | Dynamic_Dispatch(e,metho,args) -> 
                fprintf fout "dynamic_dispatch\n";
                output_exp e;
                output_id metho;
                fprintf fout "%d\n" (List.length args) ;
                List.iter output_exp args;
              (* make sure to print LIST ARGS for all of them caused a big problem for me*)
              | Static_Dispatch(e,ftype,metho,args) -> 
                fprintf fout "static_dispatch\n";
                output_exp e;
                output_id ftype;
                output_id metho;
                fprintf fout "%d\n" (List.length args) ;
                List.iter output_exp args;
              | Self_Dispatch(metho,args) -> 
                fprintf fout "self_dispatch\n";
                output_id metho;
                fprintf fout "%d\n" (List.length args) ;
                List.iter output_exp args;
              | LT(ival,xval) -> 
                fprintf fout "lt\n"; output_exp(ival); output_exp(xval)
              | LE(ival,xval) -> 
                fprintf fout "le\n"; output_exp(ival); output_exp(xval)
              | EQ(ival,xval) -> 
                fprintf fout "eq\n"; output_exp(ival); output_exp(xval)
              | Negate(ival) -> 
                fprintf fout "negate\n"; output_exp(ival)
              | Not(ival) -> 
                fprintf fout "not\n"; output_exp(ival)
              in
            print_ast ast;
            printf "entering the topo\n";
            let final_ast,sorted_classes = prep ast in
            print_ast final_ast;
            print_sorted_classes sorted_classes;
            (* let updated_classes = propagate_features ast sorted_classes in
            let last_ast = List.sort (fun ((_, cname1), _, _) ((_, cname2), _, _) ->
              compare cname1 cname2
            ) updated_classes in
            let last_classes = List.sort compare sorted_classes in
            print_ast last_ast;
            print_sorted_classes last_classes; *)
            printf "user_classes\n";
            print_sorted_classes user_classes;
            fprintf fout "class_map\n%d\n" (List.length all_classes) ;
            List.iter (fun cname ->
            (* name of class, # attrs, each attr=feature in turn *)
            fprintf fout "%s\n" cname;
            let attributes =
              (*
              (1) construct a mapping from child to parent
                (1) use topsort to find the right order of traversal
                  ( or to detect inheritance cycles )
                (2) recurly walk up that mapping until we hit object
                (3) add in all of the attr we find
                (4) while there -- look for all the attr override problems
                *)
            try
              let _, inherits, features = List.find (fun ((_, cname2),__,_) -> cname = cname2) final_ast in
              List.filter (fun feature -> match feature with
              | Attribute _ -> true
              | Method _ -> false
              ) features
              with Not_found ->(*bool/int/object *)
                []
            in
            
            fprintf fout "%d\n" (List.length attributes);
            (* fprintf fout "%d\n" (List.length attributes); *)
            if not (List.mem "Main" all_classes) then begin
              printf "ERROR: 0: Type-Check: class Main not found\n";
              exit 1;
            end;
            List.iter (fun attr -> match attr with
              | Attribute((_,aname),(_,atype),None) ->
              fprintf fout "no_initializer\n%s\n%s\n" aname atype
              | Attribute((_,aname),(_,atype),(Some init)) ->
              fprintf fout "initializer\n%s\n%s\n" aname atype ;
              output_exp init
              | Method _ -> failwith "method unexpected"
              ) attributes;
            ) sorted_classes; (* to do need to sort here *)
      close_out fout;
  end ;;
  main () ;;