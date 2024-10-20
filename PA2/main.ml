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

let rec is_sub inherit_table t1 t2 = 
  let class1 = 
    match t1 with 
      | Class(x) -> (Class x)
      | SELF_TYPE(x) -> (Class x)
    in
  let class2 = 
    match t2 with
    | Class(x) -> (Class x)
    | SELF_TYPE(x) -> (Class x)
  in
  match class1,class2 with 
  | Class(x), Class(y) when x = y -> true
  | Class(x), Class(y) when x <> y ->
    (* recursive calls *)
    let parent1 = Hashtbl.find inherit_table x in
    is_sub inherit_table (Class parent1) (Class y)
  | Class(x), Class("Object") -> true 
  | Class("Object"), Class(y) when x = y -> true
  | Class(x), Class(y) -> false (* treat later, check parent map *)
  | _, _ -> false (*check the class notes*)

(* so I don't have to write this 400 times*)
let type_to_norm t =
  match t with 
    | SELF_TYPE(c) | Class(c) -> (Class c)

let find_parent inherit_table cname = 
  if Hashtbl.mem inherit_table cname then 
    Hashtbl.find inherit_table cname
  else (
    Printf.printf "Missed adding class %s\n" cname;
    exit 1 )

let rec lub inherit_table t1 t2 = 
  let class1 = type_to_norm t1 in 
  let class2 = type_to_norm t2 in
  if is_sub inherit_table t1 t2 then 
    class2
  else  (
    let parent2 = find_parent inherit_table (type_to_str class2) in 
    lub inherit_table class1 (Class parent2)
  );

type obj_env = (static_type * string, static_type) Hashtbl.t (* use the first static_type to keep track of this stuff, fuck the scopes for now*)
type metho_env = (static_type *string, (static_type list) * string ) Hashtbl.t (* used to keep track of classes with method names and their formallist typees with return types*)
type local_env = (string * string *string, bool) Hashtbl.t (* used to keep track of classes and their feature name*)
let empty_env () = Hashtbl.create 255
(*tom you need to have this for the correct sub type thing and easier to do with the parent map*)
type inherit_table = (string,string) Hashtbl.t

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

let extract_class_name ((_, cname), _, _) = cname

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
let print_sorted_classes sorted_classes =
  List.iter (fun cname -> Printf.printf "%s\n" cname) sorted_classes

let add_base_classes map =
  let base_classes = [
    (("0", "Object"), None, []);
    (("0", "Int"), None, []);
    (("0", "String"), None, []);
    (("0", "Bool"), None, []);
    (("0", "IO"), None, [])
  ] in
  List.fold_left (fun acc_map ((loc, cname), parent_opt, features) ->
    StringMap.add cname ((loc, cname), parent_opt, features) acc_map
  ) map base_classes

let build_class_map ast =
  let initial_map = StringMap.empty in
  let map_with_bases = add_base_classes initial_map in
  List.fold_left (fun map ((loc, cname), parent_opt, features) ->
    (* Printf.printf "Adding class: %s\n" cname; *)
    StringMap.add cname ((loc, cname), parent_opt, features) map
  ) map_with_bases ast

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
            print_id metho;
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
          print_ast ast;
          printf "entering the topo\n";
          let last_ast,sorted_classes = prep ast in
          print_ast last_ast;
          print_sorted_classes sorted_classes;
          let obj_global: obj_env = empty_env () in
          let metho_global: metho_env = empty_env () in
          let class_local: local_env = empty_env () in
          let inherit_tracker: inherit_table = empty_env () in
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
          let rec get_formal_types form_list class_name metho_name =
            match form_list with 
            | [] -> []
            | ((funcloc,funcname),(functypeloc,functypename)) :: tail ->
              if funcname = "self" then (
                printf "ERROR: %s: Type-Check: class %s has method %s with formal parameter name self\n" funcloc class_name metho_name;
                exit 1
              );
              if functypename = "SELF_TYPE" then (
                printf "ERROR: %s: Type-Check: class %s has method %s with formal parameter of unknown type SELF_TYPE\n" funcloc class_name metho_name;
                exit 1
              );
              (Class functypename) :: get_formal_types tail class_name metho_name
          in
          let add_class cname = 
            let _,_, feats = List.find ( fun ((_,cnamebruh),_,_) -> cnamebruh = cname) last_ast in
            List.iter (fun feat ->
                        match feat with
                        | Attribute ((attr_loc, attr_name),(_,attr_type),_) -> 
                          Hashtbl.add class_local (cname, "attr", attr_name) true;

                          if attr_type = "SELF_TYPE" then 
                            Hashtbl.add obj_global ((Class cname),attr_name) (SELF_TYPE cname)
                          else
                            Hashtbl.add obj_global ((Class cname), attr_name) (Class attr_type)
                        | Method ((metho_loc,metho_name), forms, (metho_type_loc,metho_type), _) ->
                          Hashtbl.add class_local (cname, "meth", metho_name) true;
                          let form_type_list = get_formal_types forms cname metho_name in 
                          Hashtbl.add metho_global ((Class cname), metho_name) (form_type_list, metho_type)
                          ) feats;
            Hashtbl.add obj_global ((Class cname),"self") (SELF_TYPE cname)
          in
          (*adding the class content*)
          List.iter( fun ((cloc,cname),_,_)-> add_class cname) last_ast;
          let rec typecheck (o: obj_env) (m: metho_env) (curr_class: static_type) (exp: exp) : static_type = 
            let static_type = match exp.exp_kind with
            (*handled the easy ones first *)
            | Bool(x) ->
              (match x with
              | "true" -> (Class "Bool")
              | "false" -> (Class "Bool")
              | _ -> failwith "yeah bool is doing some wrong")
            | Integer(i) -> (Class "Int")
            | String(i) -> (Class "String")
            (*easy ones handled*)
            (*arth ones handled*)
            | Plus(e1,e2) -> 
              (*
                O |- e1: int  [1]
                O |- e2: int  [2]
                -----------
                O |- e1 + e2 : Int  [3]
              Recal |- do typechecklet static_type = 
              *)
              (*[1]*)
              let t1 = typecheck o m curr_class e1 in 
              if t1 <> (Class "Int") then begin
                printf "ERROR: %s: Type-Check: arithmetic on %s instead of Ints\n" exp.loc (type_to_str t1);
                exit 1;
              end;
              (* [2] *)
              let t2 = typecheck o m curr_class e2 in 
              if t2 <> (Class "Int") then begin
                printf "ERROR: %s: Type-Check: arithmetic on %s instead of Ints\n" exp.loc (type_to_str t2);
                exit 1
              end;
              (* [3] *)
              (Class "Int")
            | Minus(e1,e2) ->
              (*Yeah just copy and past this stuff from plus for arth*)
              let t1 = typecheck o m curr_class e1 in 
              if t1 <> (Class "Int") then begin
                printf "ERROR: %s: Type-Check: arithmetic on %s instead of Ints\n" exp.loc (type_to_str t1);
                exit 1;
              end;
              let t2 = typecheck o m curr_class e2 in 
              if t2 <> (Class "Int") then begin
                printf "ERROR: %s: Type-Check: arithmetic on %s instead of Ints\n" exp.loc (type_to_str t2);
                exit 1
              end;
              (Class "Int")
            | Times(e1,e2) ->
              let t1 = typecheck o m curr_class e1 in 
              if t1 <> (Class "Int") then begin
                printf "ERROR: %s: Type-Check: arithmetic on %s instead of Ints\n" exp.loc (type_to_str t1);
                exit 1;
              end;
              let t2 = typecheck o m curr_class e2 in 
              if t2 <> (Class "Int") then begin
                printf "ERROR: %s: Type-Check: arithmetic on %s instead of Ints\n" exp.loc (type_to_str t2);
                exit 1
              end;
              (Class "Int")
            | Divide(e1,e2) ->
              let t1 = typecheck o m curr_class e1 in 
              if t1 <> (Class "Int") then begin
                printf "ERROR: %s: Type-Check: arithmetic on %s instead of Ints\n" exp.loc (type_to_str t1);
                exit 1;
              end;
              let t2 = typecheck o m curr_class e2 in 
              if t2 <> (Class "Int") then begin
                printf "ERROR: %s: Type-Check: arithmetic on %s instead of Ints\n" exp.loc (type_to_str t2);
                exit 1
              end;
              (Class "Int")
            | EQ(e1,e2) ->
              let t1 = typecheck o m curr_class e1 in 
              let t2 = typecheck o m curr_class e2 in 
              if t1 <> t2 then begin
                printf "ERROR: %s: Type-Check: comparison between %s and %s\n" exp.loc (type_to_str t1) (type_to_str t2);
                exit 1
              end;
              (Class "Bool")
            | LE(e1,e2) ->
              let t1 = typecheck o m curr_class e1 in 
              if (type_to_norm t1) <> (Class "Int") then begin
                printf "ERROR: %s: Type-Check: compare %s instead of Int\n" exp.loc (type_to_str t1);
                exit 1;
              end;
              (* [2] *)
              let t2 = typecheck o m curr_class e2 in 
              if (type_to_norm t2) <> (Class "Int") then begin
                printf "ERROR: %s: Type-Check: compare %s instead of Int\n" exp.loc (type_to_str t2);
                exit 1
              end;
              (Class "Int")
            | LT(e1,e2) -> 
              let t1 = typecheck o m curr_class e1 in
              if (type_to_norm t1) <> (Class "Int") then begin
                printf "ERROR: %s: Type-Check: compare %s instead of Int\n" exp.loc (type_to_str t1);
                exit 1;
              end;
              (* [2] *)
              let t2 = typecheck o m curr_class e2 in 
              if (type_to_norm t2) <> (Class "Int") then begin
                printf "ERROR: %s: Type-Check: compare %s instead of Int\n" exp.loc (type_to_str t2);
                exit 1
              end;
              (Class "Int")
            | Isvoid(e) ->
              typecheck o m curr_class e; (*just to make sure no problems arise*)
              (Class "Bool")
            | Negate(e) ->
              let t1 = typecheck o m curr_class e in 
              if t1 <> (Class "Int") then (
                printf "ERROR: %s: Type-Check: negate %s instead of Int\n" exp.loc (type_to_str t1);
                exit 1
              );
              (Class "Int")
            | Not(e) ->
              let t1 = typecheck o m curr_class e in 
              if t1 <> (Class "Bool") then begin
                printf "ERROR: %s: Type-Check: not %s instead of Bool\n" exp.loc (type_to_str t1);
                exit 1
              end;
              (Class "Bool")
            | Identifier((vloc,vname)) -> 
              let normed = type_to_norm curr_class in
              if Hashtbl.mem o (normed, vname) then (*bool check in ocaml of finding the value*)
                Hashtbl.find o (normed, vname)
              else begin
                printf "ERROR: %s: Type-Check: unbound identifier %s\n" vloc vname;
                exit 1
              end;
            | Assign(id1,e1) ->
              if (snd id1) = "self" then (
                printf "ERROR: %s: Type-Check: cannot assign to %s\n" (fst id1) (snd id1);
                exit 1
              );
              let normed = type_to_norm curr_class in
              let t1 = 
                if Hashtbl.mem o (normed, (snd id1)) then
                    Hashtbl.find o (normed, (snd id1))
                else (
                  printf "ERROR: %s: Type-Check: unbound indentifier %s\n" (fst id1) (snd id1);
                  exit 1
                )
              in
              let t2 = typecheck o m curr_class e1 in 
              (*todo fix crawling up the tree for inherits*)
              if is_sub t2 t1 then
                t2
              else (
                printf "ERROR: %s: Type-Check: %s does not conform to %s in assignment\n" e1.loc (type_to_str t2) (type_to_str t1);
                exit 1
              )
            | New((loc, name)) ->
              if name = "SELF_TYPE" then 
                (SELF_TYPE (type_to_str (type_to_norm curr_class)))
              else
                (Class name)
            | While(loop,pool) ->
              let type_loop = typecheck o m curr_class loop in 
              (* just in case the body is wrong does not need to be used*)
              let type_pool = typecheck o m curr_class loop in 
              if type_loop <> (Class "Bool") then (
                printf "ERROR: %s: Type-Check: predicate has tpye %s not Bool\n" exp.loc (type_to_str type_loop);
                exit 1
              );
              (Class "Object")
            | Block(exp_list) -> 
              let exps_types = List.map (fun exp -> typecheck o m curr_class exp ) exp_list in
              List.hd (List.rev exps_types) (* take the last of all exps similar to ocaml*)
            | If(if_state,then_state,else_state) ->
              let if_type = typecheck o m curr_class if_state in 
              if if_type <> (Class "Bool") then begin 
                printf "ERROR: %s: Type-Check: conditional has type %s instead of Bool\n" exp.loc (type_to_str if_type);
                exit 1;
              end;
              let then_state = typecheck o m curr_class then_state in 
              let else_state = typecheck o m curr_class else_state in
              (*todo lub of the two types*)
              (Class "LUB")
            | Let(binding_list,let_body) -> 
              (match binding_list with 
              | [] -> 
                typecheck o let_body;
              | ((vloc, vname), (typeloc,typename), let_exp) :: tail -> (*need the tail otherwise it'll crash since let_body is a list you need to indivual
                typecheck each expression *)
                typecheck o let_body)
            | _ -> (Class "Int")
            in
            exp.static_type <- Some(static_type);
            static_type
          in
          (*type check time*)
          List.iter (fun ((cloc,cname), inherits, feats) ->
            List.iter (fun feat ->
              match feat with
                | Attribute((nameloc,name),(dtloc,declared_type),Some(init_exp)) -> (* x: int <- 5 + 3*)
                  let init_type = typecheck obj_global init_exp in
                  printf "%s init\n" (type_to_str init_type);
                  if is_sub init_type (Class declared_type) then
                    ()
                  else begin
                  printf "ERROR: %s: Type-Check: init for %s was %s not %s\n"
                  nameloc name (type_to_str init_type) declared_type;
                  exit 1
                  end;
                | _ -> () (*fix me*)
                ) feats;
          ) ast;
          (* Type-check is supposed to be here*)
          (* DONE WITH ERROR CHECKING *)
          (* Now we emit the CL-TYPE File *)
          (* For PA4_C_ -- we just do the class map *)
          let cname = (Filename.chop_extension fname) ^ ".cl-test" in 
          let fout = open_out cname in
          let rec output_exp e = 
            (* output the type for class map*)
            fprintf fout "%s\n" e.loc ;
            (match e.static_type with 
            | None -> failwith "forgot to type with to typecheck";
            | Some(Class(c)) -> fprintf fout "%s\n" c;
            | Some(SELF_TYPE(c)) -> failwith "SLEF_TYPE not fixed"
            );
            let output_id bruh_val = 
              fprintf fout "%s\n%s\n" (fst bruh_val) (snd bruh_val) 
            in
            match e.exp_kind with
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
          (* printf "entering the topo\n"; *)
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
            let _, inherits, features = List.find (fun ((_, cname2),__,_) -> cname = cname2) last_ast in
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