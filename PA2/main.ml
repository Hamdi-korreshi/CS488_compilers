(* Hamdi Korreshi and Tomasz Brauntsch
  PA2 Semantic Analyzer Checkpoint *)
module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

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
and exp_kind =
  | Integer of string (* doesn't need to be an Int until the next PA *)
  | Bool of string
  | String of string
  | Plus of exp * exp
  | Minus of exp * exp
  | Times of exp * exp
  | Divide of exp * exp
open Printf

type graph = {
  deps_map : string list StringMap.t;
  in_deg : int StringMap.t;
}

type 'a func_queue = { front: 'a list; back: 'a list}

let empty_q = {
  front = [];
  back = [];
}

let append value que = {
  que with back = value :: que.back
}

let check_empty = function
  | { front = []; back = []} -> true
  | _ -> false

let deque = function 
  | { front = []; back = []} -> None
  | { front = x :: xs; back } -> Some (x, { front = xs; back = back })
  | { front = []; back } ->
      let front_rev = List.rev back in
      Some (List.hd front_rev, { front = List.tl front_rev; back = [] })

let sort_queue que =
  let combined = que.front @ List.rev que.back in
  let sorted = List.sort compare combined in
  { front = sorted; back = [] }

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

let print_maps graph =
  (* Printf.printf "Printing deps_map:\n"; *)
  StringMap.iter (fun key value_list ->
    let deps = String.concat ", " value_list in
    Printf.printf "Class: %s -> Depends on: [%s]\n" key deps
  ) graph.deps_map;

  Printf.printf "Printing in_deg map:\n";
  StringMap.iter (fun key value ->
    Printf.printf "Class: %s -> In-degree: %d\n" key value
  ) graph.in_deg

let find_zeros min_heap map =
  (* Printf.printf "Starting fold over the map\n"; *)
  StringMap.fold (fun key value min_heap ->
    (* Printf.printf "Processing key: %s with value: %d\n" key value; *)
    if value = 0 then (
      (* Printf.printf "Appending key: %s to the min heap\n" key; *)
      append key min_heap
    ) else (
      (* Printf.printf "Skipping key: %s, value is non-zero\n" key; *)
      min_heap
    )
  ) map min_heap

let child_loop map child min_heap =
  (* Printf.printf "Starting child_loop with child list: [%s]\n" (String.concat ", " child); *)
  List.fold_left (fun (map, min_heap) node ->
    (* Printf.printf "Processing node: %s\n" node; *)
    let new_deg = StringMap.find node map.in_deg in
    (* Printf.printf "Current in-degree of node %s: %d\n" node new_deg; *)
    
    let updated_deg = StringMap.add node (new_deg - 1) map.in_deg in
    (* Printf.printf "Updated in-degree of node %s: %d\n" node (new_deg - 1); *)
    
    let updated_map = { map with in_deg = updated_deg } in
    let min_heap = if StringMap.find node updated_map.in_deg = 0 then (
      (* Printf.printf "Node %s has 0 in-degree now, adding to min_heap\n" node; *)
      append node min_heap
    ) else (
      (* Printf.printf "Node %s still has non-zero in-degree, skipping\n" node; *)
      min_heap
    ) in
    updated_map, min_heap
  ) (map, min_heap) child
  
let rec while_loop (map : graph) final min_heap count =
  Printf.printf "Entered while_loop with count: %d\n" count;
  if not (check_empty min_heap) then (
    Printf.printf "Min heap is not empty, proceeding...\n";
    let sorted = sort_queue min_heap in
    Printf.printf "Sorted min heap: ";
    List.iter (Printf.printf "%s ") sorted.front;
    Printf.printf "\n";

    let curr, min_heap = match deque sorted with
      | Some (curr, new_heap) ->
          Printf.printf "Dequeued: %s\n" curr;
          curr, new_heap
      | None -> failwith "Queue is empty"
    in

    let final = final @ [curr] in
    Printf.printf "Updated final list: [%s]\n" (String.concat ", " final);

    printf "curr: %s\n" curr;
    (* print_maps map; *)
    let value = StringMap.find curr map.deps_map in
    Printf.printf "Dependencies of %s: [%s]\n" curr (String.concat ", " value);

    let map, min_heap = child_loop map value min_heap in
    Printf.printf "Finished child_loop for %s\n" curr;
    while_loop map final min_heap (count + 1)
  ) else if count < StringMap.cardinal map.in_deg then (
    Printf.printf "Cycle detected in inheritance graph. Processed count: %d, Total classes: %d\n"
      count (StringMap.cardinal map.in_deg);
    exit 1
  ) else (
    Printf.printf "Topological sort completed successfully.\n";
    final
  )

let make_graph ast =
  let base_classes = [
    ("0", "Object");
    ("0", "IO");
    ("0", "Int");
    ("0", "Bool");
    ("0", "String")
  ] in

  (* Add base classes to the deps_map *)
  let deps_map = List.fold_left (fun acc (_, cname) ->
    StringMap.add cname [] acc
  ) StringMap.empty base_classes in

  (* Add user-defined classes to deps_map *)
  let deps_map = List.fold_left (fun acc ((_, cname), inherits, _) ->
    match inherits with
    | Some (_, pname) -> 
        let deps = match StringMap.find_opt cname acc with
          | Some existing -> pname :: existing
          | None -> [pname]
        in
        StringMap.add cname deps acc
    | None -> 
        StringMap.add cname [] acc (* Add class with no dependencies *)
  ) deps_map ast in

  (* Combine base classes and user-defined classes *)
  let all_classes = List.map (fun (_, cname) -> cname) base_classes @ 
                    List.map (fun ((_, cname), _, _) -> cname) ast in

  (* Calculate the in-degree map *)
  let in_deg = List.fold_left (fun acc cname ->
    let count = List.fold_left (fun count ((_, child_name), inherits, _) ->
      match inherits with
      | Some (_, parent_name) -> if parent_name = cname then count + 1 else count
      | None -> count
    ) 0 ast in
    StringMap.add cname count acc
  ) StringMap.empty all_classes in

  { deps_map; in_deg }

let topSort ast =
  (* printf "making graph\n"; *)
  let graph = make_graph ast in
  let min_heap = find_zeros empty_q graph.in_deg in
  (* printf "looping\n"; *)
  let sorted_classes = while_loop graph [] min_heap 0 in
  sorted_classes

let print_sorted_classes sorted_classes =
  List.iter (fun cname -> Printf.printf "%s\n" cname) sorted_classes

  let merge_features parent_features child_features =
    let rec merge acc = function
      | [] -> acc
      | Attribute (id, typ, init) :: rest ->
          if List.exists (function Attribute (id2, _, _) -> id = id2 | _ -> false) child_features then
            merge acc rest
          else
            merge (Attribute (id, typ, init) :: acc) rest
      | Method (id, formals, ret_typ, body) :: rest ->
          if List.exists (function Method (id2, _, _, _) -> id = id2 | _ -> false) child_features then
            merge acc rest
          else
            merge (Method (id, formals, ret_typ, body) :: acc) rest
    in
    merge child_features parent_features

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

let propagate_features ast sorted_classes =
  let class_map = build_class_map ast in
  List.map (fun cname ->
    let ((loc, cname), inherits, features) = StringMap.find cname class_map in
    match inherits with
    | Some (iloc, pname) -> (* Inherits from another class *)
        let (_, _, parent_features) = StringMap.find pname class_map in
        let merged_features = merge_features parent_features features in
        ((loc, cname), Some (iloc, pname), merged_features)
    | None -> (* No inheritance *)
        ((loc, cname), None, features)
  ) sorted_classes

let base_class_order = ["Object"; "IO"; "Int"; "Bool"; "String"]

let sort_classes_by_dependencies updated_classes =
  let base_classes, user_classes = List.partition (fun cname ->
    List.mem cname base_class_order
  ) (List.map (fun ((_, cname), _, _) -> cname) updated_classes) in
  base_classes @ List.sort compare user_classes

let process_classes ast =
  let sorted_classes = topSort ast in
  let updated_classes = propagate_features ast sorted_classes in
  (* Now updated_classes will contain classes with inherited attributes and methods *)
  print_ast updated_classes

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
  List.iter (fun ((cloc, cname), inherits, features) ->
      List.iter (fun feature ->
          match feature with
          | Method ((mloc, mname), formals, mtype, mbody) when mname = "main" ->
              if List.length formals > 0 then (
                Printf.printf "ERROR: 0: Type-Check: class Main method main with 0 parameters not found\n";
                exit 1
              )
          | _ -> ()
        ) features
    ) ast

let check_dup_param ast =
  List.iter (fun ((cloc, cname), inherits, features) ->
    List.iter (fun feature ->
        match feature with
        | Method ((mloc, mname), formals, mtype, mbody) ->
            let seen_formals = Hashtbl.create 32 in
            List.iter (fun ((floc, fname), ftype) ->
                if Hashtbl.mem seen_formals fname then (
                  Printf.printf "ERROR: %s: Type-Check: Duplicate formal %s in method %s of class %s\n" floc fname mname cname;
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
                printf "ERROR: %s: Type-Check: Attribute named 'self' in class %s\n" floc cname;
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

let rec print_exp (loc, exp_kind) =
  Printf.printf "Expression (location: %s)\n" loc;
  match exp_kind with
  | Integer value -> Printf.printf "  Integer: %s\n" value
  | Bool value -> Printf.printf "  Bool: %s\n" value
  | String value -> Printf.printf "  String: %s\n" value
  | Plus((loc1, t1), (loc2, t2)) ->
    Printf.printf "plus\n";
    print_exp (loc1, t1);  (* Print first expression *)
    print_exp (loc2, t2)   (* Print second expression *)
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
    printf "read_list of %d\n" k;
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
          | x -> failwith ("cannot happen: " ^ x)
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
        | x -> failwith ("cannot happen: " ^ x)

      and read_formal () =
          let fname = read_id () in
          let ftype = read_id () in
          (fname, ftype)
      and read_exp () =
          let eloc = read () in
          let ekind = match read () with
          | "integer" ->
              let ival = read () in 
              Integer(ival)
          | "string" ->
              let ival = read () in 
              String(ival)
          | "Bool" -> 
              let ival = read () in
              String(ival)
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
          | "new" -> (*have to chage this*)
            let ival = read() in
            String(ival)
          | "self_dispatch" ->
            let ival = read() in
            String(ival)
          | "identifier" ->
            let ival = read() in
            String(ival)
          | x -> (* Fixme: do all of the others*)
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
            match inherits with
            | None -> ()
            | Some (iloc, iname) ->
            if iname = "Int" || iname = "Bool" || iname = "String" then begin
              printf "ERROR: %s : Type-Check: inheriting from forbidden class %s\n" iloc iname;
            exit 1
            end ;
            if not (List.mem iname all_classes) then begin
              printf "ERROR: %s: Type-Check: inheriting from undefined class %s\n" iloc iname;
              exit 1
            end ;
          ) ast;
          (* DONE WITH ERROR CHECKING *)
          
          (* Now we emit the CL-TYPE File *)
          
          (* For PA4_C_ -- we just do the class map *)
          
          let cname = (Filename.chop_extension fname) ^ ".cl-test" in 
          let fout = open_out cname in
          
          let rec output_exp (eloc, ekind) = 
            fprintf fout "%s\n" eloc ;
            match ekind with
            | Integer(ival) -> fprintf fout "integer\n%s\n" ival
            | String(ival) -> fprintf fout "string\n%s\n" ival
            | Bool(ival) -> fprintf fout "bool\n%s\n" ival
            | Plus((loc1,t1), (loc2,t2)) -> fprintf fout "plus\n%s\n%s\n" loc1 loc2
            | Times((loc1,_), (loc2,_)) -> fprintf fout "times\n%s\n%s\n" loc1 loc2
            | Divide((loc1,_), (loc2,_)) -> fprintf fout "divide\n%s\n%s\n" loc1 loc2
            | Minus((loc1,_), (loc2,_)) -> fprintf fout "minus\n%s\n%s\n" loc1 loc2
          in
          (* print_ast ast; *)
          (* printf "entering the topo\n"; *)
          let sorted_classes = topSort ast in
          let updated_classes = propagate_features ast sorted_classes in
          let last_ast = List.sort (fun ((_, cname1), _, _) ((_, cname2), _, _) ->
            compare cname1 cname2
          ) updated_classes in
          let last_classes = List.sort compare sorted_classes in
          print_ast last_ast;
          print_sorted_classes last_classes;
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
          ) last_classes; (* to do need to sort here *)
    close_out fout;
end ;;
main () ;;