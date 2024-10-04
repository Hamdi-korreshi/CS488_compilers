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
open Printf

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
          Printf.printf "ERROR: %s: Type-Check: Redefining class %s\n" cloc cname;
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

  let print_feature feature =
    match feature with
    | Attribute ((_, fname), (_, ftype), _) ->
        Printf.printf "    Attribute: %s of type %s\n" fname ftype
    | Method ((_, mname), formals, (_, mtype), _) ->
        Printf.printf "    Method: %s returning %s\n" mname mtype
  
  let print_map map =
    StringMap.iter (fun (cname : string) ((loc, cname), parent_opt, features) ->
      Printf.printf "Key: %s\n" cname;
      Printf.printf "Location: %s\n" loc;
      (match parent_opt with
      | Some (_, parent_name) -> Printf.printf "Inherits from: %s\n" parent_name
      | None -> Printf.printf "No parent (root class)\n");
      Printf.printf "Features (%d):\n" (List.length features);
      List.iter print_feature features;
      Printf.printf "--------------------------------------\n"
    ) map
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
        Printf.printf "Adding class: %s\n" cname;
        StringMap.add cname ((loc, cname), parent_opt, features) map
      ) map_with_bases ast
    
let rec topo_sort_helper class_map visited sorted cname =
  if StringSet.mem cname !visited then sorted
  else begin
    visited := StringSet.add cname !visited;
    let class_info =
      try
        StringMap.find cname class_map
      with Not_found ->
        Printf.printf "Error: Class %s not found in map\n" cname;
        exit 1
    in
    let ((loc, cname), parent_opt, _) = class_info in
    let sorted = match parent_opt with
      | Some (_, pname) ->
          topo_sort_helper class_map visited sorted pname
      | None -> sorted
    in
    cname :: sorted
  end

let topo_sort ast =
  let class_map = build_class_map ast in
  let visited = ref StringSet.empty in
  let sorted = ref [] in
  let sorted_keys = List.sort String.compare (StringMap.fold (fun key _ acc -> key :: acc) class_map []) in
  List.iter (fun cname ->
    sorted := topo_sort_helper class_map visited !sorted cname
  ) sorted_keys;
  List.rev !sorted


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

let print_sorted_classes sorted_classes =
  List.iter (fun cname -> Printf.printf "%s\n" cname) sorted_classes

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

let main () = begin
  printf "start main \n";
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
          | "Bool" -> (* Fixme: do all of the others*)
              let ival = read () in
              String(ival)
          | "plus" ->
              let ival = read() in
              String(ival)
          | "new" -> (*have to chage this*)
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
          printf "CL-AST de-serizlized, %d classes\n" (List.length ast);
          let base_classes = ["Int"; "string"; "Bool"; "IO"; "Object" ] in
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
          print_ast ast;
          (* DONE WITH ERROR CHECKING *)
          
          (* Now we emit the CL-TYPE File *)
          
          (* For PA4_C_ -- we just do the class map *)
          
          let cname = (Filename.chop_extension fname) ^ ".cl-test" in 
          let fout = open_out cname in
          
          let rec output_exp (eloc, ekind) = 
            fprintf fout "%s\n" eloc ;
            match ekind with
            | Integer(ival) -> fprintf fout "integer\n%s\n" ival
            | String(ival) -> fprintf fout "string\n %s \n" ival
            | Bool(ival) -> fprintf fout "bool\n %s \n" ival
          in
          
          fprintf fout "class_map\n%d\n" (List.length all_classes) ;
          List.iter (fun cname ->
          
          (* name of class, # attrs, each attr=feature in turn *)
          printf "entering the topo\n";
          let sorted_classes = topo_sort ast in
          print_sorted_classes sorted_classes;
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
            let _, inherits, features = List.find (fun ((_, cname2),__,_) -> cname = cname2) ast in
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
          ) all_classes;
    close_out fout;
end ;;
main () ;;