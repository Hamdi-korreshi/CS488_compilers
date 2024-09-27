(* Hamdi Korreshi and Tomasz Bruahcnatez
  PA4 Semantic Analyzer Checkpoint *)
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
          | x -> (* Fixme: do all of the others*)
            failwith ("expression kind unhandled: " ^ x)
          in
          (eloc, ekind)
          in
          let ast = read_cool_program () in
          close_in fin ;
          printf "CL-AST de-serizlized, %d classes\n" (List.length ast);
          let base_classes = ["Int"; "string"; "Bool"; "IO"; "object" ] in
          let user_classes = List.map (fun ((_, cname),_,_) -> cname) ast in
          let all_classes = base_classes @ user_classes in
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
          ) ast ;
          
          (* DONE WITH ERROR CHECKING *)
          
          (* Now we emit the CL-TYPE File *)
          
          (* For PA4_C_ -- we just do the class map *)
          
          let cname = (Filename.chop_extension fname) ^ ".cl-type" in 
          let fout = open_out cname in
          
          let rec output_exp (eloc, ekind) = 
            fprintf fout "%s\n" eloc ;
            match ekind with
            | Integer(ival) -> fprintf fout "integer\n%s\n" ival
          in
          
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
            let _, inherits, features = List.find (fun ((_, cname2),__,_) -> cname = cname2) ast in
            List.filter (fun feature -> match feature with
            | Attribute _ -> true
            | Method _ -> false
            ) features
            with Not_found ->(*bool/int/object *)
              []
          in
          
          fprintf fout "%d\n" (List.length attributes);
          fprintf fout "%d\n" (List.length attributes);
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