module StringMap = Map.Make(String)

let print_tuple (x, y) =
  Printf.printf "(%s, %s)\n" x y

let print_list (lst: string list) =
  List.iter print_endline lst

let print_string_list (lst: string list) =
  let result = String.concat ", " lst in
  Printf.printf "%s\n" result

type graph = {
  deps_map : string list StringMap.t; (* str: [str]  needs to have the ; for any of the type things*)
  in_deg : int StringMap.t; (* str: int *)
}

(* 'a means generic so types don't matter for the queue*)
type 'a func_queue = { front: 'a list; back: 'a list}

let empty_q = {
  front = []; 
  back = [];
}

let append value que = {
  que with back =
  value :: que.back
}

let check_empty = function
  | { front = []; back = []} -> true
  | { front; back} -> false

let deque = function 
  | { front = []; back = []} -> None
  | { front = x :: xs; back } -> Some (x, { front= xs; back = back })
  | { front = []; back } ->
      let front_rev = List.rev back in
      Some (List.hd front_rev, {front = List.tl front_rev; back = []})

let sort_queue que =
  let combined = que.front @ List.rev que.back in  (* bring them together so its easier *)
  let sorted = List.sort compare combined in    
  { front = sorted; back = [] }

let init_map = {
  deps_map = StringMap.empty;
  in_deg = StringMap.empty;
}

let add_to_strmap key str_list (maps: graph) = 
  { maps with deps_map = StringMap.add key str_list maps.deps_map }

let add_to_deg key deg (maps: graph) =
  { maps with in_deg = StringMap.add key deg maps.in_deg }

let print_deps deps_map = 
  StringMap.iter ( fun key lst ->
    let stringed = String.concat "; " lst in
    Printf.printf "%s -> [%s]\n" key stringed
    ) deps_map

let print_int int_map =
  StringMap.iter ( fun key num ->
    Printf.printf "%s In Degree: %d\n" key num 
    ) int_map

let print_maps the_graph = 
  Printf.printf "Deps Map: \n";
  print_deps the_graph.deps_map;
  Printf.printf "Int Map: \n";
  print_int the_graph.in_deg

let rec loader () = 
  try
    let line = read_line () in 
    line :: loader () 
  with
  | End_of_file -> []

let rec tup_maker (str_input: string list) = 
    match str_input with
    | [] -> []
    | [x] -> 
      print_endline "Odd tasks not possible check input.";
      exit 1
    | x :: y :: rest -> 
      (x,y) :: tup_maker rest

let rec build_deps (match_y : string) (tasks : (string * string) list ) : string list = 
  match tasks with
  | [] -> []
  | (x,y) :: rest ->
    if y = match_y then (
      (* Printf.printf "Checking y:";
      print_tuple (x,y); *)
      x :: build_deps match_y rest )
    else
      build_deps match_y rest

let rec remove match_y lst = 
  match lst with
  | [] -> []
  | (x,y) :: rest -> 
    if y = match_y then 
      remove match_y rest
    else
      (x,y) :: remove match_y rest

let rec build_pre_deps (tasks : (string * string) list ) =
  match tasks with
  | (x,y) :: rest ->
    (* Printf.printf "Checking: ";
    print_tuple (x,y); *)
    [y, [x :: build_deps y rest]] @ build_pre_deps (remove y rest)
  | [] -> [] 

let rec key_exists key_str = function
  | [] -> false
  | (k, _) :: rest -> if k = key_str then true else key_exists key_str rest

let add_leafs (key: string list) (val_list: (string * string list list) list) =
  List.fold_left (fun acc k ->
    if key_exists k acc then
      acc
    else
      (k, []) :: acc 
  ) val_list key

let flatten lsts =
  List.fold_left (@) [] lsts

let add_with_fold node_map comp =
  List.fold_left (fun acc (parent, child) -> 
    let flat_child = flatten child in  (* Flatten the child list *)
    StringMap.add parent flat_child acc
  ) node_map comp

let count_deg str completed =
  List.fold_left (fun count (key, rest) ->
    let flat_rest = flatten rest in  (* Flatten the rest list *)
    if List.exists (fun x -> x = str) flat_rest then
      count + 1
    else
      count
  ) 0 completed  

let rec int_map u comp deg =
  match u with
  | [] -> deg
  | str :: rest ->
    let count = count_deg str comp in 
    let update_deg = StringMap.add str count deg in 
    int_map rest comp update_deg

let make_maps u comp =
  let that_map = init_map in
  let updated_dep = add_with_fold that_map.deps_map comp in
  let updated_int = int_map u comp that_map.in_deg in
  { that_map with deps_map = updated_dep; in_deg = updated_int }

let construct str_input tup_built = 
  let unfinished_deps = build_pre_deps tup_built in
  let unique = (List.sort_uniq compare str_input) in
  let completed = add_leafs unique unfinished_deps in
  let dag = make_maps unique completed in
  dag

let deps (str_input : string list) = 
  let tup_built = tup_maker str_input in
  let bruh = construct str_input tup_built in
  bruh

(* (string list StringMap.t * int) *)
let rec build_dag (str_input : string list) = 
  let bruh = deps str_input in
  bruh

let find_key value map = 
  StringMap.fold ( fun k v acc ->
    if v = value then 
      Some k
    else 
      acc
  ) map None

let find_zeros min_heap map = 
  StringMap.fold ( fun key value min_heap -> 
    if value = 0 then
      let min_heap = append key min_heap in
      min_heap
    else 
      min_heap
    ) map min_heap

let rec child_loop map child min_heap = 
  match child with
  | [] -> map, min_heap
  | node :: rest ->
    let new_deg = StringMap.find node map.in_deg in
    let returned = StringMap.add node (new_deg-1) map.in_deg in
    let updated_map = { map with in_deg = returned} in
    let min_heap = if StringMap.find node updated_map.in_deg = 0 then
      let sorted = sort_queue (append node min_heap) in
      sorted
    else 
      min_heap
    in
    child_loop updated_map rest min_heap

let rec while_loop (map :graph) final min_heap count =
  if not (check_empty min_heap) then
    let sorted = sort_queue min_heap in
    let curr, min_heap = match deque sorted with
      | Some (curr, new_heap) -> curr, new_heap
      | None -> failwith "Queue is empty"
    in
    let final = final @ [curr] in
    let value = StringMap.find curr map.deps_map in
    let sorted = sort_queue min_heap in
    let map, min_heap = child_loop map value sorted in
    let count = count + 1 in
    while_loop map final min_heap count
  else 
    if count < StringMap.cardinal map.in_deg then (
      print_endline "cycle";
      exit 1
    )
    else
      final

let topSort (the_graph: graph) = 
  let min_heap = empty_q in
  let updated_heap = find_zeros min_heap the_graph.in_deg in
  let final = [] in
  let print_ready = while_loop the_graph final updated_heap 0 in
  print_ready

let () =
  let jack = loader () in 
  let rose = build_dag jack in
  let caledon = topSort rose in
  print_list caledon