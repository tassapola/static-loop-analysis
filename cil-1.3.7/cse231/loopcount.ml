open Cil
open Pretty
open Cfg
open Printf

module IntMap = Map.Make(struct type t =  int let compare = compare end);;
type loop_count_type = int IntMap.t;;

let init_IntMap = ref IntMap.empty;;

let update_loop_count(s:stmt) (loop_count_map: loop_count_type ref) = 
	try
		let oldvalue = IntMap.find s.sid !loop_count_map in
		IntMap.add s.sid (oldvalue+1) !loop_count_map
	with Not_found -> IntMap.add s.sid 1 !loop_count_map


let find_next_stmt_loop(s:stmt) (loop_count_map: loop_count_type ref) : stmt =
	try
	 loop_count_map := update_loop_count s loop_count_map;
		ignore(Printf.printf "Loop count: %d\n" (IntMap.find s.sid !loop_count_map));
		List.hd s.succs;
	with Not_found -> 
		(List.hd s.succs);;	

let rec iterate(s:stmt) (loop_count_map:loop_count_type ref) : unit = 
	ignore(Pretty.printf "\n\n===STATEMENT: %a===\n" d_stmt s);

	let next_stmt = match s.skind with
	Instr (a) -> List.hd s.succs;
	| Return (a,b) -> List.hd s.succs;
	| Goto (_,_) -> List.hd s.succs;
	| Break (_) -> List.hd s.succs;
	| Continue (_) -> List.hd s.succs;
	| If (_,_,_,_) -> List.hd s.succs;
	| Switch (_,_,_,_) -> List.hd s.succs;
	| Loop (_,_,_,_) -> find_next_stmt_loop s loop_count_map;
	| _ -> List.hd s.succs;
	in
	iterate next_stmt loop_count_map;;

let main (f:file) : unit = begin
	let c = computeFileCFG f in
	let stmt_list = allStmts f in
	iterate (List.hd stmt_list) init_IntMap;
end

let feature : featureDescr = 
  { fd_name = "loopcount";              
    fd_enabled = ref false;
    fd_description = "Static loop analysis";
    fd_extraopt = [];
    fd_doit = 
    (function (f: file) -> 
	main f);
    fd_post_check = false;

  } 

