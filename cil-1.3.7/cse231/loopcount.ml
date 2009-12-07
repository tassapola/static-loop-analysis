open Cil
open Pretty
open Cfg
open Printf

module IntMap = Map.Make(struct type t =  int let compare = compare end)

type reg = {
  rvi : varinfo;
  rval : exp;
}

type t = reg IntMap.t

type ifoutput = True | False | Unknown

let var_IntMap = ref IntMap.empty
let init_IntMap = ref IntMap.empty

class rewriteExpClass (regFile : t) =
object
  inherit nopCilVisitor
  method vexpr = function
        Lval (Var v, NoOffset) ->
          begin
            try
              let defined = IntMap.find v.vid regFile in
                (* ignore(Pretty.printf ">>> %a" d_exp (defined.rval)); *)
                match defined.rval with
                    Const x -> ChangeTo defined.rval
                  | _ -> DoChildren
            with Not_found -> DoChildren
          end
      | _ -> DoChildren
end


let evalExp r (e : exp) : exp = 
  constFold true (visitCilExpr (new rewriteExpClass r) e)


let evalIfExp r (e : exp) : bool = 
  let e' = evalExp r e in
    not (isZero e')


let extractVar rMap (x:instr) = 
  match x with
    Set(lval,exp,location) -> 
      (match lval with
         Var vi,o ->
           let e' = evalExp rMap exp in 
             (*ignore(Pretty.printf "\n[old]-->%d %s %a\n" vi.vid vi.vname d_exp (exp));
             ignore(Pretty.printf "\n[new]-->%d %s %a\n" vi.vid vi.vname d_exp (e'));*)
             IntMap.add vi.vid {rvi = vi; rval = e'} rMap
         | _ -> rMap)
    |_ -> rMap


let extractStmt (rMap : t ref) (s:stmt) =
  match s.skind with
    Instr(inst_list) -> 
      let rec extractInst = function
        [] -> ()
	| x :: xs ->   
          rMap := extractVar (!rMap) x;
	  extractInst xs;
      in
        extractInst inst_list;
    |_ -> () 



type loop_count_type = int IntMap.t

let update_loop_count(s:stmt) (loop_count_map: loop_count_type ref) (value: int)= 
	try
		let oldvalue = IntMap.find s.sid !loop_count_map in
		IntMap.add s.sid (oldvalue+value) !loop_count_map
	with Not_found -> IntMap.add s.sid value !loop_count_map

let rec loop_iterate (cur_stmt_id:int) (s:stmt) (count:int) (var_IntMap : t ref) (loop_count_map: loop_count_type ref)= 
  extractStmt var_IntMap s; 
        let loop_info = match s.skind with
	Break (_) -> ("break",(List.hd s.succs));
	| If (e,_,_,_) -> ("if",(if not (evalIfExp !var_IntMap e) then(List.hd (List.tl s.succs)) else (List.hd s.succs)));
	| Loop (_,_,_,_) -> ("loop",(List.hd s.succs));
	| _ -> ("other",(List.hd s.succs));
	in	
		if((compare (fst loop_info) "break") == 0) then ((count-1), s)
		else if((compare (fst loop_info) "loop") == 0) then 
			if (s.sid == cur_stmt_id) then loop_iterate cur_stmt_id (snd loop_info) (count+1) var_IntMap loop_count_map
			else (
				let inner_loop_info = loop_iterate s.sid (snd loop_info) 1 var_IntMap loop_count_map in
				(
					loop_count_map := update_loop_count s loop_count_map (fst inner_loop_info); 
					ignore(Printf.printf "Inner Loop count:%d %d\n" s.sid (IntMap.find s.sid !loop_count_map));
					loop_iterate cur_stmt_id (List.hd (snd inner_loop_info).succs) (count) var_IntMap loop_count_map
				)
			     )
		else 
			loop_iterate cur_stmt_id (snd loop_info) (count) var_IntMap loop_count_map;;
		
let find_next_stmt_loop(s:stmt) (loop_count_map: loop_count_type ref) (var_IntMap : t ref) : stmt =
	try 	
		let loop_count = loop_iterate s.sid s 0 var_IntMap loop_count_map in
		(*loop_count_map := update_loop_count s loop_count_map;*)
		loop_count_map := update_loop_count s loop_count_map (fst loop_count);
		ignore(Printf.printf "Loop count:%d %d\n" s.sid (IntMap.find s.sid !loop_count_map));
		(snd loop_count)	
	with Not_found -> 
		(List.hd s.succs);;	

let rec iterate (s:stmt) (loop_count_map:loop_count_type ref) : unit = 
  extractStmt var_IntMap s;
	let next_stmt = match s.skind with
	Instr (a) -> List.hd s.succs;
	| Return (a,b) -> List.hd s.succs;
	| Goto (_,_) -> List.hd s.succs;
	| Break (_) -> List.hd s.succs;
	| Continue (_) -> List.hd s.succs;
	| If (e,_,_,_) -> (if not (evalIfExp !var_IntMap e) then List.hd (List.tl s.succs) else List.hd s.succs);
	| Switch (_,_,_,_) -> List.hd s.succs;
	| Loop (_,_,_,_) -> find_next_stmt_loop s loop_count_map var_IntMap;
	| _ -> List.hd s.succs;
	in
	iterate next_stmt loop_count_map;;

let main (f:file) : unit = begin
	let c = computeFileCFG f in 
	let stmt_list = allStmts f in
		iterate (List.hd stmt_list) init_IntMap ;
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

