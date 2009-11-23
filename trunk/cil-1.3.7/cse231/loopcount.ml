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
             ignore(Pretty.printf "\n[old]-->%d %s %a\n" vi.vid vi.vname d_exp (exp));
             ignore(Pretty.printf "\n[new]-->%d %s %a\n" vi.vid vi.vname d_exp (e'));
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

let var_IntMap = ref IntMap.empty
let init_IntMap = ref IntMap.empty

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

