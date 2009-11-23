open Pretty
open Cil
open Cfg
open List
open Dominators
module E = Errormsg
module H = Hashtbl

module IntMap = Map.Make (struct
                              type t = int
                              let compare x y = x - y
                            end)

type reg = {
    rvi : varinfo;
    rval : exp;
  }

type t = reg IntMap.t

let extractVar rMap (x:instr) = begin
	match x with
		Set(lval,exp,location) -> begin
			match lval with
				Var vi,o -> begin
					ignore(Pretty.printf "\n-->%d %s %a\n" vi.vid vi.vname d_exp (exp));
					IntMap.add vi.vid {rvi = vi; rval = exp} rMap;					
					end								
				|_ -> rMap;
				end
		|_ -> rMap;
end

let extractStmt rMap (s:stmt) = begin
	match s.skind with
 		Instr(inst_list) -> 
			let rec extractInst = function
				[] -> ()
				| x :: xs -> begin
					rMap = extractVar rMap x;
					extractInst xs;
					end	
			in
				extractInst inst_list;
		|_ -> () 
end

class printCFGVisitor = object
	inherit nopCilVisitor

	method vstmt (s:stmt) =	 
		ignore(Pretty.printf "===================================================");
		let rMap = IntMap.empty in
		ignore(extractStmt rMap s);		
		DoChildren
end

let feature : featureDescr =
  { fd_name = "loopcount";
  fd_enabled = ref false;
  fd_description = "output loop count to stdout";
  fd_extraopt = [];
  fd_doit =
  (function (f: file) ->
   Cfg.computeFileCFG f;
   let eVisitor = new printCFGVisitor in
   visitCilFileSameGlobals eVisitor f);
  fd_post_check = true;
  }
