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

let extractVar rMap (x:instr) =
	match x with
		Set(lval,exp,location) -> 
			(match lval with
				Var vi,o -> 
					ignore(Pretty.printf "\n-->%d %s %a\n" vi.vid vi.vname d_exp (exp));
					IntMap.add vi.vid {rvi = vi; rval = exp} rMap;													
				|_ -> rMap)
		|_ -> rMap

let extractStmt (rMap:reg IntMap.t ref) (s:stmt) = 
	match s.skind with
 		Instr(inst_list) -> 
			let rec extractInst = function
				[] -> ()
				| x :: xs -> begin
					rMap := extractVar !rMap x;
					if IntMap.is_empty !rMap then ignore(Pretty.printf "Empty\n" );
					ignore(IntMap.iter (fun vid v -> ignore(Pretty.printf "\n>>>> %d %a\n" vid d_exp (v.rval))) !rMap);
					extractInst xs;
					end	
			in
				extractInst inst_list
		|_ -> ()

class printCFGVisitor = 
object
	inherit nopCilVisitor
	val rMap = ref IntMap.empty
	method vstmt (s:stmt) =	 
		ignore(Pretty.printf "===================================================");
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
