open Pretty
open Cil
open Cfg
open List
open Dominators
open Liveness
module E = Errormsg
module H = Hashtbl

module IntMap = Map.Make(struct
			   type t = int
			   let compare x y = x - y
			 end)

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



class printCFGVisitor = object
  inherit nopCilVisitor

  val rMap = ref IntMap.empty

  method vstmt (s:stmt) =	 
    ignore(Pretty.printf "===================================================\n");
    extractStmt rMap s;		
    DoChildren
end

(* 
let printLiveSet () (sid : int) = 
  (match getLiveSet sid with
    None ->  Pretty.nil
  | Some data -> LiveFlow.pretty () data)

class printCFGVisitor = object
  inherit nopCilVisitor
  method vstmt (s:stmt) =
   ignore(Pretty.printf "===================================================");
   ignore(Pretty.printf "\n\n===STATEMENT: %a===\n" d_stmt s);
   ignore(Pretty.printf "\n\n>>>>>>>>>>>>>>> %a\n" printLiveSet (s.sid));
   let 
     rec printStmtList = function
      [] -> Pretty.printf "\n"
      | x :: rest -> ignore(Pretty.printf "\n-->%a" d_stmt (x)); 
                     printStmtList rest;
   in
    ignore(Pretty.printf "__________preds_____________\n");
    ignore(printStmtList s.preds);
    ignore(Pretty.printf "__________end preds_____________\n");
    ignore(Pretty.printf "__________succs_____________\n");
    ignore(printStmtList s.succs);
    ignore(Pretty.printf "__________end succs_____________\n");
    DoChildren
end
 
let computeMainLiveness (f : file) = 
  iterGlobals f (fun g -> 
    match g with 
      GFun(fd,_) -> 
        (if (compare fd.svar.vname "main") = 0 then Liveness.computeLiveness fd else ())
      | _ -> ())
*)

let feature : featureDescr =
  { fd_name = "loopcountCP";
  fd_enabled = ref false;
  fd_description = "output loop count to stdout with CP";
  fd_extraopt = [];
  fd_doit =
  (function (f: file) ->
   Cfg.computeFileCFG f;
   (*computeMainLiveness f;*) 
   let eVisitor = new printCFGVisitor in
   visitCilFileSameGlobals eVisitor f);
  fd_post_check = true;
  }
