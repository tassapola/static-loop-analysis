open Cil
open Pretty
open Cfg
open Printf

module IntMap = Map.Make(struct type t = int let compare = compare end)

type regT = {
  rvi : varinfo;
  rval : exp;
}

type varmapT = regT IntMap.t

(* type ifoutputT = True | False | Unknown *)

type loopinfoT = {
  loopcount : int; 
  nextstmt  : stmt; 
  newvarmap : varmapT;
}

type cacheT = { 
  csid      : int;
  cvarmap   : varmapT; 
  cloopinfo : loopinfoT;
} 

type countmapT = int IntMap.t
let countmap : countmapT ref = ref IntMap.empty

let cacheList : cacheT list ref = ref []

type countT =  {
  ckey : int;
  cval : int;
}

let countList : countT list ref = ref []

let addTocountList (k:int) (v:int) =
  countList := {ckey=k;cval=v}::(!countList) 
 

let saveToCache (stmtid:int) (varmap:varmapT) (li:loopinfoT) (l:cacheT list ref) : unit =
  let oldList = !l in
    l := {csid=stmtid; cvarmap=varmap; cloopinfo=li}::oldList


let printVarMap (k:int) (v:regT): unit =  
 ignore(Pretty.printf "(%s,%a) " v.rvi.vname d_exp (v.rval))  


let rec isCached (stmtid:int) (varmap:varmapT) (l:cacheT list) : loopinfoT option  = 
  match l with
     [] -> None
   | x::rest ->  
        begin
                (* ignore(Printf.printf "varmap   = ");
                IntMap.iter printVarMap varmap;
                ignore(Printf.printf "\n");
                ignore(Printf.printf "x.cvarmap= ");
                IntMap.iter printVarMap x.cvarmap;
                ignore(Printf.printf "\n"); *)
                if (x.csid == stmtid && (compare (x.cvarmap) (varmap) == 0)) then
                   Some (x.cloopinfo)
                else 
                   isCached (stmtid) (varmap) (rest)
        end

class getVarExpClass (l:int list ref) =
object
  inherit nopCilVisitor
        
  method vexpr = function
        Lval (Var v, NoOffset) ->
          begin
                l := (v.vid::(!l));
                DoChildren
          end
      | _ -> DoChildren
end


let traverseInstr (i:instr): int list = 
  let l = ref [] in
  match i with
   Set (lv,e,_) ->  begin
                       (l :=  match lv with
                               Var vi,o -> [vi.vid]
                             | _ -> []);
                      visitCilExpr (new getVarExpClass l) e; (!l)
                     end
   | _ -> !l 

let rec traverseInstrList (instrList: instr list): int list =
  match instrList with
     [] -> []
   | x::rest -> traverseInstr(x)@(traverseInstrList rest)


let rec

traverseStmt (s:stmt): int list = 
  let l = ref [] in
  match s.skind with
    Instr inst_list -> traverseInstrList (inst_list)
  | If (e,_,_,_) -> visitCilExpr (new getVarExpClass l) e; (!l)
  | Loop (b,_,_,_) -> traverseBlock (b)
  | Block b -> traverseBlock (b)
  | _ -> !l
 
and 

traverseStmtList (stmtList: stmt list) : int list = 
  match stmtList with
     [] -> []
   | x::rest -> traverseStmt(x)@(traverseStmtList rest)

and

traverseBlock (blk:block) : int list = 
  traverseStmtList (blk.bstmts)


class rewriteExpClass (varmap:varmapT) =
object
  inherit nopCilVisitor
  method vexpr = function
        Lval (Var v, NoOffset) ->
          begin
            try
              let defined = IntMap.find v.vid varmap in
                (* ignore(Pretty.printf ">>> %a \n" d_exp (defined.rval)); *)
                match defined.rval with
                    Const x -> ChangeTo defined.rval
                  | _ -> DoChildren
            with Not_found -> DoChildren
          end
      | _ -> DoChildren
end


let evalExp r (e:exp) : exp = 
  constFold true (visitCilExpr (new rewriteExpClass r) e)


let evalIfExp r (e:exp) : bool = 
  let e' = evalExp r e in
    not (isZero e')


let extractVar (rMap:varmapT) (x:instr) : varmapT = 
  match x with
    Set(lval,exp,location) -> 
      (match lval with
         Var vi,o ->
           let e' = evalExp rMap exp in 
             (* ignore(Pretty.printf "\n[old]-->%d %s %a\n" vi.vid vi.vname d_exp (exp));
                ignore(Pretty.printf "\n[new]-->%d %s %a\n" vi.vid vi.vname d_exp (e')); *)
             IntMap.add vi.vid {rvi = vi; rval = e'} rMap
         | _ -> rMap)
    | _ -> rMap


let rec extractInst (rMap:varmapT) (l:instr list) : varmapT =
  match l with
     [] -> rMap
   | x::xs -> extractInst (extractVar (rMap) x) xs


let extractStmt (rMap:varmapT) (s:stmt) : varmapT =
  match s.skind with
      Instr(inst_list) -> extractInst (rMap) (inst_list)
    | _ -> rMap 


let update_loop_count(s:stmt) (loop_count_map : countmapT ref) (value:int) = 
	try
		let oldvalue = IntMap.find s.sid !loop_count_map in
		IntMap.add s.sid (oldvalue+value) !loop_count_map
	with Not_found -> IntMap.add s.sid value !loop_count_map



let rec inList (k:int) (tb:int list): bool = 
  match tb with
     [] -> false
   | x::rest -> if (x == k) then true
                else inList (k) (rest) 



let computeIntersection (varmap:varmapT) (tb:int list) : varmapT =
  let vm = ref IntMap.empty in
  let f (k:int) (v:regT) = (if (inList (k) (tb)) then vm := (IntMap.add k v !vm) else ())
  in
   IntMap.iter f varmap;
   (!vm)


let updateMap (target:varmapT) (source:varmapT) : varmapT =
  let vm = ref target in
  let f (k:int) (v:regT) = vm := (IntMap.add k v !vm) in
    IntMap.iter f source;
    (!vm)

 

let rec 

loop_iterate (cur_stmt_id:int) (s:stmt) (count:int) (varmap:varmapT) : loopinfoT = 
  let newvmap = extractStmt varmap s in 
    match s.skind with
        Break (_) -> { loopcount=count-1; nextstmt=List.hd s.succs; newvarmap=newvmap }
      | If (e,_,_,_) -> if not (evalIfExp varmap e) then
                          loop_iterate (cur_stmt_id) (List.hd (List.tl s.succs)) count newvmap
                        else 
			  loop_iterate (cur_stmt_id) (List.hd s.succs) count newvmap
      | Loop (b,_,_,_) -> if (s.sid == cur_stmt_id) then
                            loop_iterate (cur_stmt_id) (List.hd s.succs) (count+1) newvmap
                          else
                            let tb = traverseBlock b in
                            let li = analyzeLoop s newvmap tb in
                              loop_iterate (cur_stmt_id) (li.nextstmt) count (li.newvarmap)
      | _ -> loop_iterate (cur_stmt_id) (List.hd s.succs) count newvmap
and

analyzeLoopIterative (s:stmt) (varmap:varmapT) : loopinfoT =
  loop_iterate s.sid s 0 varmap 

and

analyzeLoop (s:stmt) (varmap:varmapT) (tb:int list): loopinfoT  = 
  let loopinfo_o = isCached (s.sid) (computeIntersection varmap tb) (!cacheList) in
    match loopinfo_o with
      Some li -> 
          let updatedmap = updateMap varmap li.newvarmap in
          countmap := update_loop_count s countmap (li.loopcount);
          ignore(Printf.printf "Loop count [SID=%d]:\t%d\t[CACHED]\n" s.sid li.loopcount);
          {loopcount=li.loopcount; nextstmt=li.nextstmt; newvarmap=updatedmap}
    | None ->    
        let newli = analyzeLoopIterative (s) (varmap) in
        let updatedmap = computeIntersection newli.newvarmap tb in 
          countmap := update_loop_count s countmap (newli.loopcount);
          ignore(Printf.printf "Loop count [SID=%d]:\t%d\n" s.sid newli.loopcount);
          saveToCache (s.sid) (computeIntersection varmap tb)
          {loopcount=newli.loopcount; nextstmt=newli.nextstmt; newvarmap=updatedmap} (cacheList);
          newli
 

let rec printvar (l:int list) =
  match l with
   [] -> ignore(Printf.printf "\n");
  | x::rest -> begin
                  ignore(Printf.printf "%d " x);
                  printvar (rest);
               end


let rec iterate (s:stmt) (var_IntMap:varmapT) : unit = 
  match s.succs with
    [] -> ()
  | _  -> let newvmap = extractStmt var_IntMap s in
           match s.skind with
	     If (e,_,_,_) -> if not (evalIfExp newvmap e) then 
                               iterate (List.hd (List.tl s.succs)) newvmap
                             else 
			       iterate (List.hd s.succs) newvmap
	   | Loop (b,_,_,_) -> let tb = traverseBlock b in
                               let loopinfo = analyzeLoop s newvmap tb in
                                 (* printvar tb; *)
                                 iterate (loopinfo.nextstmt) (loopinfo.newvarmap)
	   | _ -> iterate (List.hd s.succs) newvmap

let main (f:file) : unit = 
begin
  computeFileCFG f;
  let stmt_list = allStmts f in 
  begin
    ignore(Printf.printf "=============START==============\n" );
    iterate (List.hd stmt_list) IntMap.empty;
    ignore(Printf.printf "==============DONE==============\n" );
  end
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

