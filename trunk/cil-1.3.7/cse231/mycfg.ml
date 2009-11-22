open Pretty
open Cil
open Cfg
open List
open Dominators
module E = Errormsg
module H = Hashtbl

class printCFGVisitor = object
  inherit nopCilVisitor
  method vstmt (s:stmt) =
  ignore(Pretty.printf "===================================================");
   ignore(Pretty.printf "\n\n===STATEMENT: %a===\n" d_stmt s);
   let rec printStmtList = function
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

let feature : featureDescr =
  { fd_name = "mycfg";
  fd_enabled = ref false;
  fd_description = "output cfg result to stdout";
  fd_extraopt = [];
  fd_doit =
  (function (f: file) ->
   Cfg.computeFileCFG f;
   let eVisitor = new printCFGVisitor in
   visitCilFileSameGlobals eVisitor f);
  fd_post_check = true;
  }
