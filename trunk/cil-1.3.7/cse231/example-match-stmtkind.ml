open Cil
open Pretty
open Cfg

open Printf

(*
let print_varinfo(a:varinfo) : string = 
	let f attribute = "zz" in  
	"variable_name=" ^ a.vname
	^ ",variable_attr=" ^
	List.iter f a.vattr;;
*)

let print_varinfo(a:varinfo) : string = 
	"variable_name=" ^ a.vname;;

let print_lhost(a:lhost) : string =
	match a with
	Var (varinfo) -> print_varinfo varinfo
	| Mem (exp) -> "mem";;

let print_lval(a:lval) : string = 
	 print_lhost (fst(a));;
	
let print_instr(a:instr) : string =
	match a with
	Set (lval,exp,location) -> "set " ^ (print_lval lval)
	| Call (_,_,_,_) -> "call"
	| Asm (_,_,_,_,_,_) -> "asm"
;;

let rec print_instr_list(l:instr list) : string =
	match l with
	[] -> ""
	| x :: remainder -> (print_instr x) ^ (print_instr_list remainder);;

let print_stmtkind(k:stmtkind) : string =
	match k with
	Instr (l) -> "instr-" ^ (print_instr_list l)
	| Return (_,_) -> "return"
	| Goto (_,_) -> "goto"
	| Break (_) -> "break"
	| Continue (_) -> "location"
	| If (_,_,_,_) -> "if"
	| Switch (_,_,_,_) -> "switch"
	| Loop (_,_,_,_) -> "loop"
	| k -> "others";;

let tor1 (f:file) : unit = begin
	let ff elem = Printf.printf "stmt_kind=%s sid=%d \n" (print_stmtkind elem.skind) elem.sid in
	let p2 = computeFileCFG f in
	let stmt_list = allStmts f in
	List.iter ff stmt_list;
end

let tor2 (f:file) : unit = begin
	let ff elem = (Pretty.printf "%a\n" Cil.d_stmt elem) in
	let p2 = computeFileCFG f in
	let stmt_list = allStmts f in
	List.iter ff stmt_list;
end

let feature : featureDescr = 
  { fd_name = "tor2";              
    fd_enabled = ref false;
    fd_description = "tor2";
    fd_extraopt = [];
    fd_doit = 
    (function (f: file) -> 
	tor2 f);
    fd_post_check = false;

  } 

