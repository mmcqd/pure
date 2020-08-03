open Dynamics
open Statics
open Parser
open Pure

let t = PI (("a",SORT "*"),PI (("_",B 0),B 1))

let id = ABS (("A",SORT "*"),ABS (("x",B 0),B 0))

let id_t = PI (("A",SORT "*"), PI (("x",B 0), B 1))

let sorts = ["*";"BOX"]
let rules = ([("*","BOX")],[("*","*");("BOX","*");("BOX","BOX")])

let parse = let p = mkParser sorts in fun s -> match p s with Some t -> bind_up t | _ -> raise (Failure "")


let rec repl () =
  print_string ">>> ";
  let s = Stdlib.read_line () in
  let e = parse s in
  let t = synthtype rules Context.empty e in
  let e' = Dynamics.beta Context.empty e in
  print_endline (pretty t);
  print_endline (pretty e');
  print_string "\n";
  repl ()

let _ = repl ()
