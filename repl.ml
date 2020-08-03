open Dynamics
open Statics
open Parser
open Pure

let sorts = ["Prop";"Type"]
let rules = ([("Prop","Type")],
             [("Prop","Prop");("Type","Prop");("Type","Type");("Prop","Type")])

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
