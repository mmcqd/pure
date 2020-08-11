
module PP = Prim_parser.Make (COMBI.Parser.Make (COMBI.Parser.OptionBase))
open PP

let parse p s =
  match p % s with
    | Some (t,[]) -> t
    | _ -> raise ParseError

let parse' p s =
  match p s with
    | Some (t,[]) -> t
    | _ -> raise ParseError


module MkRepl (T : Pure.THEORY) =
struct
  module S = Statics.Make (T)
  open S
  open Lang_parser.Make (PP) (T)
  open Pure
  open Dynamics

  let prgm = prgm

  let fold_decs = 
    List.fold_left 
    (fun (g,d) (x,e) ->
      let e = bind_up e in
      let t = synthtype (g,d) e in
      let e' = beta d e in
      print_endline (x ^ " : " ^ pretty t); 
      print_string "\n";
      (g++(x,t),d++(x,e'))
    ) (Context.empty,Context.empty)


  let rec repl (g,d) =
    try
    print_string "-- ";
    let s = Stdlib.read_line () in
    if s = "" then repl (g,d) else
    match parse cmd s with
      | EXP e ->
          begin
          let e = bind_up e in
          let t = synthtype (g,d) e in
          let e' = beta d e in
          print_endline ("_ : " ^ pretty t);
          print_endline ("_ = " ^ pretty e');
          print_string "\n";
          repl (g,d)
          end
      | DEC (x,e) ->
          let e = bind_up e in
          let t = synthtype (g,d) e in
          let e' = beta d e in
          let (g',d') = (g++(x,t),d++(x,e')) in
          print_endline (x ^" : " ^ pretty t);
          print_endline (x ^ " = " ^ pretty e');
          print_string "\n";
          repl (g',d')
    with | ParseError -> print_endline "Parse Error"; repl (g,d)
         | TypeError e -> print_endline ("Type Error: "^e); repl (g,d)
end


let read_file f =
  let ch = open_in f in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let parse_theory f =
  let s = read_file f in
  match pragmas % s with
    | None -> raise (Failure "Missing SORTS, AXIOMS, or RULES")
    | Some ((sorts,axioms,rules),rest) ->
        let theory = 
          (module struct 
             let sorts = sorts
             let axioms = axioms
             let rules = rules
           end : Pure.THEORY)
        in (theory,rest)


let _ =
  if Array.length Sys.argv < 2 then
  print_endline "Please provide file with SORTS, AXIOMS, RULES and optional definitions"
  else 
  let file = Sys.argv.(1) in
  let (theory,txt) = parse_theory file in
  let module T = (val theory : Pure.THEORY) in
  let module Repl = MkRepl (T) in
  try
  let ds = parse' Repl.prgm txt in
  let (g,d) = Repl.fold_decs ds in
  Repl.repl (g,d)
  with | Repl.S.TypeError s -> print_endline ("Type Error: "^s)



