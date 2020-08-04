open Dynamics
open Statics
module DP = Directive_parser.Make (COMBI.Parser.Make (COMBI.Parser.OptionBase))
open DP
open LP
open Pure

exception ParseError

let sorts = ["Prop";"Type"]
let rules = ([("Prop","Type")],
             [("Prop","Prop");("Type","Prop");("Type","Type");("Prop","Type")])

let (dir_parser,dec_parser,exp_parser) = DP.make sorts

(*
let parse_opt p s = 
   Option.map fst @@ List.find_opt (function (_,[]) -> true | _ -> false) (p % s)
*)
let parse_opt p s =
  match p % s with
    | Some (x,[]) -> Some x
    | _ -> None

let parse p s = 
  match parse_opt p s with
    | Some t -> t
    | None -> raise ParseError

let file_parser = many1 dec_parser

let read_file f =
  let ch = open_in f in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let fold_decs = 
  List.fold_left 
  (fun (g_dyn,g_stat) (x,e) ->
    let e = bind_up e in
    let t = synthtype rules (beta g_dyn) g_stat e in
    let e' = beta g_dyn e in
    print_endline (x ^ " : " ^ pretty t);
    print_string "\n";
    (g_dyn++(x,e'),g_stat++(x,t))
  ) (Context.empty,Context.empty)
    
let parse_file f =
  let s = read_file f in
  let ds = parse file_parser s in
  fold_decs ds


let rec repl (g_dyn,g_stat) =
  try
  print_string "-- ";
  let s = Stdlib.read_line () in
  if s = "" then repl (g_dyn,g_stat) else
  match parse dir_parser s with
    | EXP e ->
        begin
        let e = bind_up e in
        let t = synthtype rules (beta g_dyn) g_stat e in
        let e' = beta g_dyn e in
        print_endline ("_ : " ^ pretty t);
        print_endline ("_ = " ^ pretty e');
        print_string "\n";
        repl (g_dyn,g_stat)
        end
    | DEC (x,e) ->
        let e = bind_up e in
        let t = synthtype rules (beta g_dyn) g_stat e in
        let e' = beta g_dyn e in
        let (g_dyn',g_stat') = (g_dyn++(x,e'),g_stat++(x,t)) in
        print_endline (x ^" : " ^ pretty t);
        print_endline (x ^ " = " ^ pretty e');
        print_string "\n";
        repl (g_dyn',g_stat')
  with | ParseError -> print_endline "Parse Error"; repl (g_dyn,g_stat)
       | TypeError e -> print_endline ("Type Error: "^e); repl (g_dyn,g_stat)
  
let _ = if Array.length Sys.argv > 1 then 
        let file = Sys.argv.(1) in
        let gs = parse_file file in
        repl gs
        else repl (Context.empty, Context.empty)
