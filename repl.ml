open Dynamics
open Statics
module DP = Directive_parser.Make (COMBI.Parser.Make (COMBI.Parser.OptionBase))
open DP
open LP
open Pure

exception ParseError

let default_sorts = ["Prop";"Type"]
let default_rules = ([("Prop","Type")],
             [("Prop","Prop");("Type","Prop");("Type","Type");("Prop","Type")])

let (dir_parser,dec_parser,exp_parser) = DP.make default_sorts

(*
let parse_opt p s = 
   Option.map fst @@ List.find_opt (function (_,[]) -> true | _ -> false) (p % s)
*)

let parse p s = 
  match p % s with
    | Some (t,[]) -> t
    | _ -> raise ParseError

let parse' p s =
  match p s with
    | Some (t,[]) -> t
    | _ -> raise ParseError


let read_file f =
  let ch = open_in f in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let fold_decs rules = 
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
  match sorts % s with
    | None -> raise (Failure "Please specify sorts")
    | Some (ss,t) ->
        match axioms t with
          | None -> raise (Failure "Please specify axioms")
          | Some (ax,t) ->
            match rules t with
              | None -> raise (Failure "Please specify rules")
              | Some (rs,t) ->
                  let (dir,dec,_) = DP.make ss in
                    let ds = parse' (many dec) t in
                    (fold_decs (ax,rs) ds, dir, (ax,rs))


let repl rules dir_parser =
  let rec loop (g_dyn,g_stat) =
    try
    print_string "-- ";
    let s = Stdlib.read_line () in
    if s = "" then loop (g_dyn,g_stat) else
    match parse dir_parser s with
      | EXP e ->
          begin
          let e = bind_up e in
          let t = synthtype rules (beta g_dyn) g_stat e in
          let e' = beta g_dyn e in
          print_endline ("_ : " ^ pretty t);
          print_endline ("_ = " ^ pretty e');
          print_string "\n";
          loop (g_dyn,g_stat)
          end
      | DEC (x,e) ->
          let e = bind_up e in
          let t = synthtype rules (beta g_dyn) g_stat e in
          let e' = beta g_dyn e in
          let (g_dyn',g_stat') = (g_dyn++(x,e'),g_stat++(x,t)) in
          print_endline (x ^" : " ^ pretty t);
          print_endline (x ^ " = " ^ pretty e');
          print_string "\n";
          loop (g_dyn',g_stat')
    with | ParseError -> print_endline "Parse Error"; loop (g_dyn,g_stat)
         | TypeError e -> print_endline ("Type Error: "^e); loop (g_dyn,g_stat)
  in loop
  
let _ = if Array.length Sys.argv > 1 then 
        let file = Sys.argv.(1) in
        let (gs, dir, rs) = parse_file file in
        repl rs dir gs
        else repl default_rules dir_parser (Context.empty, Context.empty)
