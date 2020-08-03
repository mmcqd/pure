open Dynamics
open Statics
open Lang_parser
open Directive_parser
open Pure

exception ParseError

let sorts = ["Prop";"Type"]
let rules = ([("Prop","Type")],
             [("Prop","Prop");("Type","Prop");("Type","Type");("Prop","Type")])

let dir_parser = Directive_parser.make sorts

let parse_opt p s = 
   Option.map fst @@ List.find_opt (function (_,[]) -> true | _ -> false) (p % s)

let parse p s = 
  match parse_opt p s with
    | Some t -> t
    | None -> raise ParseError



let rec repl (g_dyn,g_stat) =
  print_string ">>> ";
  reset_var_stream ();
  let s = Stdlib.read_line () in
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
    

let _ = repl (Context.empty, Context.empty)
