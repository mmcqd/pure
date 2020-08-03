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

let _ = synthtype
let _ = beta

let _ = print_endline (pretty (synthtype rules Context.empty (parse "\\(y:\\/(x:*) x -> x) y")))

