open Dynamics
open Statics
open Pure


let t = PI (("a",SORT "*"),PI (("_",B 0),B 1))

let id = ABS (("A",SORT "*"),ABS (("x",B 0),B 0))

let id_t = PI (("A",SORT "*"), PI (("x",B 0), B 1))

let rules = ([("*","BOX")],[("*","*");("BOX","*");("BOX","BOX")])

let _ = synthtype

let _ = print_string (pretty (synthtype rules Context.empty id) ^ "\n")

