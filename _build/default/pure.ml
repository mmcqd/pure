
type term =
  | F of string
  | B of int
  | APP of term * term
  | ABS of (string * term) * term
  | PI of (string * term) * term
  | SORT of string


let rec alpha_eq = function
  | (F x, F y) -> x = y
  | (B i, B j) -> i = j
  | (APP (m,n), APP (m',n')) -> alpha_eq (m,m') && alpha_eq (n,n')
  | (SORT s, SORT s') -> s = s'
  | (ABS ((_,t),e), ABS ((_,t'),e')) -> alpha_eq (t,t') && alpha_eq (e,e')
  | (PI ((_,t),e), PI ((_,t'),e')) -> alpha_eq (t,t') && alpha_eq (e,e')
  | _ -> false

module Context = Map.Make (struct include String end)
let (++) g (x,t) = Context.add x t g 

let rec to_string = function
  | F x -> "F " ^ x
  | B i -> "B " ^ Int.to_string i
  | APP (m,n) -> "APP ("^to_string m^") ("^to_string n^")"
  | ABS ((x,t),e) -> "ABS ("^x^": "^to_string t^") ("^to_string e^")"
  | PI ((x,t),e) -> "PI ("^x^": "^to_string t^") ("^to_string e^")"
  | SORT s -> "SORT "^s
 
