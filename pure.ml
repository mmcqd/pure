
type term =
  | F of string
  | B of int
  | APP of term * term
  | ALAM of (string * term) * term
  | LAM of string * term
  | PI of (string * term) * term
  | SORT of string
  | ANNOT of term * term


let rec alpha_eq = function
  | F x, F y -> x = y
  | B i, B j -> i = j
  | APP (m,n), APP (m',n') -> alpha_eq (m,m') && alpha_eq (n,n')
  | SORT s, SORT s' -> s = s'
  | ALAM ((_,t),e), ALAM ((_,t'),e') -> alpha_eq (t,t') && alpha_eq (e,e')
  | LAM (_,e), LAM (_,e') -> alpha_eq (e,e')
  | PI ((_,t),e), PI ((_,t'),e') -> alpha_eq (t,t') && alpha_eq (e,e')
  | ANNOT (e,t), ANNOT (e',t') -> alpha_eq (e,e') && alpha_eq (t,t')
  | _ -> false

module Context = Map.Make (String)
let (++) g (x,t) = Context.add x t g 

let rec to_string = function
  | F x -> "F " ^ x
  | B i -> "B " ^ Int.to_string i
  | APP (m,n) -> "APP ("^to_string m^") ("^to_string n^")"
  | ALAM ((x,t),e) -> "LAM ("^x^": "^to_string t^") ("^to_string e^")"
  | LAM (x,e) -> "LAM ("^x^") "^to_string e
  | PI ((x,t),e) -> "PI ("^x^": "^to_string t^") ("^to_string e^")"
  | SORT s -> "SORT "^s
  | ANNOT (e,t) -> "("^to_string e^" : "^to_string t^")"
 
module type THEORY = 
  sig
    val sorts : string list
    val axioms : (string * string) list 
    val rules : (string * string * string) list
  end
