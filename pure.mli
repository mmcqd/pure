
type term =
  | F of string
  | B of int
  | APP of term * term
  | ALAM of (string * term) * term
  | LAM of string * term
  | PI of (string * term) * term
  | SORT of string
  | ANNOT of term * term

val alpha_eq : term * term -> bool

module Context : Map.S with type key = string

val (++) : 'a Context.t -> (string * 'a) -> 'a Context.t

val to_string : term -> string

module type THEORY = 
  sig
    val sorts : string list
    val axioms : (string * string) list 
    val rules : (string * string * string) list
  end
