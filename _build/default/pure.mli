
type term =
  | F of string
  | B of int
  | APP of term * term
  | ABS of (string * term) * term
  | PI of (string * term) * term
  | SORT of string

val alpha_eq : term * term -> bool

module Context : Map.S with type key = string

val (++) : 'a Context.t -> (string * 'a) -> 'a Context.t

val to_string : term -> string

