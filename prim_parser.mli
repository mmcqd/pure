
module type S = 
  sig
    include COMBI.Parser.S
    exception ParseError
    val pragmas : (string list * (string * string) list * (string * string) list) parser
    val ignore : unit parser
    val paren : 'a parser -> 'a parser
    val pre : 'a parser -> 'a parser
    val post : 'a parser -> 'a parser
    val symbol : string -> string parser
    val variable : string parser
    val bind_fold : ('a * 'b -> 'b) -> 'a list -> 'b -> 'b
    val annotated_bind_fold : (('a * 'b) * 'c -> 'c) -> 'a list * 'b -> 'c -> 'c
    val multi_bind : (('a * 'b) * 'c -> 'c) -> ('a list * 'b) list -> 'c -> 'c 
  end 

module Make (P : COMBI.Parser.S) : S with type 'a m = 'a P.m
