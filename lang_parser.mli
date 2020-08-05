

module Make (P : COMBI.Parser.S) :
  sig
    include COMBI.Parser.S
    val make : string list -> (string * Pure.term) parser * Pure.term parser 
    val symbol : string -> string parser
    val variable : string parser
    val pre : 'a parser -> 'a parser
  end with type 'a m = 'a P.m
