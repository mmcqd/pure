

module Make (P : COMBI.Parser.S) :
  sig
    include COMBI.Parser.S
    val make : string list -> (string * Pure.term) parser * Pure.term parser 
  end with type 'a m = 'a P.m
