

module Make (P : COMBI.Parser.S) (T : Pure.THEORY) :
  sig
    type cmd =
      | EXP of Pure.term
      | DEC of string * Pure.term

    val exp : Pure.term parser
    val dec : (string * Pure.term) parser 
    val cmd : cmd parser

  end with type 'a m = 'a P.m
