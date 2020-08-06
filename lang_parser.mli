

module Make (P : Prim_parser.S) (T : Pure.THEORY) :
  sig
    type cmd =
      | EXP of Pure.term
      | DEC of string * Pure.term

    val exp : Pure.term P.parser
    val dec : (string * Pure.term) P.parser 
    val prgm : (string * Pure.term) list P.parser
    val cmd : cmd P.parser

  end
