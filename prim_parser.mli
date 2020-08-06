

module Make (P : COMBI.Parser.S) :
  sig
    include COMBI.Parser.S
    exception ParseError
    val pragmas : string list * (string * string) list * (string * string) list parser
  end with type 'a m = 'a P.m
