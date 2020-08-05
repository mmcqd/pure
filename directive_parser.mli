

module Make (P : COMBI.Parser.S) :
  sig
    module LP : module type of Lang_parser.Make(P)
    type directive = 
      | EXP of Pure.term 
      | DEC of string * Pure.term 
  
    val sorts : string list LP.t
    val axioms : (string * string) list LP.t
    val rules : (string * string) list LP.t

    val make : string list -> 
               directive LP.t * 
               (string * Pure.term) LP.t *
               Pure.term LP.t

  end
