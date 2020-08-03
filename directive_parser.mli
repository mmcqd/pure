
type directive = 
  | EXP of Pure.term 
  | DEC of string * Pure.term 

val make : string list -> directive Lang_parser.t 
