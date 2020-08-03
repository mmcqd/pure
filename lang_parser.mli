
include module type of COMBI.Parser.Make (COMBI.Parser.ListBase)

val make : string list -> (string * Pure.term) parser * Pure.term parser 
