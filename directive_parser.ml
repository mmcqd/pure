
module Make (P : COMBI.Parser.S) =
struct
module LP = Lang_parser.Make (P)
open LP
open Pure

type directive = 
  | EXP of term 
  | DEC of string * term 


let make rules =
  let (dec,exp) = LP.make rules in
  (((fun (x,y) -> DEC (x,y)) <$> dec) <|> ((fun x -> EXP x) <$> exp),dec,exp)


end
