open Lang_parser
open Pure

type directive = 
  | EXP of term 
  | DEC of string * term 


let make rules =
  let (dec,exp) = Lang_parser.make rules in
  ((fun (x,y) -> DEC (x,y)) <$> dec) <|> ((fun x -> EXP x) <$> exp) 




