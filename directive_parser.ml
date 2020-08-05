
module Make (P : COMBI.Parser.S) =
struct
module LP = Lang_parser.Make (P)
open LP
open Pure

type directive = 
  | EXP of term 
  | DEC of string * term 


let pair p s = (fun x y -> (x,y)) <$> (p <* s) <*> p
let ax = pair variable (symbol ":")
let rule = pair variable (symbol "~>")
let sorts = pre (symbol "%SORTS" *> sepby1 variable (symbol "|"))
let axioms = pre (symbol "%AXIOMS" *> sepby1 ax (symbol "|"))
let rules = pre (symbol "%RULES" *> sepby1 rule (symbol "|"))

let make rs =
  let (dec,exp) = LP.make rs in
  (((fun (x,y) -> DEC (x,y)) <$> dec) <|> 
   ((fun x -> EXP x) <$> exp) 
  ,dec,exp)


end
