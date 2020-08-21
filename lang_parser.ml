
module Make (P : Prim_parser.S) (T : Pure.THEORY) =
struct

open Pure
open P

let arrow = symbol "->" <|> symbol "→" 
let forall = symbol "\\/" <|> symbol "∀" <|> symbol "Π"
let lambda = symbol "\\" <|> symbol "λ"

let rec expr i = chainr1 expr1 (arrow *> return (fun t1 t2 -> PI (("",t1),t2))) i
  
    and expr1 i = chainl1 expr2 (return (fun m n -> APP (m,n))) i

    and expr2 i = (annot <|> sort <|> var <|> lam <|> alam <|> pi <|> (fun i -> paren expr i)) i

  
    and annot i = paren ((fun e t -> ANNOT (e,t)) <$> 
                         expr <*>
                         symbol ":" *>
                         expr) i

    and sort i = ((fun s -> SORT s) <$> 
                  let* v = variable in 
                  if List.mem v T.sorts
                  then return v 
                  else fail) i
  
    and var i = ((fun v -> F v) <$> variable) i

    and lam i = (bind_fold (fun (x,e) -> LAM (x,e)) <$>
                lambda *> 
                paren (many1 variable) <*>
                expr) i

    and alam i = (multi_bind (fun (x,e) -> ALAM (x,e)) <$> 
                 lambda *> 
                 many1 (paren args) <*> 
                 expr) i
    

    and pi i = (multi_bind (fun (x,e) -> PI (x,e)) <$> 
               forall *>
               many1 (paren args) <*>
               expr) i
     
    and args i = ((fun xs t -> xs,t) <$> 
                 many1 variable <*>
                 symbol ":" *>
                 expr) i

let plain_dec i = ((fun x y -> (x,y)) <$> 
                  symbol "let" *> 
                  variable <*>
                  symbol "=" *>
                  expr) i

let annot_dec i = ((fun x t e -> (x,ANNOT (e,t))) <$>
                  symbol "let" *> 
                  variable <*>
                  symbol ":" *> 
                  expr <*>
                  symbol "=" *> 
                  expr) i

let dec = pre (annot_dec <|> plain_dec)
let exp = pre expr
let prgm = many dec

type cmd = 
  | EXP of term 
  | DEC of string * term 

let cmd = ((fun (x,y) -> DEC (x,y)) <$> dec) <|> ((fun x -> EXP x) <$> exp) 
end
