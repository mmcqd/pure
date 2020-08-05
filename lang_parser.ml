
module Make (P : COMBI.Parser.S) =
struct

open Pure
include P

let whitespace = sat (fun c -> c = ' ' || c = '\n' || c = '\t')
let ignore = consume @@ many whitespace
let pre p = ignore *> p
let post p = p <* ignore

let symbol s = post (string s)

let illegal_chr = ['\\';'(';')';':';' ';'\t';'\n';'%']
let ident = to_string @@ many1 (sat (fun x -> not (List.mem x illegal_chr)))

let illegal_str = ["let";"->"]
let variable = post (ident >>= (fun v -> if List.mem v illegal_str then fail else return v))

let paren x = between (symbol "(") (symbol ")") x

let bind_fold c (xs,t) = List.fold_right (fun x e' -> c ((x,t),e')) xs

let multi_bind c = List.fold_right (bind_fold c)

let make sorts =

  let rec expr i = chainr1 expr1 (symbol "->" *> return (fun t1 t2 -> PI (("",t1),t2))) i
    
      and expr1 i = chainl1 expr2 (return (fun m n -> APP (m,n))) i

      and expr2 i = (sort <|> var <|> alam <|> pi <|> (fun i -> paren expr i)) i

      and sort i = ((fun s -> SORT s) <$> List.fold_right (<|>) (List.map symbol sorts) fail) i
    
      and var i = ((fun v -> F v) <$> variable) i
 
      and alam i = (multi_bind (fun (x,e) -> ALAM (x,e)) <$> 
                   (symbol "\\" *> many1 (paren args)) <*> 
                   expr) i
      

      and pi i = (multi_bind (fun (x,e) -> PI (x,e)) <$> 
                   (symbol "\\/" *> many1 (paren args)) <*>
                   expr) i
       
      and args i = ((fun xs t -> (xs,t)) <$> 
                    (many1 variable <* symbol ":") <*> expr) i

  in let decl i = ((fun x y -> (x,y)) <$> 
               (symbol "let" *> variable <* symbol "=") <*>
               expr) i
  in (pre decl, pre expr)

end
