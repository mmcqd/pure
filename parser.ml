open Pure
open COMBI.Parser.Make (COMBI.Parser.ListBase)

let whitespace = sat (fun c -> c = ' ' || c = '\n' || c = '\t')
let ignore = consume @@ many whitespace
let pre p = ignore *> p
let post p = p <* ignore

let symbol s = post (string s)

let variable = post @@ to_string @@ many1 letter

let paren = between (symbol "(") (symbol ")") 

let mkParser sorts =

  let rec expr i = chainr1 expr1 (symbol "->" *> return (fun t1 t2 -> PI (("_",t1),t2))) i
    
      and expr1 i = chainl1 expr2 (return (fun m n -> APP (m,n))) i

      and expr2 i = (sort <|> var <|> abs <|> pi <|> (fun i -> paren expr i)) i

      and sort i = ((fun s -> SORT s) <$> List.fold_right (<|>) (List.map symbol sorts) fail) i
    
      and var i = ((fun v -> F v) <$> variable) i

      and abs i = ((fun x t e -> ABS ((x,t),e)) <$> 
                    (symbol "\\(" *> variable <* symbol ":") <*> 
                    (expr <* symbol ")") <*> 
                    expr) i

      and pi i = ((fun x t e -> PI ((x,t),e)) <$> 
                   (symbol "\\/(" *> variable <* symbol ":") <*> 
                   (expr <* symbol ")") <*> 
                   expr) i

  in 
  fun s -> Option.map fst @@ List.find_opt (function (_,[]) -> true | _ -> false) (pre expr % s)



