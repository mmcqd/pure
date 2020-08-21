module type S = 
  sig
    include COMBI.Parser.S
    exception ParseError
    val pragmas : (string list * (string * string) list * (string * string * string) list) parser
    val ignore : unit parser
    val paren : 'a parser -> 'a parser
    val pre : 'a parser -> 'a parser
    val post : 'a parser -> 'a parser
    val symbol : string -> string parser
    val variable : string parser
    val bind_fold : ('a * 'b -> 'b) -> 'a list -> 'b -> 'b
    val annotated_bind_fold : (('a * 'b) * 'c -> 'c) -> 'a list * 'b -> 'c -> 'c
    val multi_bind : (('a * 'b) * 'c -> 'c) -> ('a list * 'b) list -> 'c -> 'c 
  end 

module Make (P : COMBI.Parser.S) =
struct

include P

exception ParseError
let ignore = consume @@ many whitespace
let pre p = ignore *> p
let post p = p <* ignore

let symbol s = post (string s)

let illegal_chr = ['\\';'(';')';' ';'\t';'\n';'%';',';':';'=']
let illegal_str = ["let";"->";"→";"∀";"λ";"Π"]  
let variable = 
  post (let* v = ident illegal_chr in if List.mem v illegal_str then fail else return v)

let paren x = between (symbol "(") (symbol ")") x

let annotated_bind_fold c (xs,t) = List.fold_right (fun x e' -> c ((x,t),e')) xs

let bind_fold c = List.fold_right (fun x e' -> c (x,e'))

let multi_bind c = List.fold_right (annotated_bind_fold c)


let pair p s = (fun x y -> (x,y)) <$> (p <* s) <*> p
let triple p s = (fun x y z -> (x,y,z)) <$> (p <* s) <*> (p <* s) <*> p
let ax = pair variable (symbol ":")
let rule = triple variable (symbol ",")
let sorts = symbol "%SORTS" *> sepby variable (symbol "|")
let axioms = symbol "%AXIOMS" *> sepby ax (symbol "|")
let rules = symbol "%RULES" *> sepby rule (symbol "|")

let pragmas = pre ((fun x y z -> (x,y,z)) <$> sorts <*> axioms <*> rules)

end
