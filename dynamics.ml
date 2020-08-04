
open Pure

let r = ref 0

let fresh v = (r := !r + 1; v ^ Int.to_string (!r))

let reset_var_stream () = r := 0

let instantiate y = 
  let rec replace b = function
  | F x -> F x
  | B i when i = b -> y
  | B i -> B i
  | APP (m,n) -> APP (replace b m, replace b n)
  | ABS ((x,t),e) -> ABS ((x,replace b t), replace (b + 1) e)
  | PI  ((x,t),e) -> PI ((x,replace b t),replace (b + 1) e)
  | SORT s -> SORT s
  in replace 0

let unbind x e = let f = fresh x in (f,instantiate (F f) e)

let bind y = 
  let rec replace b = function
    | F x when x = y -> B b
    | F x -> F x
    | B i -> B i
    | APP (m,n) -> APP (replace b m, replace b n)
    | ABS ((x,t),e) -> ABS ((x,replace b t),replace (b+1) e)
    | PI ((x,t),e) -> PI ((x,replace b t),replace (b+1) e)
    | SORT s -> SORT s
  in replace 0

let rec subst y w = function
  | F x when x = y -> w
  | F x -> F x
  | B i -> B i
  | APP (m,n) -> APP (subst y w m,subst y w n)
  | ABS ((x,t),e) -> ABS ((x,subst y w t),subst y w e)
  | PI ((x,t),e) ->  PI ((x,subst y w t),subst y w e)
  | SORT s -> SORT s

let paren s = "("^s^")"

let rec binds y = function
  | B i when i = y -> true
  | B _ -> false
  | F _ -> false
  | APP (m,n) -> binds y m || binds y n
  | ABS ((_,t),e) | PI ((_,t),e) -> binds y t || binds (y+1) e
  | SORT _ -> false



let rec free_in y = function
  | F x when x = y -> true
  | F _ -> false
  | B _ -> false
  | APP (m,n) -> free_in y m || free_in y n
  | ABS ((_,t),e) | PI ((_,t),e) -> free_in y t || free_in y e
  | SORT _ -> false 

let pretty s = reset_var_stream ();
  let rec pretty = function
    | F x -> x
    | B _ -> raise (Failure "Shouldn't be printing bound vars")
    | SORT s -> s
    | APP (m,n) -> 
        begin
        match (m,n) with
          | (_,APP _) | (_, ABS _) | (_,PI _) -> pretty m^" "^paren (pretty n)
          | (ABS _,_) | (PI _, _) -> paren (pretty m)^" "^pretty n
          | _ -> pretty m^" "^pretty n
        end
    | ABS ((x,t),e) -> 
        let x' = if free_in x e then fresh x else x in
        "\\("^x'^":"^pretty t^") "^pretty (instantiate (F x') e)
    | PI  ((x,t),e) ->
        let x' = if free_in x e then fresh x else x in
        let v = if binds 0 e then "\\/("^x'^":"^pretty t^") " else pretty t^" -> " in
        v ^pretty (instantiate (F x') e)
  
  in pretty s

(*
let print_context = Context.iter (fun x t -> print_string ("("^x^":"^pretty t^"),"))
*)

let rec beta g = function
  | F x -> Option.value (Context.find_opt x g) ~default:(F x) 
  | B i -> B i
  | ABS ((x,t),e) -> let (f,e') = unbind x e in 
                     ABS ((x,beta g t),bind f (beta g e'))
  | PI ((x,t),e) -> let (f,e') = unbind x e in 
                    PI ((x,beta g t),bind f (beta g e'))
  | SORT s -> SORT s
  | APP (m,n) ->
      match (beta g m, beta g n) with
        | (ABS ((x,_),e),n') -> let (f,e') = unbind x e in beta (g++(f,n')) e'
        | (m',n') -> APP (m',n')

let rec bind_up = function
  | F x -> F x
  | B i -> B i
  | APP (m,n) -> APP (bind_up m, bind_up n)
  | ABS ((x,t),e) -> ABS ((x,bind_up t), bind x (bind_up e))
  | PI ((x,t),e) -> PI ((x,bind_up t), bind x (bind_up e))
  | SORT s -> SORT s



