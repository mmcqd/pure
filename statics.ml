
open Pure
open Dynamics

exception TypeError of string

let rec check_A s = function
  | [] -> raise (TypeError ("The sort '"^s^"' has no type. Check axioms?"))
  | (s1,s2)::ss -> if s = s1 then SORT s2 else check_A s ss

let check_S (s1,s2) r = 
  if List.mem (s1,s2) r then SORT s2 
  else raise (TypeError 
  ("Illegal Pi Type: value of type '"^s2^"' cannot depend on value of type '"^s1^"'"))


let go = Fun.const ()

let synthtype (aa,ss) beta =
  let rec synth g = function
        | SORT s -> check_A s aa
        | F x -> 
            begin
            match Context.find_opt x g with 
              | Some a -> go (try synth g a with TypeError s -> raise (TypeError (s^" Caused by: '"^pretty a^"'"))); a 
              | _ -> raise (TypeError ("Unbound var: '"^x^"'"))
            end
        | B _ -> raise (Failure "Should never be type checking a bound var")
        | ANNOT (e,t) -> check g (e,t)
        | PI ((x,t),e) -> 
            let (f,e') = unbind x e in
            begin
            match beta @@ synth g t with
              | SORT s1 ->
                  begin
                  match beta @@ synth (g ++ (f,t)) e' with
                    | SORT s2 -> go @@ check_S (s1,s2) ss; SORT s2
                    | x -> raise (TypeError ("'"^pretty x^"' must be a sort"))
                    end
              | x -> raise (TypeError ("'"^pretty x^"' must be a sort"))
            end
        | APP (m,n) -> 
            begin
            match beta @@ synth g m with
              | PI ((x,t),e) -> let (f,e') = unbind x e in
                                go @@ check g (n,t); subst f n e'
              | t -> raise (TypeError ("'"^pretty m^"' has type '"^pretty t^"'. It is not a function, it cannot be applied"))
            end
        | ALAM ((x,t),e) ->
            let (f,e') = unbind x e in
            let b = synth (g ++ (f,t)) e' in
            let r = PI ((x,t),bind f b) in
            go @@ synth g r; r

        | x -> raise (TypeError ("Cannot infer type for: '"^pretty x^"'"))
                              

      and check g = function
        | LAM (x,e), (PI ((y,a),b) as t) -> let (f,e') = unbind x e in
                                            let (h,b') = unbind y b in
                                            go @@ check (g++(f,a)++(h,a)) (e',b'); go @@ synth g t; t
        | m,b -> go @@ synth g b; let a = synth g m in if alpha_eq (beta a, beta b) then b 
                 else raise (TypeError ("'"^pretty a^"' does not equal '"^pretty b^"'"))
  in synth

(*
let synthtype (aa,ss) beta = 
  let rec synth g = function
    | F x -> (try Context.find x g with _ -> raise (TypeError ("Unbound Variable: '"^x^"'")))
    | B _ -> raise (Failure "Should never happen")
    | SORT s -> check_A s aa
    | ABS ((x,t),e) ->
        let (f,e') = unbind x e in
        let t' = synth (g++(f,t)) e' in
        let r = PI ((x,t),bind f t') in
        let _ = synth g r in
        r
    | PI ((x,t),e) ->
        let (f,e') = unbind x e in
        begin
        match beta (synth g t) with
          | SORT s1 ->  
              begin 
              match beta (synth (g++(f,t)) e') with
                | SORT s2 -> check_S (s1,s2) ss
                | _ -> raise (TypeError "Pi cannot produce term with non-sort type")
              end
          | _ -> raise (TypeError "Pi cannot bind variable with non-sort type")
        end
    |  APP (m,n) ->
        match (beta (synth g m),beta (synth g n)) with
          | (PI ((x,t),e),t') ->
              let (f,e') = unbind x e in 
              if alpha_eq (t,t') then subst f n e' 
              else raise (TypeError 
              ("Expected '"^pretty n^"' to have type '"^pretty t^"' but it has type '"^pretty t'^"' in expression: '"^pretty (APP (m,n))^"'"))
          
          | (t,_) -> raise (TypeError ("'"^pretty m^"' has type '"^pretty t^"'. It is not a function, it cannot be applied"))
  in synth
*)
