
open Pure
open Dynamics

exception TypeError of string

let rec check_A s = function
  | [] -> raise (TypeError "")
  | (s1,s2)::ss -> if s = s1 then SORT s2 else check_A s ss

let check_S (s1,s2) r = if List.mem (s1,s2) r then SORT s2 else raise (TypeError "")

let beta = beta Context.empty

let synthtype (aa,ss) = 
  let rec synth g = function
    | F x -> (try Context.find x g with _ -> raise (TypeError "Unbound Variable"))
    | B _ -> raise (Failure "whoops")
    | SORT s -> check_A s aa
    | ABS ((x,t),e) ->
        let (f,e') = unbind x e in
        (*print_endline ("("^f^":"^pretty t^")");*)
        let t' = synth (g++(f,t)) e' in
        let r = PI ((f,t),bind f t') in
        let _ = synth g r in
        r
    | PI ((x,t),e) ->
        let (f,e') = unbind x e in
        begin
        match beta (synth g t) with
          | SORT s1 ->  
              begin 
              (*print_endline ("("^f^":"^pretty t^")");*)
              match beta (synth (g++(f,t)) e') with
                | SORT s2 -> check_S (s1,s2) ss
                | _ -> raise (TypeError "")
              end
          | _ -> raise (TypeError "")
        end
    |  APP (m,n) ->
        match (beta (synth g m),beta (synth g n)) with
          | (PI ((x,t),e),t') -> if alpha_eq (t,t') then subst x n e else raise (TypeError "")
          | _ -> raise (TypeError "")
  in synth
