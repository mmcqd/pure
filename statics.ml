
module Make (T : Pure.THEORY) =
struct

open Pure
open Dynamics

exception TypeError of string

let rec check_A s = function
  | [] -> raise (TypeError ("The sort '"^s^"' has no type. Check axioms?"))
  | (s1,s2)::ss -> if s = s1 then SORT s2 else check_A s ss

let check_R (s1,s2) r = 
  if List.mem (s1,s2) r then SORT s2 
  else raise (TypeError 
  ("Illegal Pi Type: value of type '"^s2^"' cannot depend on value of type '"^s1^"'"))


let (++) (g,d) kv = (g ++ kv,d)

let go = Fun.const ()

let rec synth ((g,d) as c) = function
      | SORT s -> check_A s T.axioms
      | F x -> 
          begin
          match Context.find_opt x g with 
            | Some a -> go (try synth c a with TypeError s -> raise (TypeError (s^" Caused by: '"^pretty a^"'"))); a 
            | _ -> raise (TypeError ("Unbound var: '"^x^"'"))
          end
      | B _ -> raise (Failure "Should never be type checking a bound var")
      | ANNOT (e,t) -> go @@ synth c t; go @@ check c (e,beta d t);t
      | PI ((x,t),e) -> 
          let (f,e') = unbind x e in
          begin
          match beta d @@ synth c t with
            | SORT s1 ->
                begin
                match beta d @@ synth (c ++ (f,t)) e' with
                  | SORT s2 -> go @@ check_R (s1,s2) T.rules; SORT s2
                  | x -> raise (TypeError ("'"^pretty x^"' must be a sort"))
                  end
            | x -> raise (TypeError ("'"^pretty x^"' must be a sort"))
          end
      | APP (m,n) -> 
          begin
          match beta d @@ synth c m with
            | PI ((x,t),e) -> let (f,e') = unbind x e in
                              go @@ check c (n,t); subst f n e'
            | t -> raise (TypeError ("In expresion '"^pretty (APP (m,n))^"', '"^pretty m^"' has type '"^pretty t^"'. It is not a function, it cannot be applied"))
          end
      | ALAM ((x,t),e) ->
          let (f,e') = unbind x e in
          let b = synth (c ++ (f,t)) e' in
          let r = PI ((x,t),bind f b) in
          go @@ synth c r; r

      | x -> raise (TypeError ("Cannot infer type for: '"^pretty x^"'"))
                            

    and check ((_,d) as c) = function
      | LAM (x,e), (PI ((_,a),b) as t) -> let (f,e') = unbind x e in
                                          let b' = instantiate (F f) b in
                                          go @@ check (c++(f,a)) (e',b'); go @@ synth c t; t
      | m,b -> go @@ synth c b; let a = synth c m in if alpha_eq (beta d a, beta d b) then b 
               else raise (TypeError ("Expected '"^pretty m^"' to have type '"^pretty b^"', but it has type '"^pretty a^"'"))

let synthtype = synth

end
