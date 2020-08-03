
module type BASE =
sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val fail : 'a t
  val choice : 'a t -> 'a t -> 'a t
end


module type S =
sig
  type 'a m
  type 'a t = char list -> ('a * char list) m
  type 'a parser = 'a t
  include BASE with type 'a t := 'a parser

  val (<$>) : ('a -> 'b) -> 'a parser -> 'b parser
  val (<$) : 'a -> 'b parser -> 'a parser
  val ($>) : 'a parser -> 'b -> 'b parser
  val (<*>) : ('a -> 'b) parser -> 'a parser -> 'b parser
  val (<* ) : 'a parser -> 'b parser -> 'a parser
  val ( *>) : 'a parser -> 'b parser -> 'b parser
  val lift2 : ('a -> 'b -> 'c) -> 'a parser -> 'b parser -> 'c parser
  val (>>=) : 'a parser -> ('a -> 'b parser) -> 'b parser
  val (<|>) : 'a parser -> 'a parser -> 'a parser

  val (let*) : 'a parser -> ('a -> 'b parser) -> 'b parser
  val item : char parser
  val sat : (char -> bool) -> char parser
  val letter : char parser
  val lower : char parser
  val upper : char parser
  val digit : char parser
  val alphanum : char parser
  val char : char -> char parser
  val char_list : char list -> char list parser
  val string : string -> string parser
  val to_string : char list parser -> string parser
  val nat : int parser
  val int : int parser
  val many : 'a parser -> 'a list parser
  val many1 : 'a parser -> 'a list parser
  val sepby : 'a parser -> 'sep parser -> 'a list parser
  val sepby1 : 'a parser -> 'sep parser -> 'a list parser
  val between : 'dl parser -> 'dr parser -> 'a parser -> 'a parser
  val chainl : 'a parser -> ('a -> 'a -> 'a) parser -> 'a -> 'a parser
  val chainl1 : 'a parser -> ('a -> 'a -> 'a) parser -> 'a parser
  val chainr : 'a parser -> ('a -> 'a -> 'a) parser -> 'a -> 'a parser
  val chainr1 : 'a parser -> ('a -> 'a -> 'a) parser -> 'a parser
  val postfix : 'a parser -> ('a -> 'a) parser -> 'a parser
  val prefix : ('a -> 'a) parser -> 'a parser -> 'a parser
  val consume : 'a parser -> unit parser
  val (%) : 'a parser -> string -> ('a * char list) m
end


module OptionBase : BASE with type 'a t = 'a option =
struct
  type 'a t = 'a option
  let return x = Some x
  let bind x f =
    match x with
      | Some x -> f x
      | _      -> None

  let fail = None
  let choice x y =
    match (x,y) with
      | (Some x,_) -> Some x
      | (_,Some y) -> Some y
      | _          -> None
end

module ListBase : BASE with type 'a t = 'a list =
struct
  type 'a t = 'a list
  let return x = [x]

  let bind x f = List.concat (List.map f x)

  let fail = []

  let choice = (@)
end

module Make (M : BASE) : S
with type 'a m = 'a M.t =
struct
  type 'a m = 'a M.t
  type 'a t = char list -> ('a * char list) M.t
  type 'a parser = 'a t


  let uncurry f (x,y) = f x y
  let cons x xs = x::xs
  let foldr f z xs = List.fold_right f xs z
  let foldl = List.fold_left
  let explode s = List.init (String.length s) (String.get s)
  let implode = foldr (fun x y -> Char.escaped x ^ y) ""

  let return x i = M.return (x,i)

  let bind p f i =
    M.bind (p i) (uncurry f)

  let fail _ = M.fail

  let choice p q i = M.choice (p i) (q i)

  let (<|>) = choice

  let (>>=) = bind
  let (let*) = bind

  let map f p =
    let* x = p in
    return @@ f x

  let (<$>) = map

  let (<$) x = map @@ Fun.const x
  let ($>) x = Fun.flip (<$) @@ x

  let apply p q =
    let* f = p in
    let* x = q in
    return @@ f x

  let (<*>) = apply

  let ( <* ) p q =
    let* x = p in
    let* _ = q in
    return x

  let ( *> ) p q =
    let* _ = p in
    let* y = q in
    return y

  let lift2 f p q = f <$> p <*> q

  let item = function
    | []      -> fail []
    | (x::xs) -> return x xs

  let sat p =
    let* c = item in
    if p c then return c else fail

  let in_range (x,y) c = x <= c && c <= y

  let digit = sat @@ in_range ('0','9')
  let lower = sat @@ in_range ('a','z')
  let upper = sat @@ in_range ('A','Z')

  let letter = lower <|> upper
  let alphanum = letter <|> digit

  let char c = sat (fun x -> x = c)

  let rec char_list = function
    | []    -> return []
    | c::cs -> lift2 cons (char c) (char_list cs)

  let string s = implode <$> (char_list (explode s))

  let rec many p = lift2 cons p (fun x -> many p x) <|> return []
  let many1 p = lift2 cons p (many p)

  let nat =
    let toNum c = Char.code c - Char.code '0' in
    let eval = foldl (fun i c -> toNum c + 10*i) 0 in
    eval <$> many1 digit

  let int =
    let p = (char '-' $> Int.neg) <|> return Fun.id in
    p <*> nat

  let to_string p = implode <$> p

  let sepby1 p sep = lift2 cons p (many @@ sep *> p)
  let sepby p sep = sepby1 p sep <|> return []

  let between l r p = l *> p <* r

  let chainl1 t o =
    let rec rest x =
      (let* f = o in
       let* y = t in
       rest @@ f x y)
     <|> return x
    in
      t >>= rest

  let chainl t o v =
    chainl1 t o <|> return v

  let rec chainr1 t o =
    let* x = t in
    (let* f = o in
     let* y = chainr1 t o in
     return @@ f x y) <|> return x

  let chainr t o v =
    chainr1 t o <|> return v

  let postfix p o =
    let rec rest x =
      (let* f = o in
       rest @@ f x) <|> return x
    in
      p >>= rest

  let prefix o p =
    lift2 (foldr (@@)) p (many o)

  let consume p = () <$ p

  let (%) p s = p (explode s)
end


