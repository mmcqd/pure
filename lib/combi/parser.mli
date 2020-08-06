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
  val whitespace : char parser
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
  val ident : char list -> string parser
  val (%) : 'a parser -> string -> ('a * char list) m
end

module OptionBase : BASE with type 'a t = 'a option

module ListBase : BASE with type 'a t = 'a list

module Make (M : BASE) : S with type 'a m = 'a M.t


