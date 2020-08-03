

val unbind : string -> Pure.term -> string * Pure.term
val bind : string -> Pure.term -> Pure.term
val subst : string -> Pure.term -> Pure.term -> Pure.term
val beta : Pure.term Pure.Context.t -> Pure.term -> Pure.term
val bind_up : Pure.term -> Pure.term
val pretty : Pure.term -> string


