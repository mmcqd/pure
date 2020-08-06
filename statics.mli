
module Make (T : Pure.THEORY) :
  sig
    exception TypeError of string

    val synthtype : Pure.term Pure.Context.t * Pure.Term Pure.Context.t -> Pure.term -> Pure.term
  end
