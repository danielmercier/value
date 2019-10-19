module Make (Value : Abstract_values.S) : sig
  val eval : Exp.t -> Value.value
end
