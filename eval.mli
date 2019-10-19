module Make (Value : Abstract_values.S) : sig
  type eval = Eval : ('a, 'a) Value.typ * 'a -> eval

  type _ eval_list =
    | EvalList : ('a, 'r) Value.typ * ('a, 'r) Utils.hlist -> 'r eval_list

  val eval : Exp.t -> eval
end
