module type S = sig
  type (_, _) typ

  val ( @-> ) : ('a, 'a) typ -> ('b, 'r) typ -> ('a -> 'b, 'r) typ

  type any_typ = Any : ('a, 'a) typ -> any_typ

  val typer : Exp.t -> any_typ

  type (_, _) signature =
    | Function : string * ('a, 'r) typ -> ('a, 'r) signature

  val interpretation :
    ('a, 'r) signature -> (('a, 'r) Utils.hlist -> 'r) option
end
