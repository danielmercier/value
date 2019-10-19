open Exp
open Utils

module MyAbstractValues = struct
  type intervals = int * int

  type booleans = bool * bool

  type (_, _) typ =
    | Integer : (intervals, intervals) typ
    | Boolean : (booleans, booleans) typ
    | Fun : ('a, 'a) typ * ('b, 'r) typ -> ('a -> 'b, 'r) typ

  let ( @-> ) a b = Fun (a, b)

  type any_typ = Any : ('a, 'a) typ -> any_typ

  let typer = function
    | Call (("+" | "mk0" | "mk1"), _) ->
        Any Integer
    | _ ->
        assert false

  type (_, _) signature =
    | Function : string * ('a, 'r) typ -> ('a, 'r) signature

  let interpretation : type a r. (a, r) signature -> ((a, r) hlist -> r) option
      = function
    | Function ("+", Fun (Integer, Fun (Integer, Integer))) ->
        Some (fun [(i1, i2); (j1, j2)] -> (i1 + j1, i2 + j2))
    | Function ("mk0", Integer) ->
        Some (fun [] -> (0, 0))
    | Function ("mk1", Integer) ->
        Some (fun [] -> (1, 1))
    | _ ->
        None

  let pp : type a. Format.formatter -> (a, a) typ * a -> unit =
   fun fmt (typ, value) ->
    match (typ, value) with
    | Integer, (i1, i2) ->
        Format.fprintf fmt "[%i .. %i]" i1 i2
    | Boolean, (b1, b2) -> (
      match (b1, b2) with
      | true, true ->
          Format.fprintf fmt "T"
      | false, true ->
          Format.fprintf fmt "True"
      | true, false ->
          Format.fprintf fmt "False"
      | false, false ->
          Format.fprintf fmt "_|_" )
    | _ ->
        assert false
end

module MyEval = Eval.Make (MyAbstractValues)

let () =
  let open MyEval in
  let exp = Call ("+", [Call ("mk1", []); Call ("mk1", [])]) in
  let (Eval (typ, value)) = eval exp in
  Format.printf "@[%a =@ %a@]@." Exp.pp exp MyAbstractValues.pp (typ, value)
