open Exp

module MyAbstractValues = struct
  type value = Intervals of int * int | Booleans of bool * bool

  let interpretation name values =
    match (name, values) with
    | "+", [Intervals (i1, i2); Intervals (j1, j2)] ->
        Some (Intervals (i1 + j1, i2 + j2))
    | "mk0", [] ->
        Some (Intervals (0, 0))
    | "mk1", [] ->
        Some (Intervals (1, 1))
    | _ ->
        None

  let pp fmt value =
    match value with
    | Intervals (i1, i2) ->
        Format.fprintf fmt "[%i .. %i]" i1 i2
    | Booleans (b1, b2) -> (
      match (b1, b2) with
      | true, true ->
          Format.fprintf fmt "T"
      | false, true ->
          Format.fprintf fmt "True"
      | true, false ->
          Format.fprintf fmt "False"
      | false, false ->
          Format.fprintf fmt "_|_" )
end

module MyEval = Eval.Make (MyAbstractValues)

let () =
  let open MyEval in
  let exp = Call ("+", [Call ("mk1", []); Call ("mk1", [])]) in
  let value = eval exp in
  Format.printf "@[%a =@ %a@]@." Exp.pp exp MyAbstractValues.pp value
