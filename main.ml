open Exp

let () =
  let exp = Call ("+", [Call ("mk1", []); Call ("mk1", [])]) in
  Format.printf "%a@." Exp.pp exp
