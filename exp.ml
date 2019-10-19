type t = Call of string * t list

let rec pp fmt (Call (name, args)) =
  let pp_sep fmt () = Format.fprintf fmt ",@ " in
  Format.fprintf fmt "%s (@[%a@])" name (Format.pp_print_list ~pp_sep pp) args
