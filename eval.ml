open Exp

module Make (Value : Abstract_values.S) = struct
  open Value

  let rec eval = function
    | Call (name, args) -> (
        let values = List.map eval args in
        match interpretation name values with
        | Some value ->
            value
        | None ->
            assert false )
end
