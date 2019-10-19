module Make (Value : Abstract_values.S) = struct
  open Value

  type eval = Eval : ('a, 'a) typ * 'a -> eval

  type _ eval_list =
    | EvalList : ('a, 'r) typ * ('a, 'r) Utils.hlist -> 'r eval_list

  let rec eval_exp_list : type r. (r, r) typ -> Exp.t list -> r eval_list =
   fun ret_typ args ->
    match args with
    | h :: q ->
        let (EvalList (rest_typ, rest_values)) = eval_exp_list ret_typ q in
        let (Eval (typ, value)) = eval h in
        EvalList (typ @-> rest_typ, value :: rest_values)
    | [] ->
        EvalList (ret_typ, [])

  and eval = function
    | Call (name, args) as exp -> (
        let (Any ret_typ) = typer exp in
        let (EvalList (typ, values)) = eval_exp_list ret_typ args in
        let signature = Function (name, typ) in
        match interpretation signature with
        | Some interpretation ->
            Eval (ret_typ, interpretation values)
        | None ->
            assert false )
end
