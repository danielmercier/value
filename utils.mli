type (_, _) hlist =
  | [] : ('a, 'a) hlist
  | ( :: ) : 'a * ('b, 'c) hlist -> ('a -> 'b, 'c) hlist
