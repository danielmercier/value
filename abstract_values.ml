module type S = sig
  type value

  val interpretation : string -> value list -> value option
end
