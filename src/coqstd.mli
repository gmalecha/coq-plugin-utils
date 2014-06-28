module Std (C : sig val contrib_name : string end) :
sig
  val resolve_symbol : string list -> string -> Term.constr

  val to_positive : int -> Term.constr
  val to_N : int -> Term.constr
  val to_nat : int -> Term.constr

  val c_Some : Term.constr Lazy.t
  val c_None : Term.constr Lazy.t
  val to_option : Term.constr -> Term.constr option -> Term.constr

  val c_nil : Term.constr Lazy.t
  val c_cons : Term.constr Lazy.t
  val to_list : Term.constr -> Term.constr list -> Term.constr
  val list_of : Term.constr -> Term.constr

  val to_posmap : 'b -> ('b -> 'c option -> 'b -> 'b) ->
    ('a -> 'c option) -> 'a list -> 'b

  val sigT_ctor : Term.constr Lazy.t
  val existT_ctor : Term.constr Lazy.t
  val sigT : Term.constr -> Term.constr -> Term.constr
  val existT : Term.constr -> Term.constr -> Term.constr -> Term.constr -> Term.constr

  val prod_ctor : Term.constr Lazy.t
  val pair_ctor : Term.constr Lazy.t
  val prod : Term.constr -> Term.constr -> Term.constr
  val pair : Term.constr -> Term.constr -> Term.constr -> Term.constr -> Term.constr
end
