module Std
  (C : sig
         val contrib_name : string
       end) :
sig
  type coq_term = Term.constr
  type coq_type = Term.constr

  val resolve_symbol : string list -> string -> Term.constr Lazy.t

  module Unit :
  sig
    val tt : coq_term Lazy.t
    val unit : coq_type Lazy.t
  end

  module Positive :
  sig
    val pos_type : coq_type Lazy.t
    val to_positive : int -> coq_term
    val of_positive : coq_term -> int
  end

  module BinNum :
  sig
    val n_type : coq_type Lazy.t
    val to_N : int -> coq_term
    val of_N : coq_term -> int
  end

  module Nat :
  sig
    val nat_type : coq_type Lazy.t
    val c_O : coq_term Lazy.t
    val c_S : coq_term Lazy.t

    val to_nat : int -> coq_term
    val of_nat : coq_term -> int
  end

  module Option :
  sig
    val option_type : coq_type Lazy.t
    val option_of : coq_type -> coq_type

    val c_Some : coq_term Lazy.t
    val c_None : coq_term Lazy.t
    val to_option : coq_type -> coq_term option -> coq_term
  end

  module List :
  sig
    val list_of : coq_type -> coq_type
    val c_nil : coq_term Lazy.t
    val c_cons : coq_term Lazy.t
    val to_list : coq_type -> coq_term list -> coq_term
  end

  module PosMap :
  sig
    val c_leaf : coq_term Lazy.t
    val c_node : coq_term Lazy.t

    val to_posmap : 'b -> ('b -> 'c option -> 'b -> 'b) ->
      ('a -> 'c option) -> 'a list -> 'b
  end

  module SigT :
  sig
    val sigT_type : coq_type Lazy.t
    val sigT : coq_type -> coq_term -> coq_type

    val c_existT : coq_term Lazy.t
    val existT : coq_type -> coq_term -> coq_term -> coq_term -> coq_term
  end

  module Pair :
  sig
    val prod_type : coq_type Lazy.t
    val prod : coq_type -> coq_type -> coq_type

    val c_pair : coq_term Lazy.t
    val pair   : coq_type -> coq_type -> coq_term -> coq_term -> coq_term
  end
end

