module type STD =
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

module Std (C : sig val contrib_name : string end) : STD =
struct
  type coq_term = Term.constr
  type coq_type = Term.constr

  let pp_constr fmt x = Pp.pp_with fmt (Printer.pr_constr x)

  let resolve_symbol (path : string list) (tm : string) : Term.constr Lazy.t =
  lazy (
    let re = Coqlib.find_reference C.contrib_name path tm in
    Universes.constr_of_global re)

  let rec app_full trm acc =
    match Term.kind_of_term trm with
      Term.App (f, xs) -> app_full f (Array.to_list xs @ acc)
    | _ -> (trm, acc)

  let bad_arg msg trm =
    let msg = Format.asprintf "%s: %a" msg pp_constr trm in
    raise (Invalid_argument msg)

  let datatypes_pkg = ["Coq";"Init";"Datatypes"]
  let bignums_pkg = ["Coq";"Numbers";"BinNums"]
  let specif_pkg = ["Coq";"Init";"Specif"]

  module Unit =
  struct
    let tt = resolve_symbol datatypes_pkg "tt"
    let unit = resolve_symbol datatypes_pkg "unit"
  end

  module Positive =
  struct

    let pos_type = resolve_symbol bignums_pkg "positive"

    let xH = resolve_symbol bignums_pkg "xH"
    let xO = resolve_symbol bignums_pkg "xO"
    let xI = resolve_symbol bignums_pkg "xI"

    let to_positive =
      let rec to_positive n =
        if n = 1 then
	  Lazy.force xH
        else
        if n mod 2 = 0 then
	  Term.mkApp (Lazy.force xO, [| to_positive (n / 2) |])
        else
  	  Term.mkApp (Lazy.force xI, [| to_positive (n / 2) |])
      in
      fun n ->
        if n <= 0
        then raise (Invalid_argument ("to_positive: " ^ string_of_int n))
        else to_positive n

    let rec of_positive n =
      let (h,args) = app_full n [] in
      if Term.eq_constr h (Lazy.force xH) then
        1
      else if Term.eq_constr h (Lazy.force xO) then
        of_positive (List.hd args) * 2
      else if Term.eq_constr h (Lazy.force xI) then
        of_positive (List.hd args) * 2 + 1
      else
        bad_arg "of_positive" n

  end

  module BinNum =
  struct
    let n_type = resolve_symbol bignums_pkg "N"

    let o = resolve_symbol bignums_pkg "N0"
    let pos = resolve_symbol bignums_pkg "Npos"

    let to_N n =
      if n = 0 then
        Lazy.force o
      else if n < 0 then
        raise (Invalid_argument (Printf.sprintf "to_N: %d" n))
      else
        let res = Positive.to_positive n in
        Term.mkApp (Lazy.force pos, [| res |])

    let of_N n =
      let (h,args) = app_full n [] in
      if Term.eq_constr h (Lazy.force o) then
        0
      else if Term.eq_constr h (Lazy.force pos) then
        Positive.of_positive (List.hd args)
      else
        bad_arg "of_N" n
  end

  module Nat =
  struct
    let nat_type = resolve_symbol datatypes_pkg "nat"
    let c_S = resolve_symbol datatypes_pkg "S"
    let c_O = resolve_symbol datatypes_pkg "O"

    let to_nat =
      let rec to_nat n =
	if n = 0 then
	  Lazy.force c_O
	else
	  Term.mkApp (Lazy.force c_S, [| to_nat (n - 1) |])
      in
      fun n ->
	if n < 0
	then raise (Invalid_argument ("to_nat: " ^ string_of_int n))
	else to_nat n

    let rec of_nat (trm : Term.constr) : int =
      let (h,args) = app_full trm [] in
      if Term.eq_constr h (Lazy.force c_O) then
	0
      else if Term.eq_constr h (Lazy.force c_S) then
	match args with
	  n :: _ -> 1 + of_nat n
	| _ -> bad_arg "of_nat" trm
      else
	bad_arg "of_nat" trm
  end

  module Option =
  struct
    let c_None = resolve_symbol datatypes_pkg "None"
    let c_Some = resolve_symbol datatypes_pkg "Some"

    let option_type = resolve_symbol datatypes_pkg "option"
    let option_of (t : coq_type) =
      Term.mkApp (Lazy.force option_type, [| t |])

    let to_option typ (x : Term.constr option) =
      match x with
	None -> Term.mkApp (Lazy.force c_None, [| typ |])
      | Some x -> Term.mkApp (Lazy.force c_Some, [| typ ; x |])
  end

  module List =
  struct
    let list_type =
      resolve_symbol datatypes_pkg "list"
    let list_of typ =
      Term.mkApp (Lazy.force list_type, [| typ |])

    let c_nil = resolve_symbol datatypes_pkg "nil"
    let c_cons = resolve_symbol datatypes_pkg "cons"

    let to_list typ =
      let the_nil = Term.mkApp (Lazy.force c_nil, [| typ |]) in
      let rec to_list (ls : Term.constr list) : Term.constr =
	match ls with
	  [] -> the_nil
	| l :: ls ->
	  Term.mkApp (Lazy.force c_cons, [| typ ; l ; to_list ls |])
      in to_list

  end

  module PosMap =
  struct
    type 'a pmap =
    | PM_Empty
    | PM_Branch of 'a pmap * 'a option * 'a pmap

    let posmap_mod = ["Coq";"FSets";"FMapPositive";"PositiveMap"]

    let c_leaf = resolve_symbol posmap_mod "Leaf"
    let c_node = resolve_symbol posmap_mod "Node"

    let rec pmap_add k v m =
      if k = 1 then
	match m with
	  PM_Empty -> PM_Branch (PM_Empty, Some v, PM_Empty)
	| PM_Branch(l,_,r) -> PM_Branch (l, Some v, r)
      else
	if k mod 2 = 0 then
	  match m with
	    PM_Empty -> PM_Branch (pmap_add (k / 2) v PM_Empty, None, PM_Empty)
	  | PM_Branch (l,d,r) -> PM_Branch (pmap_add (k / 2) v l, d, r)
	else
	  match m with
	    PM_Empty -> PM_Branch (PM_Empty, None, pmap_add (k / 2) v PM_Empty)
	  | PM_Branch (l,d,r) -> PM_Branch (l, d, pmap_add (k / 2) v r)

    let to_posmap (mkEmpty : 'b)
	(mkBranch : 'b -> 'c option -> 'b -> 'b)
	(get : 'a -> 'c option) =
      let rec to_pm i ls acc =
	match ls with
	  [] -> acc
	| l :: ls ->
	  to_pm (1 + i) ls
	    (match get l with
	      None ->  acc
	    | Some d -> pmap_add i d acc)
      in
      let rec convert pm =
	match pm with
	  PM_Empty -> mkEmpty
	| PM_Branch (l,d,r) ->
	  mkBranch (convert l) d (convert r)
      in
      fun ls ->
	let pm = to_pm 1 ls PM_Empty in
	convert pm
  end

  (** Depdendent pairs **)
  module SigT =
  struct
    let sigT_type : Term.constr Lazy.t =
      resolve_symbol specif_pkg "sigT"

    let c_existT : Term.constr Lazy.t =
      resolve_symbol specif_pkg "existT"

    let sigT a b : Term.constr =
      Term.mkApp (Lazy.force sigT_type, [| a ; b |])

    let existT a b f s : Term.constr =
      Term.mkApp (Lazy.force c_existT, [| a ; b ; f ; s |])
  end

  (** Non-dependent pairs **)
  module Pair =
  struct
    let prod_type : Term.constr Lazy.t =
      resolve_symbol datatypes_pkg "prod"

    let prod a b : Term.constr =
      Term.mkApp (Lazy.force prod_type, [| a ; b |])

    let c_pair : Term.constr Lazy.t =
      resolve_symbol datatypes_pkg "pair"

    let pair a b f s : Term.constr =
      Term.mkApp (Lazy.force c_pair, [| a ; b ; f ; s |])
  end

end
