module Std (C : sig val contrib_name : string end) =
struct
  let pp_constr fmt x = Pp.pp_with fmt (Printer.pr_constr x)

  let resolve_symbol (path : string list) (tm : string) : Term.constr =
    let re = Coqlib.find_reference C.contrib_name path tm in
    Libnames.constr_of_global re

  let rec app_full trm acc =
    match Term.kind_of_term trm with
      Term.App (f, xs) -> app_full f (Array.to_list xs @ acc)
    | _ -> (trm, acc)

  let datatypes_pkg = ["Coq";"Init";"Datatypes"]
  let bignums_pkg = ["Coq";"Numbers";"BinNums"]
  let specif_pkg = ["Coq";"Init";"Specif"]

  let c_None = lazy (resolve_symbol datatypes_pkg "None")
  let c_Some = lazy (resolve_symbol datatypes_pkg "Some")

  let to_option typ (x : Term.constr option) =
    match x with
      None -> Term.mkApp (Lazy.force c_None, [| typ |])
    | Some x -> Term.mkApp (Lazy.force c_Some, [| typ ; x |])

  let to_positive =
    let xH = lazy (resolve_symbol bignums_pkg "xH") in
    let xO = lazy (resolve_symbol bignums_pkg "xO") in
    let xI = lazy (resolve_symbol bignums_pkg "xI") in
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

  let to_N =
    let o = lazy (resolve_symbol bignums_pkg "N0") in
    let pos = lazy (resolve_symbol bignums_pkg "Npos") in
    fun n ->
      if n = 0
      then Lazy.force o
      else
	if n < 0
	then raise (Invalid_argument ("to_N: " ^ string_of_int n))
	else Term.mkApp (Lazy.force pos, [| to_positive n |])

  let c_S = lazy (resolve_symbol datatypes_pkg "S")
  let c_O = lazy (resolve_symbol datatypes_pkg "O")

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

  let bad_arg msg trm =
    let _ = Format.eprintf "\n%s: %a\n" msg pp_constr trm in
    raise (Invalid_argument msg)

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

  let c_nil = lazy (resolve_symbol datatypes_pkg "nil")
  let c_cons = lazy (resolve_symbol datatypes_pkg "cons")

  let to_list typ =
    let the_nil = Term.mkApp (Lazy.force c_nil, [| typ |]) in
    let rec to_list (ls : Term.constr list) : Term.constr =
      match ls with
	[] -> the_nil
      | l :: ls ->
	Term.mkApp (Lazy.force c_cons, [| typ ; l ; to_list ls |])
    in to_list

  let list_of typ =
    Term.mkApp (resolve_symbol datatypes_pkg "list", [| typ |])

  type 'a pmap =
  | PM_Empty
  | PM_Branch of 'a pmap * 'a option * 'a pmap

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

  (** Depdendent pairs **)
  let sigT_ctor : Term.constr Lazy.t =
    lazy (resolve_symbol specif_pkg "sigT")

  let existT_ctor : Term.constr Lazy.t =
    lazy (resolve_symbol specif_pkg "existT")

  let sigT a b : Term.constr =
    Term.mkApp (Lazy.force sigT_ctor, [| a ; b |])

  let existT a b f s : Term.constr =
    Term.mkApp (Lazy.force existT_ctor, [| a ; b ; f ; s |])

  (** Non-dependent pairs **)
  let prod_ctor : Term.constr Lazy.t =
    lazy (resolve_symbol specif_pkg "prod")

  let pair_ctor : Term.constr Lazy.t =
    lazy (resolve_symbol specif_pkg "pair")

  let prod a b : Term.constr =
    Term.mkApp (Lazy.force prod_ctor, [| a ; b |])

  let pair a b f s : Term.constr =
    Term.mkApp (Lazy.force pair_ctor, [| a ; b ; f ; s |])

end
