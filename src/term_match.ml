type ('a,'b,'c) pattern =
| Glob of Term.constr Lazy.t
| EGlob of Term.constr
| Glob_no_univ of Term.constr Lazy.t
| EGlob_no_univ of Term.constr
| App of ('a,'b,'c) pattern * ('a,'b,'c) pattern
| Lam of 'b * ('a,'b,'c) pattern * ('a,'b,'c) pattern
| As of ('a,'b,'c) pattern * 'a
| Ref of 'b
| Choice of (('a,'b,'c) pattern) list
| Impl of ('a,'b,'c) pattern * ('a,'b,'c) pattern
| Pi of ('a,'b,'c) pattern * ('a,'b,'c) pattern
| Sort
| Ignore
| Filter of ('c -> Term.constr -> bool) * ('a,'b,'c) pattern

exception Match_failure

let rec apps f ls =
  match ls with
    [] -> f
  | l :: ls ->
    apps (App (f,l)) ls

let get x =
  As (Ignore, x)


(** NOTE: This function does not clear writes by failed choices **)
let rec match_pattern p e ctx s =
  match p with
  | Ignore -> s
  | Glob name ->
    begin
      if Term.eq_constr (Lazy.force name) e
      then
	s
      else
	raise Match_failure
    end
  | EGlob name ->
    begin
      if Term.eq_constr name e
      then
	s
      else
	raise Match_failure
    end
  | Glob_no_univ name ->
    begin
      if Term.eq_constr_nounivs (Lazy.force name) e
      then
	s
      else
	raise Match_failure
    end
  | EGlob_no_univ name ->
    begin
      if Term.eq_constr_nounivs name e
      then
	s
      else
	raise Match_failure
    end
  | Filter (f, p) ->
    if f ctx e then match_pattern p e ctx s else raise Match_failure
  | Choice pl ->
    begin
      let rec try_each pl =
	match pl with
	  [] -> raise Match_failure
	| p :: pl ->
	  try
	    match_pattern p e ctx s
	  with
	    Match_failure -> try_each pl
      in try_each pl
    end
  | App _ ->
    begin
      match Term.kind_of_term e with
      | Term.App (f, args) ->
	  match_app f args (Array.length args - 1) p ctx s
      | _ -> raise Match_failure
    end
  | Lam (nm, pty, pbody) ->
    begin
      match Term.kind_of_term e with
      | Term.Lambda (n, t, c) ->
	let _ = match_pattern pty t ctx s in
	match_pattern pbody c ctx s
      | _ -> raise Match_failure
    end
  | As (ptrn, nm) ->
    begin
      let res = match_pattern ptrn e ctx s in
      let _ = Hashtbl.add res nm e in
      res
    end
  | Impl (l,r) ->
    begin
      match Term.kind_of_term e with
	Term.Prod (_, lhs, rhs) ->
	  if Vars.noccurn 1 rhs then
	    let _ = match_pattern l lhs ctx s in
	    match_pattern r rhs ctx s
	  else
	    raise Match_failure
      | _ -> raise Match_failure
    end
  | Pi (l,r) ->
    begin
      match Term.kind_of_term e with
	Term.Prod (_, lhs, rhs) ->
	  let _ = match_pattern l lhs ctx s in
	  match_pattern r rhs ctx s
      | _ -> raise Match_failure
    end
  | Sort ->
    if Term.isSort e then s
    else raise Match_failure
  | Ref n ->
    assert false
and match_app f args i p ctx s =
  if i < 0
  then match_pattern p f ctx s
  else
    match p with
      App (fp , arg_p) ->
	let s = match_app f args (i - 1) fp ctx s in
	match_pattern arg_p args.(i) ctx s
    | _ ->
      match_pattern p (Term.mkApp (f, Array.sub args 0 (i + 1))) ctx s

let matches gl ls e =
  let x = Hashtbl.create 5 in
  let rec recur ls =
    match ls with
    | [] -> raise Match_failure
    | (p,f) :: ls ->
      try
	f gl (match_pattern p e gl x)
      with
	Match_failure ->
	  let _ = Hashtbl.clear x in
	  recur ls
  in
  recur ls

let matches_app gl ls e args from =
  let x = Hashtbl.create 5 in
  let rec recur ls =
    match ls with
    | [] -> raise Match_failure
    | (p,f) :: ls ->
      try
	f gl (match_app e args from p gl x)
      with
	Match_failure ->
	  let _ = Hashtbl.clear x in
	  recur ls
  in
  recur ls

let dbg msg =
  Format.printf "%s\n" msg
