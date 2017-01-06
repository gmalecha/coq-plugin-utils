(** Calling Ltac **)
let ltac_call tac (args:Tacexpr.glob_tactic_arg list) =
  Tacexpr.TacArg(Loc.dummy_loc,Tacexpr.TacCall(Loc.dummy_loc, Misctypes.ArgArg(Loc.dummy_loc, Lazy.force tac),args))

(* Calling a locally bound tactic *)
let ltac_lcall tac args =
  Tacexpr.TacArg(Loc.dummy_loc,Tacexpr.TacCall(Loc.dummy_loc, Misctypes.ArgVar(Loc.dummy_loc, Names.id_of_string tac),args))

let ltac_letin (x, e1) e2 =
  Tacexpr.TacLetIn(false,[(Loc.dummy_loc,Names.id_of_string x),e1],e2)

(* Copied from coq/plugins/setoid_ring/newring.ml *)
let ltac_apply (f : Tacinterp.Value.t) (args: Tacinterp.Value.t list) =
  let open Names in
  let open Tacexpr in
  let open Misctypes in
  let fold arg (i, vars, lfun) =
    let id = Id.of_string ("x" ^ string_of_int i) in
    let x = Reference (ArgVar (Loc.ghost, id)) in
    (succ i, x :: vars, Id.Map.add id arg lfun)
  in
  let (_, args, lfun) = List.fold_right fold args (0, [], Id.Map.empty) in
  let lfun = Id.Map.add (Id.of_string "F") f lfun in
  let ist = { (Tacinterp.default_ist ()) with Tacinterp.lfun = lfun; } in
  Tacinterp.eval_tactic_ist ist (ltac_lcall "F" args)

let to_ltac_val c = Tacinterp.Value.of_constr c

(** Pose **)

(* A clause specifying that the [let] should not try to fold anything
   in the goal *)
let nowhere =
{ Locus.onhyps = Some []
; Locus.concl_occs = Locus.NoOccurrences
}

let pose (name : string) (value : Term.constr)
    (k : Term.constr -> 'a Proofview.tactic) : 'a Proofview.tactic =
  Proofview.Goal.enter { enter = begin fun gl ->
      let name = Names.id_of_string name in
      let fresh_name =
        Tactics.fresh_id_in_env [] name (Proofview.Goal.env gl)
      in
      Proofview.tclTHEN (Tactics.letin_tac None (Names.Name fresh_name) value None nowhere)
	(k (Term.mkVar fresh_name))
    end }
