(** Calling Ltac **)
let ltac_call tac (args:Tacexpr.glob_tactic_arg list) =
  Tacexpr.TacArg(Loc.dummy_loc,Tacexpr.TacCall(Loc.dummy_loc, Misctypes.ArgArg(Loc.dummy_loc, Lazy.force tac),args))

(* Calling a locally bound tactic *)
let ltac_lcall tac args =
  Tacexpr.TacArg(Loc.dummy_loc,Tacexpr.TacCall(Loc.dummy_loc, Misctypes.ArgVar(Loc.dummy_loc, Names.id_of_string tac),args))

let ltac_letin (x, e1) e2 =
  Tacexpr.TacLetIn(false,[(Loc.dummy_loc,Names.id_of_string x),e1],e2)

let ltac_apply (f:Tacexpr.glob_tactic_expr) (args:Tacexpr.glob_tactic_arg list) =
  Tacinterp.eval_tactic
    (ltac_letin ("F", Tacexpr.Tacexp f) (ltac_lcall "F" args))

let to_ltac_val c = Tacexpr.TacDynamic(Loc.dummy_loc,Pretyping.constr_in c)

(** Pose **)

(* A clause specifying that the [let] should not try to fold anything
   in the goal *)
let nowhere =
{ Locus.onhyps = Some []
; Locus.concl_occs = Locus.NoOccurrences
}

let pose (name : string) (value : Term.constr)
    (k : Term.constr -> 'a Proofview.tactic) : 'a Proofview.tactic =
  Proofview.Goal.enter (fun gl ->
     let name = Names.id_of_string name in
     let fresh_name =
       Tactics.fresh_id_in_env [] name (Proofview.Goal.env gl)
     in
     Proofview.tclTHEN (Tactics.letin_tac None (Names.Name fresh_name) value None nowhere)
		       (k (Term.mkVar fresh_name)))
