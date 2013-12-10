(** Calling Ltac **)
let ltac_call tac (args:Tacexpr.glob_tactic_arg list) =
  Tacexpr.TacArg(Util.dummy_loc,Tacexpr.TacCall(Util.dummy_loc, Glob_term.ArgArg(Util.dummy_loc, Lazy.force tac),args))

(* Calling a locally bound tactic *)
let ltac_lcall tac args =
  Tacexpr.TacArg(Util.dummy_loc,Tacexpr.TacCall(Util.dummy_loc, Glob_term.ArgVar(Util.dummy_loc, Names.id_of_string tac),args))

let ltac_letin (x, e1) e2 =
  Tacexpr.TacLetIn(false,[(Util.dummy_loc,Names.id_of_string x),e1],e2)

let ltac_apply (f:Tacexpr.glob_tactic_expr) (args:Tacexpr.glob_tactic_arg list) =
  Tacinterp.eval_tactic
    (ltac_letin ("F", Tacexpr.Tacexp f) (ltac_lcall "F" args))

let to_ltac_val c = Tacexpr.TacDynamic(Util.dummy_loc,Pretyping.constr_in c)

(** Pose **)

(* A clause specifying that the [let] should not try to fold anything
   in the goal *)
let nowhere =
{ Tacexpr.onhyps = Some []
; Tacexpr.concl_occs = false, []
}

let pose (name : string) (value : Term.constr)
    (k : Term.constr -> Proof_type.tactic) : Proof_type.tactic =
  fun gl ->
    let name = Names.id_of_string name in
    let fresh_name = Tactics.fresh_id [] name gl in
    let letin = (Tactics.letin_tac None (Names.Name fresh_name) value None nowhere) in
    Tacticals.tclTHEN letin (k (Term.mkVar fresh_name)) gl
