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
