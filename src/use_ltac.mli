(** [ltac_apply f xs] constructs a tactic which calls [f] with the
 ** [xs] arguments. The result is a tactic that can be run by passing it
 ** an appropriate [goal sigma].
 **)
val ltac_apply : Tacexpr.glob_tactic_expr -> Tacexpr.glob_tactic_arg list -> Proof_type.tactic

(** Convert a Gallina term (Term.constr) into an Ltac value which
 ** can be passed to an Ltac function.
 **)
val to_ltac_val : Term.constr -> Tacexpr.glob_tactic_arg


(** [pose n c k] ~ ltac:(pose (n := c) ; k n) **)
val pose : string -> Term.constr -> (Term.constr -> Proof_type.tactic)
  -> Proof_type.tactic
