type ('a,'b) pattern =
| Glob of Term.constr Lazy.t
| EGlob of Term.constr
| App of ('a,'b) pattern * ('a,'b) pattern
| Lam of 'b * ('a,'b) pattern * ('a,'b) pattern
| As of ('a,'b) pattern * 'a
| Ref of 'b
| Choice of (('a,'b) pattern) list
| Impl of ('a,'b) pattern * ('a,'b) pattern
| Pi of ('a,'b) pattern * ('a,'b) pattern
| Ignore
| Filter of (Term.constr -> bool) * ('a,'b) pattern

exception Match_failure

val match_pattern : ('a,'b) pattern -> Term.constr -> 'c -> ('a,Term.constr) Hashtbl.t -> ('a,Term.constr) Hashtbl.t

val matches : 'a -> (('b,'d) pattern * ('a -> ('b, Term.constr) Hashtbl.t -> 'c)) list -> Term.constr -> 'c

val matches_app : 'a -> (('b,'d) pattern * ('a -> ('b, Term.constr) Hashtbl.t -> 'c)) list -> Term.constr -> Term.constr array -> int -> 'c
