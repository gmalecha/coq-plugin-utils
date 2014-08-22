type 'a pattern =
| Glob of Term.constr Lazy.t
| EGlob of Term.constr
| App of 'a pattern * 'a pattern
| Lam of string * 'a pattern * 'a pattern
| As of 'a pattern * 'a
| Ref of 'a
| Choice of ('a pattern) list
| Impl of 'a pattern * 'a pattern
| Ignore

val match_pattern : 'a pattern -> Term.constr -> 'c -> ('a,Term.constr) Hashtbl.t -> ('a,Term.constr) Hashtbl.t

val matches : 'a -> ('b pattern * (('b, Term.constr) Hashtbl.t -> 'c)) list -> Term.constr -> 'c
