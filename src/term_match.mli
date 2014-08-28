type ('a,'b,'c) pattern =
| Glob of Term.constr Lazy.t
| EGlob of Term.constr
| App of ('a,'b,'c) pattern * ('a,'b,'c) pattern
| Lam of 'b * ('a,'b,'c) pattern * ('a,'b,'c) pattern
| As of ('a,'b,'c) pattern * 'a
| Ref of 'b
| Choice of (('a,'b,'c) pattern) list
| Impl of ('a,'b,'c) pattern * ('a,'b,'c) pattern
| Pi of ('a,'b,'c) pattern * ('a,'b,'c) pattern
| Ignore
| Filter of ('c -> Term.constr -> bool) * ('a,'b,'c) pattern

exception Match_failure

val apps : ('a,'b,'c) pattern -> ('a,'b,'c) pattern list -> ('a,'b,'c) pattern

val get : 'a -> ('a,'b,'c) pattern

val match_pattern : ('a,'b,'c) pattern -> Term.constr -> 'c -> ('a,Term.constr) Hashtbl.t -> ('a,Term.constr) Hashtbl.t

val matches : 'a -> (('b,'d,'a) pattern * ('a -> ('b, Term.constr) Hashtbl.t -> 'c)) list -> Term.constr -> 'c

val matches_app : 'a -> (('b,'d,'a) pattern * ('a -> ('b, Term.constr) Hashtbl.t -> 'c)) list -> Term.constr -> Term.constr array -> int -> 'c
