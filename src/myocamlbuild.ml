open Ocamlbuild_plugin
(*
let coqlib = "/home/gmalecha/.opam/system/lib/coq/"

let coq_dir s = coqlib ^ s

let all_coq_dirs =
  ["kernel";"lib";"library";"parsing";"pretyping";
   "interp";"proofs";"tactics";"toplevel"]

let all_coq_plugins =
  ["plugins/cc"
  ;"plugins/decl_mode"
  ;"plugins/extraction"
  ;"plugins/field"
  ;"plugins/firstorder"
  ;"plugins/fourier"
  ;"plugins/funind"
  ;"plugins/interface"
  ;"plugins/micromega"
  ;"plugins/nsatz"
  ;"plugins/omega"
  ;"plugins/quote"
  ;"plugins/ring"
  ;"plugins/romega"
  ;"plugins/rtauto"
  ;"plugins/setoid_ring"
  ;"plugins/subtac"
  ;"plugins/subtac/test"
  ;"plugins/syntax"
  ;"plugins/xml"]

let coq_args = [A "-rectypes"] @ (List.flatten (List.map (fun x -> [A "-I"; A (coq_dir x)]) all_coq_dirs))
let coq_plugin_args = [A "-rectypes"] @ (List.flatten (List.map (fun x -> [A "-I"; A (coq_dir x)]) all_coq_plugins))
;;

let add_coq = function
  | After_rules ->
    flag ["ocaml";"coq";"compile"] (S coq_args) ;
    flag ["ocaml";"coq";"pack"] (S coq_args) ;
    flag ["ocaml";"coq_plugins";"compile"] (S coq_plugin_args) ;
    flag ["ocaml";"coq_plugins";"pack"] (S coq_plugin_args) ;
  | _ -> ()
*)
open Coq_paths;;

Ocamlbuild_plugin.dispatch Coq_paths.add_coq;;
