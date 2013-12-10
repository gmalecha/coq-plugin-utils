open Ocamlbuild_plugin
open Unix

let get_output cmd =
  let ic, oc = Unix.open_process cmd in
  let buf = Buffer.create 16 in
  (try
     while true do
       Buffer.add_channel buf ic 1
     done
   with End_of_file -> ());
  let _ = Unix.close_process (ic,oc) in
  Buffer.contents buf

let split ls sep =
  let rec split_from n =
    try
      let x = String.index_from ls n sep in
      if x > 0 then
	String.sub ls n (x-n) :: split_from (x+1)
      else
	[String.sub ls n (String.length ls - n)]
    with
      Not_found ->
	[String.sub ls n (String.length ls - n)]
  in split_from 0

let config =
  let res =
    lazy (let x = split (get_output "coqc -config") '\n' in
          List.flatten (List.map
			  (fun x ->
			    match split x '=' with
			      k :: v :: [] -> [(k,v)]
			    | _ -> []) x)) in
  fun x ->
    try
      Some (List.assoc x (Lazy.force res))
    with
      Not_found -> None

let coqlib =
  lazy (
    match config "COQLIB" with
      None -> assert false
    | Some x -> x)

let coq_dir s = Lazy.force coqlib ^ s

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

let coq_args =
  [A "-rectypes"] @ (List.flatten (List.map (fun x -> [A "-I"; A (coq_dir x)]) all_coq_dirs))
let coq_plugin_args =
  [A "-rectypes"] @ (List.flatten (List.map (fun x -> [A "-I"; A (coq_dir x)]) all_coq_plugins))

let coq_plugin_util_args =
  List.flatten (List.map (fun x -> [A "-I"; A (coq_dir x)])
		  ["user-contrib/PluginUtils"])

let add_coq x =
  match x with
  | After_rules ->
    rule ".ml4.ml" ~dep:"%.ml4" ~prod:"%.ml"
      (fun env _ ->
	let ml4 = env "%.ml4" and ml = env "%.ml" in
	Cmd (S[A"camlp5o";T(tags_of_pathname ml4 ++ "p4mod");
	       T(tags_of_pathname ml4 ++ "p4option");
	       A"-I";A (coq_dir "parsing");
	       A"pa_extend.cmo";A"pa_macro.cmo";A"q_MLast.cmo";A"grammar.cma";
	       A"-loc";A"loc";
	       A"-o"; Px ml; A"-impl"; P ml4])) ;
    flag ["ocaml";"coq";"compile"] & (S coq_args) ;
    flag ["ocaml";"coq";"pack"] & (S coq_args) ;
    flag ["ocaml";"coq_plugins";"compile"] & (S coq_plugin_args) ;
    flag ["ocaml";"coq_plugins";"pack"] & (S coq_plugin_args) ;
    flag ["ocaml";"compile";"plugin_utils"] & (S coq_plugin_util_args) ;
    flag ["ocaml";"link";"byte";"plugin_utils"] & (S (coq_plugin_util_args @ [A (coq_dir "user-contrib/PluginUtils/plugin_utils.cma")])) ;
    flag ["ocaml";"link";"native";"plugin_utils"] & (S (coq_plugin_util_args @ [A (coq_dir "user-contrib/PluginUtils/plugin_utils.cmx")])) ;
    pflag ["ocaml"] "cflags" (fun x -> S (List.map (fun x -> A x) (split x ','))) ;
    pflag ["ocaml";"link"] "lflags" (fun x -> S (List.map (fun x -> A x) (split x ',')))
  | _ -> ()
;;

Ocamlbuild_plugin.dispatch add_coq
