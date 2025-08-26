open Util
open Parsetree

module Parse = struct

  module P = Parser
  module L = Lexing

  let lexbuf_from_channel = fun name channel ->
    let lexbuf = Lexing.from_channel channel in
    lexbuf.Lexing.lex_curr_p <- {
        Lexing.pos_fname = name;
        Lexing.pos_lnum  = 1;
        Lexing.pos_bol   = 0;
        Lexing.pos_cnum  = 0
      };
    lexbuf

  let parse_command = fun () ->
    MenhirLib.Convert.Simplified.traditional2revised Parser.command

  let parse_file = fun () ->
    MenhirLib.Convert.Simplified.traditional2revised Parser.file

  let lexer lexbuf = fun () ->
    let token = Lexer.main lexbuf in
    (token, L.lexeme_start_p lexbuf, L.lexeme_end_p lexbuf)

  let from_channel parse ~name channel =
    let lexbuf = lexbuf_from_channel name channel in
    parse () (lexer lexbuf)

  let from_file parse filename =
    let channel = open_in filename in
    finally
      (fun () -> close_in channel)
      (from_channel parse ~name:filename) channel

  let process_file filename =
    let decl = from_file parse_file filename in
    decl

  let stdbuf = lexbuf_from_channel "stdin" stdin

  let process_command () =
    parse_command () (lexer stdbuf)

end
(* ----------------------------------------------------------- *)
let globals = Hashtbl.create 107

type check_opt = {
   trans     : bool;
   glitch    : bool;
   para      : bool;
   order     : int option;
   option    : Util.tool_opt;
  }

let process_check_opt os =
  let trans = ref false in
  let glitch = ref true in
  let para = ref false in
  let order = ref None in
  let bool = ref true in
  let print = ref true in
  let doit = function
    | Transition -> trans := true
    | NoGlitch -> glitch := false
    | Para -> para := true
    | Order n -> order := Some n
    | NoBool -> bool := false
    | NoPrint -> print := false in
  List.iter doit os;
  { trans  = !trans;
    glitch = !glitch;
    para   = !para;
    order  = !order;
    option = { pp_error = !print; checkbool = !bool; };
  }

let mk_order o nb_shares =
  match o.order with
  | None -> nb_shares - 1
  | Some i -> i

let pp_option fmt o =
  Format.fprintf fmt "(%stransition,%sglitch)"
  (if o.trans then "" else "no ")
  (if o.glitch then "" else "no ")

let check_ni f o =
  let func = Prog.get_global globals f in
  Format.printf "Checking NI for %s: %a@." (data f) pp_option o;
  let (params, nb_shares, all, _) =
    Prog.build_obs_func ~trans:o.trans ~glitch:o.glitch ~ni:`NI (loc f) func in
(*  Format.printf "@[<v>observations:@ %a@]@." Checker.pp_eis all; *)
  let order = mk_order o nb_shares in
  Checker.check_ni ~para:o.para ~fname:(data f) o.option params ~order nb_shares all

let check_threshold f o =
  let func = Prog.get_global globals f in
  let nb_shares =
    match func.Prog.f_in with
    | (_,xs) :: _ -> List.length xs
    | _           -> assert false in
  Format.printf "Checking Threshold for %s: %a@." (data f) pp_option o;
  let func = Prog.threshold func in
(*  Format.printf "%a@." (Prog.pp_func ~full:Prog.var_pinfo) func; *)
  let (params, _, all, _) =
    Prog.build_obs_func ~trans:o.trans ~glitch:o.glitch ~ni:`Threshold (loc f) func in
  let order = mk_order o nb_shares in
 (* Format.printf "@[<v>observations:@ %a@]@." Checker.pp_eis all; *)
  Checker.check_threshold o.option ~para:o.para order params all

let check_sni f b o =
  let from, to_ =
    match b with None -> None, None | Some (i,j) -> Some i, Some j in
  let func = Prog.get_global globals f in
  Format.printf "Checking SNI for %s: %a@." (data f) pp_option o;
  let (params, nb_shares, interns, outputs) =
    Prog.build_obs_func ~trans:o.trans ~glitch:o.glitch ~ni:`SNI (loc f) func in
  let order = mk_order o nb_shares in
  Checker.check_sni o.option ~para:o.para ~fname:(data f) ?from ?to_ params nb_shares ~order interns outputs

let pp_added func =
  Format.printf "proc %a added@." (HS.pp false) func.Prog.f_name
(*  Format.printf "%a@." (Prog.pp_func ~full:Prog.dft_pinfo) func *)

let add_operator o ty bij = 
  match List.rev ty with
  | [] -> assert false
  | ty::tys -> 
    try ignore (Expr.Op.find (data o)); error "" (Some(loc o)) "duplicate operator %s" (data o)
    with Not_found ->
    let o = Expr.Op.make (data o) (Some(List.rev tys, ty)) bij Expr.Other in
    Format.printf "operator %s added@." o.Expr.op_name

let rec process_command c =
  match c with
  | Operator (o, ty, bij) ->
    add_operator o ty bij
  | Func f ->
    pp_added (Prog.Process.func globals f)
  | NI (f,o)     -> check_ni f (process_check_opt o)
  | SNI (f,b,o)  -> check_sni f b (process_check_opt o)
  | Probing(f,o) -> check_threshold f (process_check_opt o)
  | Read_file filename ->
    Format.eprintf "read_file %s@." (data filename);
    process_file filename
  | Read_ilang filename ->
    Format.eprintf "read_ilang %s@." (data filename);
    let func = Ilang.process_file (data filename) in
    Prog.add_global globals func
  | Print f ->
    let func = Prog.get_global globals f in
    Format.printf "%a@." (Prog.pp_func ~full:Prog.dft_pinfo) func
  | Verbose i -> Util.set_verbose (data i)
  | Exit ->
    Format.eprintf "Bye bye!@.";
    exit 0

and process_file filename =
  let cs = Parse.process_file (data filename) in
  List.iter process_command cs

(*
let main =
  while true do
    try
      Format.printf ">"; Format.print_flush ();
      let c = Parse.process_command () in
      process_command c
    with
    | ParseError (l,s) ->
      let s = match s with Some s -> s | None -> "" in
      Format.eprintf "Parse error at %s: %s@." (Location.to_string l) s;
      exit 1
    | LexicalError (l,s) ->
      let pp_loc fmt loc =
        match loc with
        | None -> ()
        | Some loc -> Format.fprintf fmt " at %s" (Location.to_string loc) in
      Format.eprintf "Lexical error %a : %s@." pp_loc l s

    | Util.Error e ->
      Format.eprintf "%a@." Util.pp_error e
  done
*)
open Expr
open State
(*
let main =
    let word = w1 in
    let a = V.mk_var "a" word in
    let av = V.mk_var "av" word in
    let b = V.mk_var "b" word in
    let bv = V.mk_var "bv" word in
    let a0v = V.mk_var "a0" word in
    let a1v = V.mk_var "a1" word in
    let b0v = V.mk_var "b0" word in
    let b1v = V.mk_var "b1" word in
    (*
    let a0 = share a 0 a0v in
    let a1 = share a 1 a1v in
    let b0 = share b 0 b0v in
    let b1 = share b 1 b1v in
    *)
    let ae = share a 0 av in
    let be = share b 0 bv in
    let a0 = rnd a0v in
    let b0 = rnd b0v in
    let a1 = add ae a0 in
    let b1 = add be b0 in

    let rv = V.mk_var "r" word in
    let r = rnd rv in
    (* let o0 = add (mul a0 b0) (add r (mul a0 b1)) in *)
    (* let o0 = add r (mul a0 b1) in *)
    let o0 = add r a0 in
    (* let t = add r (mul a1 b0) in *)
    let params = [a; b] in
    (* let params = [a0v; a1v; b0v; b1v; rv] in *)
    let state = init_state 1 params in (* nb_shares=1 for "probing", 2 for NI *)
    let n0 = add_top_expr state o0 in
    init_todo state; (* initalize the list of randoms used only once *)
    (* let simplified = simplify state in *)
    let simplified = simplify_until state 0 in (* maybe more efficient than simplify ? *)
    Format.printf "simplified res: %s\n" (if simplified then "true" else "false");
    let simpl_o0 = simplified_expr state o0 in
    Format.printf "base: %a\n" pp_expr o0;
    Format.printf "simplified: %a\n" pp_expr simpl_o0
    *)


let rec process_lines process_line =
  try
    let line = read_line () in
    process_line line;
    process_lines process_line
  with End_of_file -> ()

let read_circuit line =
    (* Format.printf "circuit: %s\n" line; *)
    let open Yojson.Basic.Util in
    let json = Yojson.Basic.from_string line in
    let gates = json |> member "gates" |> to_list in
    let word = w1 in
    let build_gate gate_map gate =
        (* Format.printf "Building gate %s\n" (Yojson.Basic.pretty_to_string gate); *)
        let name = gate |> member "name" |> to_string in
        let kind = gate |> member "kind" |> to_string in
        if kind = "random" then
            let v = V.mk_var name word in
            (name, rnd v, None)
        else if kind = "constant" then
            (*
            let value = gate |> member "value" |> to_int in
            (name, econst (C.make word (Z.of_int value)), None)
            *)
            let v = V.mk_var name word in
            (name, pub v, None)
        else if kind = "secret" then
            let v_secret = V.mk_var name word in
            let v_sh = V.mk_var ("vsh:" ^ name) word in
            (name, share v_secret 0 v_sh, Some v_secret)
        else begin
            assert (kind = "operation");
            let operation = gate |> member "operation" |> to_string in
            let operands =
                gate |> member "operands" |> to_list |> List.map to_string
                |> List.map (Hashtbl.find gate_map) in
            let unary_ops = [("neg", neg); ("mul2", mul2); ("mul3", mul3); ("square", square)] in 
            let e = if List.mem_assoc operation unary_ops then begin
                assert (List.length operands = 1);
                (List.assoc operation unary_ops) (List.hd operands)
            end else begin
                assert (List.length operands > 0);
                let op =
                    if operation = "add" then
                        add
                    else if operation = "mul" then
                        mul
                    else begin
                        Format.printf "invalid operation: %s\n%!" operation;
                        assert false (* Invalid operation *)
                    end
                in
                List.fold_left op (List.hd operands) (List.tl operands)
            end
            in
            (name, e, None)
        end
    in
    let gate_map = Hashtbl.create (List.length gates) in
    let new_gate gate =
        let name, e, param = build_gate gate_map gate in
        Hashtbl.add gate_map name e;
        (e, param)
    in
    Format.printf "start creating expr@.";
    let (expr_time, expr_params) = time (List.map new_gate) gates in
    Format.printf "done creating expr@.";
    let gates_e = List.map fst expr_params in
    let params = List.filter_map snd expr_params in
    let state = init_state 1 params in
    let get_secret_node (e, param) = match param with
    | Some param -> Some (param.v_name, add_expr state e)
    | None -> None
    in
    let secret_nodes = List.filter_map get_secret_node expr_params in
    let get_secret_param (e, param) = match param with
    | Some param -> Some (param.v_name, param)
    | None -> None
    in
    let secret_params = List.filter_map get_secret_param expr_params in
    let res_json =
        let open Yojson.Basic in
        to_string (`Assoc [
            ("done", `Bool true);
            ("expr_time", `Float expr_time);
        ])
    in
    Format.printf "MVRES: %s\n%!" res_json;
    state, gate_map, secret_nodes, secret_params

let str_of_bool b = if b then "true" else "false"


let check_tuple state gate_map secret_nodes secret_params line =
    let check tuple =
        (* Format.printf "Clearing state\n."; *)
        clear_state state;
        Format.printf "add_top_expr\n%!";
        (* add_top_expr seems to be a bottleneck *)
        let (time_top, n0) = time (add_top_expr state) tuple in
        (* let used_sec (name, node) = if used_share state node then Some name else None in *)
        let used_sec (name, node) = 
            if used_param state node then Some name else None in
        let pre_used_secrets = List.filter_map used_sec secret_params in
        Format.printf "init_todo\n%!";
        init_todo state;
        Format.printf "simplify_until\n%!";
        (* let simpl_res = simplify state in *)
        let (time_simplify, simpl_res) = time (simplify_until state) 0 in
        Format.printf "simplify_expr\n%!";
        (* let simpl_expr = simplified_expr state probed_tuple in *)
        (* Format.printf "Tuple: %s\n" line; *)
        (* Format.printf "  Simplify res: %s\n" (str_of_bool simpl_res); *)
        (* Format.printf "  Simplified: %a\n" pp_expr simpl_expr; *)
        (* Format.printf "  Secret nodes: %s\n" (String.concat ", " (List.map fst secret_nodes)); *)
        (* Format.eprintf "post-simplify %a@." pp_state state; *)
        Format.printf "used_secrets\n%!";
        let used_sec2 (name, node) = if used_share state node then Some name else None in
        let used_secrets = List.filter_map used_sec secret_params in
        (used_secrets, time_top, time_simplify, pre_used_secrets)
    in
    let open Yojson.Basic.Util in
    let json = Yojson.Basic.from_string line in
    let probes = json |> member "probes" |> to_list in
    let probes_e = List.map (fun p -> p |> to_string |> Hashtbl.find gate_map) probes in
    let probed_tuple = tuple (Array.of_list probes_e) in
    let (exec_t, (used_secrets, time_top, time_simplify, preu)) = time check probed_tuple in
    (* Format.printf "  Used secrets: %s\n" (String.concat ", " used_secrets); *)
    let res_json =
        let open Yojson.Basic in
        let res = `Assoc [
            ("result", `Bool (List.length used_secrets = 0));
            ("used_secrets", `List (List.map (fun s -> `String s) used_secrets));
            ("pre_used_secrets", `List (List.map (fun s -> `String s) preu));
            ("exec_time", `Float exec_t);
            ("add_top_time", `Float time_top);
            ("simplify_time", `Float time_simplify);
            ("n_bij", `Int (n_bij state))
        ] in
        to_string res
    in
    Format.printf "MVRES: %s\n%!" res_json;
    ()

let main =
    let state, gate_map, secret_nodes, secret_params = read_circuit (read_line ()) in
    process_lines (check_tuple state gate_map secret_nodes secret_params)

