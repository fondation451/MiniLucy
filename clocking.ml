(*
########
Copyright © 2017

This file is part of MiniLucy.
MiniLucy is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License v3 as published by
the Free Software Foundation.

Nicolas ASSOUAD <nicolas.assouad@ens.fr>
Clément PASCUTTO <clement.pascutto@ens.fr>
########
*)

open Ast_type;;
open Ast;;

exception Bad_Clock of location * string;;

let print_list f l =
  print_string "(";
  List.iter (fun x -> f x; print_string ", ") l;
  print_string ")\n"
;;

let find_param_ck env id = try IdentMap.find id env with Not_found -> assert false;;

let rec string_ck ck =
  match ck with
  |CK_base -> "BASE"
  |CK_free -> "FREE"
  |CK_on(ck', enum_id, id) -> (string_ck ck') ^ " on " ^ enum_id ^ "(" ^ id ^ ")"
  |CK_tuple(ck_l) -> (List.fold_left (fun out ck' -> out ^ (string_ck ck') ^ ", ") "(" ck_l) ^ ")"
;;

let rec synchronize_over ck_base ck =
  match ck with
  |CK_base -> ck_base
  |CK_free -> CK_free
  |CK_on(ck', enum_id, id) -> CK_on(synchronize_over ck_base ck', enum_id, id)
  |CK_tuple(ck_l) -> CK_tuple(List.map (synchronize_over ck_base) ck_l)
;;

let equal_ck ck1 ck2 =
  match ck1, ck2 with
  |CK_free, _ |_, CK_free -> true
  |_ -> ck1 = ck2
;;

let ck_same ck1 ck2 =
  match ck1, ck2 with
  |CK_free, _ -> ck2, true
  |_, CK_free -> ck1, true
  |_ -> ck1, ck1 = ck2
;;

let rec check_clock env e =
  let node_env, var_env = env in
  match e.pexpr_desc with
  |PE_const(c) -> CK_free , {e with pexpr_clk = CK_free}
  |PE_ident(id) ->
    let ck = find_param_ck var_env id in
    ck, {e with pexpr_clk = ck}
  |PE_op(op, e1) ->
    let ck1, e1' = check_clock env e1 in
    ck1, {e with pexpr_desc = PE_op(op, e1'); pexpr_clk = ck1}
  |PE_binop(op, e1, e2) ->
    let ck1, e1' = check_clock env e1 in
    let ck2, e2' = check_clock env e2 in
    let ck, is_equal = ck_same ck1 ck2 in
    if is_equal then
      ck, {e with pexpr_desc = PE_binop(op, e1', e2'); pexpr_clk = ck}
    else
      raise (Bad_Clock (e.pexpr_loc, "Binary operator : different clocks " ^ (string_ck ck1) ^ " <> " ^ (string_ck ck2)))
  |PE_app(id, e_l, id_reset) ->
    print_string "PE_APP\n";
(*    let same_elem l ck_out =
      let rec loop x l ck_out =
        match l with
        |[] -> if ck_out = [] then x, true else CK_tuple (x::ck_out), true
        |h::t -> if x = h then loop x t (x::ck_out) else CK_base, false
      in
      match l with |[] -> CK_base, false |h::t -> loop h t []
    in*)
    let same_elem l =
      let rec loop x l = match l with |[] -> x, true |h::t -> print_endline ("ICI : " ^ string_ck h); if equal_ck x h then loop x t else CK_base, false in
      match l with |[] -> CK_base, false |h::t -> loop h t
    in
    let id_reset_ck = try IdentMap.find id_reset var_env with Not_found -> CK_free in
    let ck_l, e_l' = List.split (List.map (check_clock env) e_l) in
    let ck_arg, is_ok = same_elem ck_l in
    let ck =
      try
        match List.map (fun p -> p.param_ck) (IdentMap.find id node_env).pn_output with
        |[ck] -> synchronize_over ck_arg ck
        |_ as ck_l -> synchronize_over ck_arg (CK_tuple(ck_l))
      with Not_found -> assert false
    in
    if equal_ck id_reset_ck ck_arg && is_ok then
      ck, {e with pexpr_desc = PE_app(id, e_l', id_reset); pexpr_clk = ck}
    else
      raise (Bad_Clock (e.pexpr_loc, "Application : " ^ (List.fold_left (fun out c -> out ^ (string_ck c) ^ ", ") "(" ck_l) ^ ")"))
  |PE_fby(c, e2) ->
    let ck2, e2' = check_clock env e2 in
    ck2, {e with pexpr_desc = PE_fby(c, e2'); pexpr_clk = ck2}
  |PE_tuple(e_l) ->
    let rec mk_ck l ck_out = match l with |[] -> CK_tuple (ck_out) |h::t -> mk_ck t (h::ck_out) in
    let ck_l, e_l' = List.split (List.map (check_clock env) e_l) in
    let ck = mk_ck ck_l [] in
    ck, {e with pexpr_desc = PE_tuple e_l'; pexpr_clk =  ck}
  |PE_when(e1, enum_id, id) ->
    let ck1, e1' = check_clock env e1 in
    let ck_id = find_param_ck var_env id in
    if equal_ck ck1 ck_id then
      let ck = CK_on (ck1, enum_id, id) in
      ck, {e with pexpr_desc = PE_when(e1', enum_id, id); pexpr_clk = ck}
    else
      raise (Bad_Clock (e.pexpr_loc, "When " ^ enum_id ^ "(" ^ id ^ ") : " ^ (string_ck ck1) ^ " <> " ^ (string_ck ck_id)))
  |PE_current(e) ->
    let _, e' = check_clock env e in
    CK_base, {e with pexpr_desc = PE_current(e'); pexpr_clk = CK_base}
  |PE_merge(id, merge_l) ->
    let ck_id = find_param_ck var_env id in
    let ck_l, merge_l' = List.split (List.map (fun (id, e') -> let ck, e'' = check_clock env e' in (ck, (id, e''))) merge_l) in
(*    print_endline (string_ck ck_id);
    List.iter (fun ck -> print_endline (string_ck ck)) ck_l;*)
    if List.for_all (function |CK_on (ck', _, id') -> equal_ck ck' ck_id && id' = id |_ -> false) ck_l then
      ck_id, {e with pexpr_desc = PE_merge(id, merge_l'); pexpr_clk = ck_id}
    else
      raise (Bad_Clock (e.pexpr_loc, "Merge : " ^ (List.fold_left (fun out c -> "(" ^ (string_ck c) ^ "), ") "" ck_l)))
;;

let mk_env p_l env =
  List.fold_left (fun env p -> IdentMap.add p.param_id p.param_ck env) env p_l
;;

let mk_env_const env const = IdentMap.add (fst const) CK_base env;;

let check_eq env eq =
  let node_env, var_env = env in
(*  (match eq.peq_patt.ppatt_desc with
  |PP_ident(id) ->
    print_endline ("EQUATION DE " ^ id)
  |PP_tuple(id_l) -> print_endline ((List.fold_left (fun out id -> out ^ id ^ ", ") "(" id_l) ^ ")"));*)
  let ck, e' = check_clock (node_env, var_env) eq.peq_expr in
  let eq' = {eq with peq_expr = e';} in
  match eq.peq_patt.ppatt_desc with
  |PP_ident(id) ->
    print_string ("AQUI "^ id ^ "\n");
    let ck_id = IdentMap.find id var_env in
    if ck_id = CK_free then
      eq', node_env, IdentMap.add id ck var_env
    else if ck = CK_free then
      eq', node_env, var_env
    else if ck_id = ck then
      eq', node_env, var_env
    else
      raise
        (Bad_Clock
          (eq.peq_patt.ppatt_loc,
          "Variable : " ^ id ^ " incompatibility clock " ^ (string_ck ck_id) ^ " <> " ^ (string_ck ck)))
  |PP_tuple(id_l) -> begin
    match ck with
    |CK_tuple(ck_l) ->
    print_string "ESTA\n";
(*      print_list print_string id_l;
      print_list (fun ck -> print_string (string_ck ck)) ck_l;*)
      let rec check_clock_list id_l ck_l var_env =
        match id_l, ck_l with
        |[], [] -> eq', node_env, var_env
        |id::t1, ck'::t2 ->
          let ck_id = IdentMap.find id var_env in
          if ck_id = CK_free then
            check_clock_list t1 t2 (IdentMap.add id ck' var_env)
          else if ck_id = ck' then
            check_clock_list t1 t2 var_env
          else
            raise
              (Bad_Clock
                (eq.peq_patt.ppatt_loc,
                "Variable : " ^ id ^ " incompatibility clock " ^ (string_ck ck_id) ^ " <> " ^ (string_ck ck')))
        |_ -> raise (Bad_Clock (eq.peq_patt.ppatt_loc, "Tuple : different length between infered clock and paramter clock !"))
      in check_clock_list id_l ck_l var_env
    |_ -> raise (Bad_Clock (eq.peq_patt.ppatt_loc, "Parameter tuple : no clock tuple infered " ^ (string_ck ck)))
  end
;;

let mk_env p_l env =
  List.fold_left (fun env p -> IdentMap.add p.param_id p.param_ck env) env p_l
;;

let mk_env_const env const = IdentMap.add (fst const) CK_base env;;

let mk_env_node node_l =
  List.fold_left (fun out node -> IdentMap.add node.pn_name node out) IdentMap.empty node_l
;;

let check_clock_node env n =
  let node_env, var_env = env in
  let var_env = mk_env n.pn_input (mk_env n.pn_output (mk_env n.pn_local var_env)) in
  let eq_l, _, _ =
    List.fold_left
      (fun (eq_l, node_env, var_env) eq ->
        let eq', node_env, var_env = check_eq (node_env, var_env) eq in
        eq'::eq_l, node_env, var_env) ([], node_env, var_env) n.pn_equs
  in
  {n with pn_equs = eq_l}
;;

let check_clock_file f =
  let typemap, node_l = f in
  let node_env = mk_env_node node_l in
  typemap, List.map (check_clock_node (node_env, IdentMap.empty)) node_l
;;
