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

open Ast;;

exception Bad_Clock of location * string;;

module ParamMap = Map.Make(struct
  type t = ident;;
  let compare = String.compare;;
end);;

let find_param_ck env id = try ParamMap.find id env with Not_found -> raise (Variable_Not_Found id);;

let rec string_ck ck =
  match ck with
  |CK_base -> "BASE"
  |CK_free -> "FREE"
  |CK_on(ck', enum_id, id) -> (string_ck ck') ^ " on " ^ enum_id ^ "(" ^ id ^ ")"
  |CK_tuple(ck_l) -> (List.fold_left (fun out ck' -> out ^ (string_ck ck') ^ ", ") "(" ck_l) ^ ")"
;;

let rec check_clock env e =
  match e.pexpr_desc with
  |PE_const(c) -> CK_base , {e with pexpr_clk = CK_base}
  |PE_ident(id) ->
    let ck = find_param_ck env id in
    ck, {e with pexpr_clk = ck}
  |PE_op(op, e1) ->
    let ck1, e1' = check_clock env e1 in
    ck1, {e with pexpr_desc = PE_op(op, e1'); pexpr_clk = ck1}
  |PE_binop(op, e1, e2) ->
    let ck1, e1' = check_clock env e1 in
    let ck2, e2' = check_clock env e2 in
    if ck1 = ck2 then
      ck1, {e with pexpr_desc = PE_binop(op, e1', e2'); pexpr_clk = ck1}
    else
      raise (Bad_Clock (e.pexpr_loc, "Binary operator : different clocks " ^ (string_ck ck1) ^ " <> " ^ (string_ck ck2)))
  |PE_app(id, e_l) ->
    let same_elem l ck_out =
      let rec loop x l ck_out =
        match l with |[] -> CK_tuple (x::ck_out), true |h::t -> if x = h then loop x t (x::ck_out) else CK_base, false in
      match l with |[] -> CK_base, false |h::t -> loop h t []
    in
    let ck_l, e_l' = List.split (List.map (check_clock env) e_l) in
    let ck, is_ok = same_elem ck_l [] in
    if is_ok then
      ck, {e with pexpr_desc = PE_app(id, e_l'); pexpr_clk = ck}
    else
      raise (Bad_Clock (e.pexpr_loc, "Application : " ^ (List.fold_left (fun out c -> out ^ (string_ck c) ^ ", ") "(" ck_l) ^ ")"))
  |PE_arrow(e1, e2) ->
    let ck1, e1' = check_clock env e1 in
    let ck2, e2' = check_clock env e2 in
    if ck1 = CK_base then
      ck2, {e with pexpr_desc = PE_arrow(e1', e2'); pexpr_clk = ck2}
    else
      raise (Bad_Clock (e.pexpr_loc, "Arrow : " ^ (string_ck ck1)))
  |PE_pre(e) ->
    let ck, e' = check_clock env e in
    ck, {e with pexpr_desc = PE_pre(e'); pexpr_clk = ck}
  |PE_tuple(e_l) ->
    let rec mk_ck l ck_out = match l with |[] -> CK_tuple (ck_out) |h::t -> mk_ck t (h::ck_out) in
    let ck_l, e_l' = List.split (List.map (check_clock env) e_l) in
    let ck = mk_ck ck_l [] in
    ck, {e with pexpr_desc = PE_tuple e_l'; pexpr_clk =  ck}
  |PE_when(e1, enum_id, id) ->
    let ck1, e1' = check_clock env e1 in
    let ck_id = find_param_ck env id in
    if ck1 = ck_id then
      let ck = CK_on (ck1, enum_id, id) in
      ck, {e with pexpr_desc = PE_when(e1', enum_id, id); pexpr_clk = ck}
    else
      raise (Bad_Clock (e.pexpr_loc, "When : " ^ (string_ck ck1) ^ " <> " ^ (string_ck ck_id)))
  |PE_current(e) ->
    let ck, e' = check_clock env e in
    ck, {e with pexpr_desc = PE_current(e'); pexpr_clk = ck}
  |PE_merge(id, merge_l) ->
    let ck_id = find_param_ck env id in
    let ck_l, merge_l' = List.split (List.map (fun (id, e') -> let ck, e'' = check_clock env e' in (ck, (id, e''))) merge_l) in
    print_endline (string_ck ck_id);
    List.iter (fun ck -> print_endline (string_ck ck)) ck_l;
    if List.for_all (function |CK_on (ck', _, id') -> ck' = ck_id && id' = id |_ -> false) ck_l then
      ck_id, {e with pexpr_desc = PE_merge(id, merge_l'); pexpr_clk = ck_id}
    else
      raise (Bad_Clock (e.pexpr_loc, "Merge : " ^ (List.fold_left (fun out c -> "(" ^ (string_ck c) ^ "), ") "" ck_l)))
;;

let mk_env p_l env =
  List.fold_left (fun env p -> ParamMap.add p.param_id p.param_ck env) env p_l
;;

let mk_env_const env const = ParamMap.add (fst const) CK_base env;;

let check_eq env eq =
  (match eq.peq_patt.ppatt_desc with
  |PP_ident(id) ->
    print_endline ("EQUATION DE " ^ id)
  |PP_tuple(id_l) -> print_endline ((List.fold_left (fun out id -> out ^ id ^ ", ") "(" id_l) ^ ")"));
  let ck, e' = check_clock env eq.peq_expr in
  match eq.peq_patt.ppatt_desc with
  |PP_ident(id) ->
    let ck_id = ParamMap.find id env in
    if ck_id <> ck then
      raise
        (Bad_Clock
          (eq.peq_patt.ppatt_loc,
          "Variable : " ^ id ^ " incompatibility clock " ^ (string_ck ck_id) ^ " <> " ^ (string_ck ck)))
  |PP_tuple(id_l) -> begin
    match ck with
    |CK_tuple(ck_l) ->
      let rec check_clock_list id_l ck_l =
        match id_l, ck_l with
        |[], [] -> ()
        |id::t1, ck'::t2 ->
          let ck_id = ParamMap.find id env in
          if ck_id <> ck' then
            raise
              (Bad_Clock
                (eq.peq_patt.ppatt_loc,
                "Variable : " ^ id ^ " incompatibility clock " ^ (string_ck ck_id) ^ " <> " ^ (string_ck ck')))
        |_ -> raise (Bad_Clock (eq.peq_patt.ppatt_loc, "Tuple : different length between infered clock and paramter clock !"))
      in check_clock_list id_l ck_l
    |_ -> raise (Bad_Clock (eq.peq_patt.ppatt_loc, "Parameter tuple : no clock tuple infered " ^ (string_ck ck)))
  end
;;

let check_clock_node env n =
  let env = mk_env n.pn_input (mk_env n.pn_output (mk_env n.pn_local env)) in
  List.iter (check_eq env) n.pn_equs
;;

let check_clock_file f =
  let typemap, const_l, node_l = f in
  let env = List.fold_left mk_env_const (ParamMap.empty) const_l in
  List.iter (check_clock_node env) node_l
;;
