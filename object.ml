(*
########
Copyright © 2017

This file is part of MiniLucy.
MiniLucy is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License v3 as published by
the Free Software Foundation.

Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########
*)

open Ast_type;;
open Ast;;
open Ast_object;;

exception Object_Error of string;;

let rec control ck instr =
  match ck with
  |CK_base -> instr
  |CK_on(ck', enum_id, id) -> control ck' (IOBJ_case(id, [enum_id, instr]))
  |CK_tuple(ck_l) -> assert false
  |CK_free -> assert false
;;

let remove_l p l =
  let rec loop l out =
    match l with
    |[] -> None, List.rev_append l out
    |h::t ->
      if p h then
        Some(h), List.rev_append t out
      else
        loop t (h::out)
  in loop l []
;;

let rec merge_case case_l1 case_l2 =
  let rec loop case_l1 case_l2 out =
    match case_l1, case_l2 with
    |[], [] -> out
    |[], h2::t2 -> loop case_l1 t2 (h2::out)
    |h1::t1, [] -> loop t1 case_l2 (h1::out)
    |(enum_id1, instr1)::t1, h2::t2 -> begin
      let elem_of2, new_case_l2 = remove_l (fun (enum_id2, instr2) -> enum_id2 = enum_id1) case_l2 in
      match elem_of2 with
      |None -> loop t1 new_case_l2 ((enum_id1, instr1)::out)
      |Some((enum_id2, instr2)) -> loop t1 new_case_l2 ((enum_id1, join instr1 instr2)::out)
    end
  in loop case_l1 case_l2 []
and join s1 s2 =
  match s1, s2 with
  |IOBJ_case(id1, case_l1), IOBJ_case(id2, case_l2) when id1 = id2 -> IOBJ_case(id1, merge_case case_l1 case_l2)
  |_ -> IOBJ_sequence(s1, s2)
;;

let rec join_list instr_l =
  match instr_l with
  |[] -> IOBJ_skip
  |h::t -> join h (join_list t)
;;

let rec trans_expr env e =
  let m, si, j, d, s = env in
  match e.pexpr_desc with
  |PE_const(c) -> OBJ_const(c)
  |PE_ident(id) ->
    if IdentMap.mem id m then
      OBJ_state(id)
    else if IdentMap.mem id d then
      OBJ_ident(id)
    else
      raise (Object_Error ("the ident " ^ id ^ " is neither a variable nor a memory"))
  |PE_op(op, e1) -> OBJ_op(op, trans_expr env e1)
  |PE_binop(op, e1, e2) -> OBJ_binop(op, trans_expr env e1, trans_expr env e2)
  |PE_app(id, e_l, id_reset) -> assert false
  |PE_fby(c, e1) -> assert false
  |PE_tuple(e_l) -> assert false
  |PE_when(e1, enum_id, id) -> trans_expr env e1
  |PE_current(e1) -> assert false
  |PE_merge(id, merge_l) -> assert false

and trans_merge env var_id e =
  match e.pexpr_desc with
  |PE_merge(id, merge_l) -> IOBJ_case(id, List.map (fun (merge_id, merge_e) -> merge_id, trans_merge env var_id merge_e) merge_l)
  |_ -> IOBJ_var_affect(var_id, trans_expr env e)

and trans_eq env eq =
  let m, si, j, d, s = env in
  let e = eq.peq_expr in
  let pat = eq.peq_patt in
  match e.pexpr_desc with
  |PE_fby(c, e1) -> begin
    match pat.ppatt_desc with
    |PP_ident(id) ->
      let new_m = IdentMap.add id e.pexpr_ty m in
      let new_si = (IOBJ_state_affect(id, OBJ_const(c)))::si in
      let new_s = (control e.pexpr_clk (IOBJ_state_affect(id, trans_expr env e1)))::s in
      new_m, new_si, j, d, new_s
    |PP_tuple(id_l) -> assert false
  end
  |PE_app(id, e_l, id_reset) -> begin
    let pattern =
      match pat.ppatt_desc with
      |PP_ident(pid) -> [pid]
      |PP_tuple(pid_l) -> pid_l
    in
    let new_instance = gen_new_id () in
    let new_e_l = List.map (fun e1 -> trans_expr env e1) e_l in
    let new_si = (IOBJ_reset(new_instance))::si in
    let new_j = IdentMap.add new_instance id j in
    let new_s = (control e.pexpr_clk (IOBJ_step(pattern, new_instance, new_e_l)))::s in
    m, new_si, new_j, d, new_s
  end
  |_ -> begin
    match pat.ppatt_desc with
    |PP_ident(pid) ->
      let new_s = (control e.pexpr_clk (trans_merge env pid e))::s in
      m, si, j, d, new_s
    |PP_tuple(pid_l) -> assert false
  end
;;

let rec trans_eqs env eqs = List.fold_left trans_eq env eqs;;

let trans_node node =
  let new_inputs = List.map (fun input -> input.param_id, input.param_ty) node.pn_input in
  let new_outputs = List.map (fun output -> output.param_id, output.param_ty) node.pn_output in
  let var_map =
    List.fold_left
      (fun out v -> IdentMap.add v.param_id v.param_ty out)
      IdentMap.empty
      (List.rev_append node.pn_input (List.rev_append node.pn_local node.pn_output)) in
  let m, si, j, d, s = trans_eqs (IdentMap.empty, [], IdentMap.empty, var_map, []) node.pn_equs in
  let name = node.pn_name in
  let memory = IdentMap.bindings m in
  let instance = IdentMap.bindings j in
  let reset = si in
  let step = (new_inputs, new_outputs, IdentMap.bindings d, join_list (List.rev s)) in
  name, memory, instance, reset, step
;;

let trans_file f =
  let ty_map, node_l = f in
  ty_map, List.map trans_node node_l
;;
