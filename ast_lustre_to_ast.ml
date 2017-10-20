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
open Ast_lustre;;

Random.self_init ();;

let gen_new_id () =
  "__" ^ (string_of_int (Random.bits ())) ^ "id" ^ (string_of_int (Random.bits ())) ^ "__"
;;

let eq_lustre_to_eq eq =
  let out = ref [] in
  let rec expr_lustre_to_expr e =
    let mk_expr_from e e_v = {
      pexpr_desc = e_v;
      pexpr_clk = e.pexpr_lustre_clk;
      pexpr_loc = e.pexpr_lustre_loc;
    } in
    match e.pexpr_lustre_desc with
    |PEL_const c -> mk_expr_from e (PE_const c)
    |PEL_ident id -> mk_expr_from e (PE_ident id)
    |PEL_op (op, e_l) -> mk_expr_from e (PE_op (op, List.rev (List.rev_map expr_lustre_to_expr e_l)))
    |PEL_if (e1, e2, e3) ->
      let new_id = gen_new_id () in
      let new_eq = {
        peq_patt = {ppatt_desc = PP_ident new_id; ppatt_loc = eq.peq_lustre_patt.ppatt_loc;};
        peq_expr = expr_lustre_to_expr e1;
      } in
      out := new_eq::(!out);
      mk_expr_from e
        (PE_merge
          (new_id,
            [(ident_from_string "True", expr_lustre_to_expr e2);
             (ident_from_string "False", expr_lustre_to_expr e3)]))
    |PEL_app (id, e_l) -> mk_expr_from e (PE_app (id, List.rev (List.rev_map expr_lustre_to_expr e_l)))
    |PEL_arrow (e1, e2) -> mk_expr_from e (PE_arrow (expr_lustre_to_expr e1, expr_lustre_to_expr e2))
    |PEL_pre e' -> mk_expr_from e (PE_pre (expr_lustre_to_expr e'))
    |PEL_tuple e_l -> mk_expr_from e (PE_tuple (List.rev (List.rev_map expr_lustre_to_expr e_l)))
    |PEL_when (e1, enum_id, e2) ->
      let new_id = gen_new_id () in
      let new_eq = {
        peq_patt = {ppatt_desc = PP_ident new_id; ppatt_loc = eq.peq_lustre_patt.ppatt_loc;};
        peq_expr = expr_lustre_to_expr e2;
      } in
      out := new_eq::(!out);
      mk_expr_from e (PE_when (expr_lustre_to_expr e1, enum_id, new_id))
    |PEL_current e' -> mk_expr_from e (PE_current (expr_lustre_to_expr e'))
    |PEL_merge (id, merge_l) ->
      mk_expr_from e (PE_merge (id, List.rev (List.rev_map (fun (enum_id, e') -> (enum_id, expr_lustre_to_expr e')) merge_l)))
  in
  let new_eq = {
    peq_patt = {ppatt_desc = eq.peq_lustre_patt.ppatt_desc; ppatt_loc = eq.peq_lustre_patt.ppatt_loc;};
    peq_expr = expr_lustre_to_expr eq.peq_lustre_expr;
  } in
  List.rev (new_eq::(!out))
;;

let eqs_lustre_to_eqs eqs =
  List.rev (List.fold_left (fun out eq -> List.rev_append (eq_lustre_to_eq eq) out) [] eqs)
;;

let node_lustre_to_node node = {
  pn_name = node.pn_lustre_name;
  pn_input = node.pn_lustre_input;
  pn_output = node.pn_lustre_output;
  pn_local = node.pn_lustre_local;
  pn_equs = eqs_lustre_to_eqs node.pn_lustre_equs;
  pn_loc = node.pn_lustre_loc;
};;

let convert (ty_map, const_l, node_l) =
  (ty_map, const_l, List.rev (List.rev_map node_lustre_to_node node_l))
;;
