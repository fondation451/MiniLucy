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

let rec normalize_exp e vars eqs =
  let substitute_with_id () =
    let new_id = gen_new_id () in
    let new_id_ty = e.pexpr_ty in
    let new_id_ck = e.pexpr_clk in
    let new_eq = {peq_expr = e; peq_patt = {ppatt_desc = PP_ident(new_id); ppatt_loc = Lexing.dummy_pos, Lexing.dummy_pos;};} in
    let vars, eqs = normalize_eq new_eq vars eqs in
    {e with pexpr_desc = PE_ident(new_id)}, {param_id = new_id; param_ty = new_id_ty; param_ck = new_id_ck;}::vars, eqs
  in
(*print_endline ("normalize_exp");*)
  match e.pexpr_desc with
  |PE_const(c) -> e, vars, eqs
  |PE_ident(id) -> e, vars, eqs
  |PE_op(op, e1) ->
    let e1', vars, eqs = normalize_exp e1 vars eqs in
    {e with pexpr_desc = PE_op(op, e1')}, vars, eqs
  |PE_binop(op, e1, e2) ->
    let e1', vars, eqs = normalize_exp e1 vars eqs in
    let e2', vars, eqs = normalize_exp e2 vars eqs in
    {e with pexpr_desc = PE_binop(op, e1', e2')}, vars, eqs
  |PE_app(id, e_l) -> substitute_with_id ()
  |PE_fby(c, e2) -> substitute_with_id ()
  |PE_tuple(e_l) ->
    let e_l', vars, eqs =
      List.fold_left
        (fun (out, vars, eqs) e ->
          let e', vars, eqs = normalize_exp e vars eqs in
          e'::out, vars, eqs) ([], vars, eqs) e_l
    in
    {e with pexpr_desc = PE_tuple(List.rev e_l)}, vars, eqs
  |PE_when(e1, enum_id, id) ->
    let e1', vars, eqs = normalize_exp e1 vars eqs in
    {e with pexpr_desc = PE_when(e1', enum_id, id)}, vars, eqs
  |PE_current(e1) ->
    let e1', vars, eqs = normalize_exp e1 vars eqs in
    {e with pexpr_desc = PE_current(e1')}, vars, eqs
  |PE_merge(id, enum_l) -> substitute_with_id ()

and normalize_merge e vars eqs =
(*print_endline ("normalize_merge");*)
  match e.pexpr_desc with
  |PE_merge(id, enum_l) ->
    let enum_l', vars, eqs =
      List.fold_left
        (fun (out, vars, eqs) (enum_id, e1) ->
          let e1', vars, eqs = normalize_merge e1 vars eqs in
          (enum_id, e1')::out, vars, eqs) ([], vars, eqs) enum_l
    in
    {e with pexpr_desc = PE_merge(id, List.rev enum_l')}, vars, eqs
  |_ -> normalize_exp e vars eqs

and normalize_eq eq vars eqs =
  let e = eq.peq_expr in
  let pat = eq.peq_patt.ppatt_desc in
  match pat with
  |PP_ident(pid) -> begin
(*print_endline ("normalize_eq " ^ pid);*)
    match e.pexpr_desc with
    |PE_fby(c, e2) ->
      let e2', vars, eqs = normalize_exp e2 vars eqs in
      let new_eq = {eq with peq_expr = {e with pexpr_desc = PE_fby(c, e2')}} in
      vars, new_eq::eqs
    |PE_app(id, e_l) ->
      let e_l', vars, eqs =
        List.fold_left
          (fun (out, vars, eqs) e1 ->
            let e1', vars, eqs = normalize_exp e1 vars eqs in
            e1'::out, vars, eqs) ([], vars, eqs) e_l
      in
      let new_eq = {eq with peq_expr = {e with pexpr_desc = PE_app(id, List.rev e_l')}} in
      vars, new_eq::eqs
    |_ ->
      let e', vars, eqs = normalize_merge e vars eqs in
      let new_eq = {eq with peq_expr = e'} in
      vars, new_eq::eqs
  end
  |PP_tuple(pid_l) -> begin
    match e.pexpr_desc with
    |PE_app(id, e_l) ->
      let e_l', vars, eqs =
        List.fold_left
          (fun (out, vars, eqs) e1 ->
            let e1', vars, eqs = normalize_exp e1 vars eqs in
            e1'::out, vars, eqs) ([], vars, eqs) e_l
      in
      let new_eq = {eq with peq_expr = {e with pexpr_desc = PE_app(id, e_l')}} in
      vars, new_eq::eqs
    |PE_tuple(e_l) ->
      List.fold_left2
        (fun (vars, eqs) id e1 ->
          let eq1 = {peq_expr = e1; peq_patt = {eq.peq_patt with ppatt_desc = PP_ident(id)}} in
          let vars, eqs = normalize_eq eq1 vars eqs in
          vars, eqs) (vars, eqs) pid_l e_l
    |_ -> assert false
  end
;;

let normalize_node node =
(*  print_endline ("normalize_node " ^ node.pn_name);*)
  let vars, eqs = List.fold_left (fun (vars, eqs) eq -> normalize_eq eq vars eqs) ([], []) node.pn_equs in
  {node with pn_local = List.rev_append vars node.pn_local; pn_equs = eqs}
;;

let normalize_file f =
  let typemap, node_l = f in
  typemap, List.map normalize_node node_l
;;
