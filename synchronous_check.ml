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

exception Bad_Clock of string;;

module ParamMap = Map.Make(struct
  type t = ident;;
  let compare = String.compare;;
end);;

let find_param_ck env id = try ParamMap.find id env with Not_found -> raise (Variable_Not_Found id);;

let rec check_clock env e =
  let mk_expr exp ck loc = {
    pexpr_desc = exp;
    pexpr_clk = Some(ck);
    pexpr_loc = loc;
  } in
  match e.pexpr_clk with
  |Some(ck) -> ck, e
  |None -> begin
    match e.pexpr_desc with
    |PE_const(c) -> CK_base , mk_expr e.pexpr_desc CK_base e.pexpr_loc
    |PE_ident(id) ->
      let ck = find_param_ck env id in
      ck, mk_expr e.pexpr_desc ck e.pexpr_loc
    |PE_op(op, e_l) ->
      let same_elem l =
        let rec loop x l = match l with |[] -> x, true |h::t -> if x = h then loop x t else CK_base, false in
        match l with |[] -> CK_base, false |h::t -> loop h t
      in
      let ck_l, e_l' = List.split (List.map (check_clock env) e_l) in
      let ck, is_ok = same_elem ck_l in
      if is_ok then
        ck, mk_expr (PE_op(op, e_l')) ck e.pexpr_loc
      else
        raise (Bad_Clock "Operator")
    |PE_app(id, e_l) ->
      let same_elem l ck_out =
        let rec loop x l ck_out =
          match l with |[] -> CK_tuple (x::ck_out), true |h::t -> if x = h then loop x t (x::ck_out) else CK_base, false in
        match l with |[] -> CK_base, false |h::t -> loop h t []
      in
      let ck_l, e_l' = List.split (List.map (check_clock env) e_l) in
      let ck, is_ok = same_elem ck_l [] in
      if is_ok then
        ck, mk_expr (PE_app(id, e_l')) ck e.pexpr_loc
      else
        raise (Bad_Clock "Application")
    |PE_arrow(e1, e2) ->
      let ck1, e1' = check_clock env e1 in
      let ck2, e2' = check_clock env e2 in
      if ck1 = CK_base then
        ck2, mk_expr (PE_arrow(e1', e2')) ck2 e.pexpr_loc
      else
        raise (Bad_Clock "Arrow")
    |PE_pre(e) ->
      let ck, e' = check_clock env e in
      ck, mk_expr (PE_pre(e')) ck e.pexpr_loc
    |PE_tuple(e_l) ->
      let rec mk_ck l ck_out = match l with |[] -> CK_tuple (ck_out) |h::t -> mk_ck t (h::ck_out) in
      let ck_l, e_l' = List.split (List.map (check_clock env) e_l) in
      let ck = mk_ck ck_l [] in
      ck, mk_expr (PE_tuple e_l') ck e.pexpr_loc
    |PE_when(e1, enum_id, id) ->
      let ck1, e1' = check_clock env e1 in
      let ck_id = find_param_ck env id in
      if ck1 = ck_id then
        let ck = CK_on (ck1, enum_id, id) in
        ck, mk_expr (PE_when (e1', enum_id, id)) ck e.pexpr_loc
      else
        raise (Bad_Clock "When")
    |PE_current(e) ->
      let ck, e' = check_clock env e in
      ck, mk_expr (PE_current(e')) ck e.pexpr_loc
    |PE_merge(id, merge_l) ->
      let ck_id = find_param_ck env id in
      let ck_l, merge_l' = List.split (List.map (fun (id, e') -> let ck, e'' = check_clock env e' in (ck, (id, e''))) merge_l) in
      if List.for_all (function |CK_on (ck', _, id') -> ck' = ck_id && id' = id |_ -> false) ck_l then
        ck_id, mk_expr (PE_merge(id, merge_l')) ck_id e.pexpr_loc
      else
        raise (Bad_Clock "Merge")
  end
;;
