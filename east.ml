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

exception Merge_Inconsistency;;

type location = Lexing.position * Lexing.position;;

type ident = string;;

exception Variable_Not_Found of ident;;

module TypeMap = Map.Make(struct
  type t = ident;;
  let compare = String.compare;;
end);;

type lustre_ty =
  |Tint
  |Treal
  |Ttype of ident
;;

type enum_ty = string list option;;

type ty = lustre_ty list;;

type const =
  |Cint of int
  |Creal of float
;;

type clock_t =
  |CK_base
  |CK_on of clock_t * ident * ident
  |CK_tuple of clock_t list
;;

type op =
  |Op_eq |Op_neq |Op_lt |Op_le |Op_gt |Op_ge
  |Op_add |Op_sub |Op_mul |Op_div |Op_mod
  |Op_add_f |Op_sub_f |Op_mul_f |Op_div_f
  |Op_not
  |Op_and |Op_or |Op_impl
;;

type p_patt = {
  ppatt_desc: p_patt_desc;
  ppatt_loc: location;
}
and p_patt_desc =
  |PP_ident of ident
  |PP_tuple of ident list
and p_equation = {
  peq_patt: p_patt;
  peq_expr: p_expr;
}
and p_decl = {
  pdecl_desc : p_decl_desc;
  pdecl_loc : location;
}
and p_decl_desc =
  |PD_and of p_decl * p_decl
  |PD_eq of p_equation
  |PD_clk of ident * p_expr
  |PD_let_in of p_decl * p_decl
  |PD_match of p_expr * (ident * p_decl) list
  |PD_reset of p_decl * p_expr
  |PD_automaton of (ident * p_shared_var * p_strong_cond) list
and p_expr = {
  pexpr_desc : p_expr_desc;
  pexpr_clk : clock_t option;
  pexpr_loc : location;
}
and p_expr_desc =
  |PE_const of const
  |PE_ident of ident
  |PE_op of op * p_expr list
  |PE_if of p_expr * p_expr * p_expr
  |PE_app of ident * p_expr list
  |PE_arrow of p_expr * p_expr
  |PE_pre of p_expr
  |PE_tuple of p_expr list
  |PE_when of p_expr * ident * p_expr
  |PE_merge of p_expr * (ident * p_expr) list
  |PE_last of ident
  |PE_let_in of p_decl * p_expr
and p_shared_var =
  |PSV_let of p_decl * p_shared_var
  |PSV_do of p_decl * p_weak_cond
and p_strong_cond =
  |PSC_unless_then of p_expr * ident * p_strong_cond
  |PSC_unless_cont of p_expr * ident * p_strong_cond
  |PSC_epsilon
and p_weak_cond =
  |PWC_until_then of p_expr * ident * p_weak_cond
  |PWC_until_cont of p_expr * ident * p_weak_cond
  |PWC_epsilon
;;

type param = {
  param_id : ident;
  param_ty : lustre_ty;
  param_ck : clock_t option;
}

type p_node = {
  pn_name: ident;
  pn_input: param list;
  pn_output: param list;
  pn_local: param list;
  pn_equs: p_equation list;
  pn_loc: location;
};;

type p_const = ident * p_expr;;

type p_file = p_decl;;
