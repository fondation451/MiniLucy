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

type enum_ty = string list;;

type elustre_ty =
  |Tint
  |Treal
  |Ttype of ident
  |Tfun of elustre_ty * elustre_ty;;

let none_type = Ttype "__None__";; (* dummy type that means "not typed yet" *)
let bool_type   = Ttype "bool";;
let const_true  = Cenum (lustre_bool_true);;
let const_false = Cenum (lustre_bool_false);;

type ident_ty = ident * elustre_ty;;

module IdentTySet = Set.Make(struct
  type t = ident_ty;;
  let compare = fun (a, b) (c, d) -> String.compare a c;;
end);;

type param = {
  param_id : ident;
  param_ty : elustre_ty;
  param_ck : clock_t;
}

type p_equation = {
  peq_patt: p_patt;
  peq_expr: p_expr;
}
and p_decl = {
  pdecl_desc : p_decl_desc;
  pdecl_loc : location;
}
and p_decl_desc =
  |PD_skip
  |PD_and of p_decl * p_decl
  |PD_eq of p_equation
  |PD_clk of ident_ty * p_expr
  |PD_let_in of p_decl * p_decl
  |PD_match of p_expr * (ident_ty * p_decl) list
  |PD_reset of p_decl * p_expr
  |PD_automaton of (ident_ty * p_shared_var * p_strong_cond) list
and p_expr = {
  pexpr_desc : p_expr_desc;
  pexpr_ty : elustre_ty;
  pexpr_loc : location;
}
and p_expr_desc =
  |PE_const of const
  |PE_ident of ident_ty
  |PE_uop of uop * p_expr
  |PE_bop of op * p_expr * p_expr
  |PE_if of p_expr * p_expr * p_expr
  |PE_app of ident_ty * p_expr list * p_expr
  |PE_fby of const * p_expr
  |PE_pre of p_expr
  |PE_when of p_expr * ident * p_expr
  |PE_merge of p_expr * (ident_ty * p_expr) list
  |PE_last of ident_ty
and p_shared_var =
  |PSV_let of p_decl * p_shared_var
  |PSV_do of p_decl * p_weak_cond
and p_strong_cond =
  |PSC_unless_then of p_expr * ident_ty * p_strong_cond
  |PSC_unless_cont of p_expr * ident_ty * p_strong_cond
  |PSC_epsilon
and p_weak_cond =
  |PWC_until_then of p_expr * ident_ty * p_weak_cond
  |PWC_until_cont of p_expr * ident_ty * p_weak_cond
  |PWC_epsilon
;;

type p_node = {
  pn_name: ident;
  pn_input: param list;
  pn_output: param list;
  pn_local: param list;
  pn_decl: p_decl;
  pn_loc: location;
};;

type p_file = enum_ty IdentMap.t * p_node list;;
