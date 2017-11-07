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

type p_patt = {
  ppatt_desc: p_patt_desc;
  ppatt_loc: location;
}
and p_patt_desc =
  |PP_ident of ident
  |PP_tuple of ident list
;;

type param = {
  param_id : ident;
  param_ty : lustre_ty;
  param_ck : clock_t;
}

type p_expr = {
  pexpr_desc : p_expr_desc;
  pexpr_ty : lustre_ty;
  pexpr_clk : clock_t;
  pexpr_loc : location;
}
and p_expr_desc =
  |PE_const of const
  |PE_ident of ident
  |PE_op of uop * p_expr
  |PE_binop of op * p_expr * p_expr
  |PE_app of ident * p_expr list * ident
  |PE_fby of const * p_expr
  |PE_tuple of p_expr list
  |PE_when of p_expr * ident * ident
  |PE_current of p_expr
  |PE_merge of ident * (ident * p_expr) list
;;

type p_equation = {
  peq_patt: p_patt;
  peq_expr: p_expr;
};;

type p_node = {
  pn_name: ident;
  pn_input: param list;
  pn_output: param list;
  pn_local: param list;
  pn_equs: p_equation list;
  pn_loc: location;
};;

type p_file = enum_ty IdentMap.t * p_node list;;
