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

type p_expr_lustre = {
  pexpr_lustre_desc : p_expr_lustre_desc;
  pexpr_lustre_clk : clock_t;
  pexpr_lustre_loc : location;
}
and p_expr_lustre_desc =
  |PEL_const of const
  |PEL_ident of ident
  |PEL_op of uop * p_expr_lustre
  |PEL_binop of op * p_expr_lustre * p_expr_lustre
  |PEL_if of p_expr_lustre * p_expr_lustre * p_expr_lustre
  |PEL_app of ident * p_expr_lustre list
  |PEL_fby of const * p_expr_lustre
  |PEL_tuple of p_expr_lustre list
  |PEL_when of p_expr_lustre * ident * p_expr_lustre
  |PEL_current of p_expr_lustre
  |PEL_merge of ident * (ident * p_expr_lustre) list
;;

type p_equation_lustre = {
  peq_lustre_patt: p_patt;
  peq_lustre_expr: p_expr_lustre;
};;

type p_node_lustre = {
  pn_lustre_name: ident;
  pn_lustre_input: param list;
  pn_lustre_output: param list;
  pn_lustre_local: param list;
  pn_lustre_equs: p_equation_lustre list;
  pn_lustre_loc: location;
};;

type p_const = ident * const;;

type p_file_lustre = enum_ty IdentMap.t * p_const list * p_node_lustre list;;
