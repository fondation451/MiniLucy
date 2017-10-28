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

type location = Lexing.position * Lexing.position;;

type ident = string;;

let ident_from_string str = str;;

let gen_new_id =
  let cpt = ref 0 in fun () -> incr cpt; "__aux"^(string_of_int !cpt)
;;

exception Variable_Not_Found of ident;;

module IdentMap = Map.Make(struct
  type t = ident;;
  let compare = String.compare;;
end);;

type lustre_ty =
  |Tint
  |Treal
  |Ttype of ident
  |Ttuple of lustre_ty list
;;

let lustre_bool_type = Ttype "bool";;
let lustre_bool_true = "True";;
let lustre_bool_false = "False";;

type enum_ty = string list;;

type ty = lustre_ty list;;

type const =
  |Cint of int
  |Creal of float
  |Cenum of ident
;;

type clock_t =
  |CK_base
  |CK_free
  |CK_on of clock_t * ident * ident
  |CK_tuple of clock_t list
;;

type uop =
  |UOp_not
  |UOp_minus
;;

type op =
  |Op_eq |Op_neq |Op_lt |Op_le |Op_gt |Op_ge
  |Op_add |Op_sub |Op_mul |Op_div |Op_mod
  |Op_and |Op_or |Op_impl
;;

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
  |PE_app of ident * p_expr list
  |PE_fby of p_expr * p_expr
  |PE_tuple of p_expr list
  |PE_when of p_expr * ident * ident
  |PE_current of p_expr
  |PE_merge of ident * (ident * p_expr) list
;;

type p_patt = {
  ppatt_desc: p_patt_desc;
  ppatt_loc: location;
}
and p_patt_desc =
  |PP_ident of ident
  |PP_tuple of ident list
;;

type p_equation = {
  peq_patt: p_patt;
  peq_expr: p_expr;
};;

type param = {
  param_id : ident;
  param_ty : lustre_ty;
  param_ck : clock_t;
}

type p_node = {
  pn_name: ident;
  pn_input: param list;
  pn_output: param list;
  pn_local: param list;
  pn_equs: p_equation list;
  pn_loc: location;
};;

type p_const = ident * const;;

type p_file = enum_ty IdentMap.t * p_const list * p_node list;;
