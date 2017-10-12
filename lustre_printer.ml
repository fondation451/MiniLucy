(*
########
Copyright © 2017

This file is part of MiniLucy.
MiniLucy is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License v3 as published by
the Free Software Foundation.

Nicolas ASSOUAD <nicolas.assouad@ens.fr>
Clément PASCUTTO <clement.pascutto@ens.fr
########
*)

open Format;;
open Ast;;

let print_id = print_string;;

let print_type t =
  match t with
  |Tbool -> print_string "bool"
  |Tint -> print_string "int"
  |Treal -> print_string "real"
;;

let print_const c =
  match c with
  |Cbool(b) -> if b then print_string "true" else print_string "false"
  |Cint(i) -> print_int i
  |Creal(f) -> print_float f
;;

let print_separated_list print_fn sep l =
  let rec aux = function
    | [] -> ()
    | [h] -> print_fn h;
    | h::t -> print_fn h; print_string sep; aux t;
  in
  aux l
;;

let string_of_op o =
  match o with
  |Op_eq -> "="
  |Op_neq -> "<>"
  |Op_lt -> "<"
  |Op_le -> "<="
  |Op_gt -> ">"
  |Op_ge -> ">="
  |Op_add |Op_add_f -> "+"
  |Op_sub |Op_sub_f -> "-"
  |Op_mul |Op_mul_f -> "*"
  |Op_div |Op_div_f -> "/"
  |Op_mod -> "mod"
  |Op_not -> "not"
  |Op_and -> "and"
  |Op_or -> "or"
  |Op_impl -> "=>"
;;

let rec print_expr e =
  match e.pexpr_desc with
  |PE_const(c) -> print_const c
  |PE_ident(id) -> print_id id
  |PE_op(op, exp_l) ->
    print_string "(";
    print_separated_list print_expr (" " ^ (string_of_op op) ^ " ") exp_l;
    print_string ")"
  |PE_if(e, e', e'') ->
    print_string "if ";
    print_expr e;
    print_string " then ";
    print_expr e';
    print_string " else ";
    print_expr e''
  |PE_app(id, exp_l) ->
    print_id id;
    print_string "(";
    print_separated_list print_expr ", " exp_l;
    print_string ")"
  |PE_arrow(e, e') ->
    print_expr e;
    print_string " -> ";
    print_expr e'
  |PE_pre(e) ->
    print_string "pre (";
    print_expr e;
    print_string ")"
  |PE_tuple(exp_l) ->
    print_string "(";
    print_separated_list print_expr ", " exp_l;
    print_string ")"
  |PE_when(e, e') ->
    print_expr e;
    print_string " when ";
    print_expr e'
  |PE_current(e) ->
    print_string "current ";
    print_expr e
  |PE_merge(id, e, e') ->
    print_string "merge ";
    print_id id;
    print_string " (true -> ";
    print_expr e;
    print_string ") (false -> ";
    print_expr e';
    print_string ")"
;;

let print_equation e =
  (match e.peq_patt.ppatt_desc with
     |PP_ident(id) -> print_id id
     |PP_tuple(id_l) -> print_separated_list print_id ", " id_l);
  print_string " = ";
  print_expr e.peq_expr
;;

let print_var_decl v =
  let ident, base_ty, when_expr = v in
  print_id ident;
  print_string ": ";
  print_type base_ty;
  (match when_expr with
   |None -> ()
   |Some(e) ->
     print_string " when ";
     print_expr e)
;;

let print_node n =
  print_string "node ";
  print_id n.pn_name;
  print_space ();
  print_string "(";
  print_separated_list print_var_decl ", " n.pn_input;
  print_string ")";
  print_space ();
  print_string "returns (";
  print_separated_list print_var_decl ", " n.pn_output;
  print_string ");\n";
  print_string "var ";
  open_hovbox 2;
    print_separated_list print_var_decl ";\n" n.pn_local;
    print_string ";\n";
  close_box ();
  print_string "let";
  print_newline ();
  open_hovbox 2;
    print_separated_list print_equation ";\n" n.pn_equs;
    print_string ";\n";
  close_box ();
  print_string "tel"
;;

let print_const_dec const_dec =
  let id, e = const_dec in
  print_string "const ";
  print_id id;
  print_string " = ";
  print_expr e
;;

let print_lustre f =
  let const_decs, node_decs = f in
  open_hovbox 0;
    print_separated_list print_const_dec ";\n" const_decs;
    print_newline ();
    print_separated_list print_node "\n\n" node_decs;
    print_newline ();
  close_box ()
;;
