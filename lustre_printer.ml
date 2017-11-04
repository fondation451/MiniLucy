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
open Ast_lustre;;

let print_separated_list print_fn sep l =
  let rec aux = function
    | [] -> ()
    | [h] -> print_fn h;
    | h::t -> print_fn h; print_string sep; aux t;
  in
  aux l
;;

let print_id = print_string;;

let rec print_type t =
  match t with
  |Tint -> print_string "int"
  |Treal -> print_string "real"
  |Ttype(id) -> print_id id
  |Ttuple(ty_l) -> print_string "("; print_separated_list print_type ", " ty_l; print_string ")"
;;

let print_const c =
  match c with
  |Cint(i) -> print_int i
  |Creal(f) -> print_float f
  |Cenum(id) -> print_id id
;;

let rec print_ck_const ck_const =
  match ck_const with
  |CK_base -> ()
  |CK_free -> print_string " (FREE!!!) "
  |CK_on(ck_const', enum_id, id) ->
    print_string " when ";
    print_id enum_id;
    print_string "(";
    print_id id;
    print_string ")"
  |CK_tuple(ck_const_l) ->
    print_string "(";
    print_separated_list print_ck_const ", " ck_const_l;
    print_string ")"
;;

let string_of_uop o =
  match o with
  |UOp_not -> "not"
  |UOp_minus -> "-"
;;

let string_of_op o =
  match o with
  |Op_eq -> "="
  |Op_neq -> "<>"
  |Op_lt -> "<"
  |Op_le -> "<="
  |Op_gt -> ">"
  |Op_ge -> ">="
  |Op_add -> "+"
  |Op_sub -> "-"
  |Op_mul -> "*"
  |Op_div -> "/"
  |Op_mod -> "mod"
  |Op_and -> "and"
  |Op_or -> "or"
  |Op_impl -> "=>"
;;

let rec print_expr_lustre e =
  match e.pexpr_lustre_desc with
  |PEL_const(c) -> print_const c
  |PEL_ident(id) -> print_id id
  |PEL_op(op, e1) ->
    print_string "(";
    print_string (string_of_uop op);
    print_string " ";
    print_expr_lustre e1;
    print_string ")"
  |PEL_binop(op, e1, e2) ->
    print_string "(";
    print_expr_lustre e1;
    print_string " ";
    print_string (string_of_op op);
    print_string " ";
    print_expr_lustre e2;
    print_string ")"
  |PEL_if(e, e', e'') ->
    print_string "if ";
    print_expr_lustre e;
    print_string " then ";
    print_expr_lustre e';
    print_string " else ";
    print_expr_lustre e''
  |PEL_app(id, exp_l) ->
    print_id id;
    print_string "(";
    print_separated_list print_expr_lustre ", " exp_l;
    print_string ")"
  |PEL_fby(c, e') ->
    print_const c;
    print_string " fby ";
    print_expr_lustre e'
  |PEL_tuple(exp_l) ->
    print_string "(";
    print_separated_list print_expr_lustre ", " exp_l;
    print_string ")"
  |PEL_when(e, id, e') ->
    print_expr_lustre e;
    print_string " when ";
    print_id id;
    print_string "(";
    print_expr_lustre e';
    print_string ")"
  |PEL_current(e) ->
    print_string "current ";
    print_expr_lustre e
  |PEL_merge(id, e_l) ->
    print_string "merge ";
    print_id id;
    print_string " ";
    print_separated_list (fun (id, e) ->
                            print_string "(";
                            print_id id;
                            print_string " -> ";
                            print_expr_lustre e;
                            print_string ")") " " e_l
;;

let print_equation_lustre e =
  (match e.peq_lustre_patt.ppatt_desc with
     |PP_ident(id) -> print_id id
     |PP_tuple(id_l) -> print_separated_list print_id ", " id_l);
  print_string " = ";
  print_expr_lustre e.peq_lustre_expr
;;

let print_var_decl v =
  print_id v.param_id;
  print_string ": ";
  print_type v.param_ty;
  print_ck_const v.param_ck
;;

let print_node_lustre n =
  print_string "node ";
  print_id n.pn_lustre_name;
  print_space ();
  print_string "(";
  print_separated_list print_var_decl ", " n.pn_lustre_input;
  print_string ")";
  print_space ();
  print_string "returns (";
  print_separated_list print_var_decl ", " n.pn_lustre_output;
  print_string ");\n";
  if List.length n.pn_lustre_local > 0 then print_string "var ";
  open_hovbox 2;
    print_separated_list print_var_decl ";\n" n.pn_lustre_local;
    print_string ";\n";
  close_box ();
  print_string "let";
  print_newline ();
  open_hovbox 2;
    print_separated_list print_equation_lustre ";\n" n.pn_lustre_equs;
    print_string ";\n";
  close_box ();
  print_string "tel"
;;

let print_const_dec const_dec =
  let id, c = const_dec in
  print_string "const ";
  print_id id;
  print_string " = ";
  print_const c
;;

let print_lustre f =
  let type_decs, const_decs, node_decs = f in
  open_hovbox 0;
    print_separated_list print_const_dec ";\n" const_decs;
    print_newline ();
    print_separated_list print_node_lustre "\n\n" node_decs;
    print_newline ();
  close_box ()
;;

let rec print_expr e =
  match e.pexpr_desc with
  |PE_const(c) -> print_const c
  |PE_ident(id) -> print_id id
  |PE_op(op, e1) ->
    print_string "(";
    print_string (string_of_uop op);
    print_string " ";
    print_expr e1;
    print_string ")"
  |PE_binop(op, e1, e2) ->
    print_string "(";
    print_expr e1;
    print_string " ";
    print_string (string_of_op op);
    print_string " ";
    print_expr e2;
    print_string ")"
  |PE_app(id, exp_l) ->
    print_id id;
    print_string "(";
    print_separated_list print_expr ", " exp_l;
    print_string ")"
  |PE_fby(c, e') ->
    print_const c;
    print_string " fby ";
    print_expr e'
  |PE_tuple(exp_l) ->
    print_string "(";
    print_separated_list print_expr ", " exp_l;
    print_string ")"
  |PE_when(e, id, id') ->
    print_expr e;
    print_string " when ";
    print_id id;
    print_string "(";
    print_id id';
    print_string ")"
  |PE_current(e) ->
    print_string "current ";
    print_expr e
  |PE_merge(id, e_l) ->
    print_string "merge ";
    print_id id;
    print_string " ";
    print_separated_list (fun (id, e) ->
                            print_string "(";
                            print_id id;
                            print_string " -> ";
                            print_expr e;
                            print_string ")") " " e_l
;;

let print_equation e =
  (match e.peq_patt.ppatt_desc with
     |PP_ident(id) -> print_id id
     |PP_tuple(id_l) -> print_separated_list print_id ", " id_l);
  print_string " = ";
  print_expr e.peq_expr;
  print_string "    [";
  print_ck_const e.peq_expr.pexpr_clk;
  print_string "]"
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
  if List.length n.pn_local > 0 then print_string "var ";
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

let print f =
  let type_decs, node_decs = f in
  open_hovbox 0;
    print_separated_list print_node "\n\n" node_decs;
    print_newline ();
  close_box ()
;;
