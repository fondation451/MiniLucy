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
open Ast_type;;
open East;;
open Lustre_printer;;

let rec print_reg_typ t =
  match t with
  |Tint      -> print_string "int"
  |Treal     -> print_string "real"
  |Ttype id  -> print_string id
  |Ttuple tu ->
    print_string "(";
    print_separated_list print_reg_typ ", " tu;
    print_string ")";
  |Tvar i ->
    print_string "`a";
    print_int i;
  |Tcfun (ty1, ty2) ->
    print_reg_typ ty1;
    print_string "-0->";
    print_reg_typ ty2;
  |Tsfun (ty1, ty2) ->
    print_reg_typ ty1;
    print_string "-1->";
    print_reg_typ ty2;
;;

let print_typ t =
  let rec print_typ_aux alpha t =
    match t with
    |Regular ty ->
      if List.length alpha > 0 then begin
        print_string "forall ";
        print_separated_list (fun i -> print_string "`a"; print_int i)
                             ", " alpha;
        print_string "." end;
      print_reg_typ ty;
    |Scheme (i, ty) ->
      print_typ_aux (i :: alpha) ty in

  print_typ_aux [] t
;;

let print_param p =
  print_id p.param_id;
  print_string " : ";
  print_typ p.param_ty;;

let print_type_decl ty_decl =
  let (id,decl) = ty_decl in
    print_string "type ";
    print_id id;
    if List.length decl > 0 then begin
      print_string " = ";
      print_separated_list print_id " + " decl;
    end;
    print_string ";";;

let rec print_decl d =
  match d.pdecl_desc with
  |PD_skip -> print_string "skip"
  |PD_and(d1, d2) ->
    print_decl d1;
    print_string ";\n";
    print_decl d2
  |PD_eq(eq) -> print_equation eq
  |PD_clk(id, expr) ->
    print_string "clock ";
    print_id id;
    print_string " = ";
    print_expr expr
  |PD_let_in(d1, d2) ->
    print_string "let ";
    print_decl d1;
    print_string " in ";
    print_decl d2
  |PD_match(expr, case_list) ->
    print_string "match ";
    print_expr expr;
    print_string " with\n  ";
    print_separated_list (fun (id, d) ->
                            print_string "|";
                            print_id id;
                            print_string " -> ";
                            print_decl d;
                            print_string "\n") "  " case_list
  |PD_reset(decl, expr) ->
    print_string "reset ";
    print_decl decl;
    print_string " every ";
    print_expr expr;
  |PD_automaton(case_list) ->
    print_string "automaton\n  ";
    print_separated_list (fun (id, psv, psc) ->
                            print_string "|";
                            print_id id;
                            print_string " -> ";
                            print_shared psv;
                            print_string " ";
                            print_strong psc;
                            print_string "\n") "  " case_list
and print_shared ps =
  match ps with
  |PSV_let(decl, ps) ->
    print_string "let ";
    print_decl decl;
    print_string " in ";
    print_shared ps
  |PSV_do(decl, wc) ->
    print_string "do ";
    print_decl decl;
    print_string " ";
    print_weak wc
and print_strong sc =
  match sc with
  |PSC_unless_then(expr, id, sc) ->
    print_string "unless ";
    print_expr expr;
    print_string " then ";
    print_id id;
    print_string " ";
    print_strong sc
  |PSC_unless_cont(expr, id, sc) ->
    print_string "unless ";
    print_expr expr;
    print_string " continue ";
    print_id id;
    print_string " ";
    print_strong sc
  |PSC_epsilon -> ()
and print_weak wc =
  match wc with
  |PWC_until_then(expr, id, wc) ->
    print_string "until ";
    print_expr expr;
    print_string " then ";
    print_id id;
    print_string " ";
    print_weak wc
  |PWC_until_cont(expr, id, wc) ->
    print_string "until ";
    print_expr expr;
    print_string " continue ";
    print_id id;
    print_string " ";
    print_weak wc
  |PWC_epsilon -> ()
and print_expr e =
  match e.pexpr_desc with
  |PE_const(c) -> print_const c
  |PE_ident(id) -> print_id id
  |PE_uop(op, e) ->
    print_string (string_of_uop op);
    print_expr e;
    print_string " ";
  |PE_bop(op, e1, e2) ->
    print_string "(";
    print_expr e1;
    print_string " ";
    print_string (string_of_op op);
    print_string " ";
    print_expr e2;
    print_string ")";
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
  |PE_when(e, id, e') ->
    print_expr e;
    print_string " when ";
    print_id id;
    print_string "(";
    print_expr e';
    print_string ")"
  |PE_merge(e, e_l) ->
    print_string "merge ";
    print_expr e;
    print_string " ";
    print_separated_list (fun (id, e) ->
                            print_string "(";
                            print_id id;
                            print_string " -> ";
                            print_expr e;
                            print_string ")") " " e_l
  |PE_last(id) ->
    print_string "last ";
    print_id id
and print_equation e =
  (match e.peq_patt.ppatt_desc with
     |PP_ident(id) -> print_id id
     |PP_tuple(id_l) -> print_separated_list print_id ", " id_l);
  print_string " = ";
  print_expr e.peq_expr
;;

let print_node n =
  print_string "let node ";
  print_id n.pn_name;
  print_string " (";
  print_separated_list print_param ", " n.pn_input;
  print_string ") ";
  print_string "= (";
  print_separated_list print_param ", " n.pn_output;
  print_string ")";
  if List.length n.pn_local > 0 then begin
    print_string "\nwith ";
    print_separated_list print_param ", " n.pn_local;
  end;
  print_string " where\n";
  open_hovbox 2;
    print_decl n.pn_decl;
    print_string ";\n";
  close_box ();
;;

let print_elustre f =
  let (type_decls, nodes) = f in
  open_hovbox 2;
    print_string "-- Enum type declarations\n\n";
    print_separated_list print_type_decl "\n" (IdentMap.bindings type_decls);
    print_newline ();
    print_newline ();
    print_newline ();
    print_string "-- Nodes declarations\n\n";
    print_separated_list print_node "\n\n" nodes;
  close_box ()
;;
