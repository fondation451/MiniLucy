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

let rec str_of_elustre_ty verb t =
  match t with
  |Tint      -> "int"
  |Treal     -> "real"
  |Ttype id  -> id
  |Tfun (ty1, ty2) ->
    String.concat " " [str_of_elustre_ty verb ty1; "->"; str_of_elustre_ty verb ty2]
;;

let print_typ verb t = print_string (str_of_elustre_ty verb t);;

let rec str_of_elustre_ck verb c =
  match c with
  |CKbase  -> "BASE"
  |CKid id -> id
  |CKon (ck1, id1, ck2) -> String.concat "" [
    str_of_elustre_ck verb ck1; " on "; id1; "("; str_of_elustre_ck verb ck1; ")"]
  |CKfun (ck1, ck2) ->
    String.concat " " [str_of_elustre_ck verb ck1; "->"; str_of_elustre_ck verb ck2]
;;

let print_ck verb ck = print_string (str_of_elustre_ck verb ck);;

let print_id verb id = print_id id;;

let print_id_ty_ck v (id, ty, ck) =
  if v then print_string "(";
  print_id v id;
  if v then begin
    print_string " : ";
    print_typ v ty;
    print_string " ";
    print_ck v ck;
    print_string ")"
  end
;;

let print_param verb p =
  print_id verb p.param_id;
  print_string " : ";
  print_typ verb p.param_ty;
  print_string " ";
  print_ck verb p.param_ck;;

let print_type_decl verb ty_decl =
  let (id,decl) = ty_decl in
    print_string "type ";
    print_id verb id;
    if List.length decl > 0 then begin
      print_string " = ";
      print_separated_list (print_id verb) " + " decl;
    end;
    print_string ";";;

let rec print_decl verb d =
  match d.pdecl_desc with
  |PD_skip -> print_string "skip"
  |PD_and(d1, d2) ->
    print_decl verb d1;
    print_string ";\n";
    print_decl verb d2
  |PD_eq(eq) -> print_equation verb eq
  |PD_clk(id, expr) ->
    print_string "clock ";
    print_id_ty_ck verb id;
    print_string " = ";
    print_expr verb expr
  |PD_let_in(d1, d2) ->
    print_string "let ";
    print_decl verb d1;
    print_string " in ";
    print_decl verb d2
  |PD_match(expr, case_list) ->
    print_string "match ";
    print_expr verb expr;
    print_string " with\n  ";
    print_separated_list  (fun (id, d) ->
                            print_string "|";
                            print_id_ty_ck verb id;
                            print_string " -> ";
                            print_decl verb d;
                            print_string "\n") "  " case_list
  |PD_reset(decl, expr) ->
    print_string "reset ";
    print_decl verb decl;
    print_string " every ";
    print_expr verb expr;
  |PD_automaton(case_list) ->
    print_string "automaton\n  ";
    print_separated_list  (fun (id, psv, psc) ->
                            print_string "|";
                            print_id_ty_ck verb id;
                            print_string " -> ";
                            print_shared verb psv;
                            print_string " ";
                            print_strong verb psc;
                            print_string "\n") "  " case_list
and print_shared verb ps =
  match ps with
  |PSV_let(decl, ps) ->
    print_string "let ";
    print_decl verb decl;
    print_string " in ";
    print_shared verb ps
  |PSV_do(decl, wc) ->
    print_string "do ";
    print_decl verb decl;
    print_string " ";
    print_weak verb wc
and print_strong verb sc =
  match sc with
  |PSC_unless_then(expr, id, sc) ->
    print_string "unless ";
    print_expr verb expr;
    print_string " then ";
    print_id_ty_ck verb id;
    print_string " ";
    print_strong verb sc
  |PSC_unless_cont(expr, id, sc) ->
    print_string "unless ";
    print_expr verb expr;
    print_string " continue ";
    print_id_ty_ck verb id;
    print_string " ";
    print_strong verb sc
  |PSC_epsilon -> ()
and print_weak verb wc =
  match wc with
  |PWC_until_then(expr, id, wc) ->
    print_string "until ";
    print_expr verb expr;
    print_string " then ";
    print_id_ty_ck verb id;
    print_string " ";
    print_weak verb wc
  |PWC_until_cont(expr, id, wc) ->
    print_string "until ";
    print_expr verb expr;
    print_string " continue ";
    print_id_ty_ck verb id;
    print_string " ";
    print_weak verb wc
  |PWC_epsilon -> ()
and print_expr verb e =
  match e.pexpr_desc with
  |PE_const(c) -> print_const c
  |PE_ident(id) -> print_id_ty_ck verb id
  |PE_uop(op, e) ->
    print_string (string_of_uop op);
    print_expr verb e;
    print_string " ";
  |PE_bop(op, e1, e2) ->
    print_string "(";
    print_expr verb e1;
    print_string " ";
    print_string (string_of_op op);
    print_string " ";
    print_expr verb e2;
    print_string ")";
  |PE_if(e, e', e'') ->
    print_string "if ";
    print_expr verb e;
    print_string " then ";
    print_expr verb e';
    print_string " else ";
    print_expr verb e''
  |PE_app(id, exp_l, e) ->
    print_id_ty_ck verb id;
    print_string "(";
    print_separated_list (print_expr verb) ", " exp_l;
    print_string ")";
    print_string " every ";
    print_expr verb e;
  |PE_fby(c, e') ->
    if verb then print_string "(";
    print_const c;
    if verb then begin
      print_string " : ";
      print_typ verb e.pexpr_ty;
      print_string " ";
      print_ck verb e.pexpr_ck;
      print_string ")"
    end;
    print_string " fby ";
    print_expr verb e'
  |PE_pre(e) ->
    print_string "pre (";
    print_expr verb e;
    print_string ")"
  |PE_when(e, id, e') ->
    print_expr verb e;
    print_string " when ";
    if verb then print_string "(";
    print_id verb id;
    if verb then begin
      print_string " : ";
      print_typ verb e.pexpr_ty;
      print_string " ";
      print_ck verb e.pexpr_ck;
      print_string ")"
    end;
    print_string "(";
    print_expr verb e';
    print_string ")"
  |PE_merge(e, e_l) ->
    print_string "merge ";
    print_expr verb e;
    print_string " ";
    print_separated_list (fun (id, e) ->
                            print_string "(";
                            print_id_ty_ck verb id;
                            print_string " -> ";
                            print_expr verb e;
                            print_string ")") " " e_l
  |PE_last(id) ->
    print_string "last ";
    print_id_ty_ck verb id;
and print_equation verb e =
  (match e.peq_patt.ppatt_desc with
     |PP_ident(id) ->
      if verb then print_string "(";
      print_id verb id;
      if verb then begin
        print_string " : ";
        print_typ verb e.peq_expr.pexpr_ty;
        print_string " ";
        print_ck verb e.peq_expr.pexpr_ck;
        print_string ")"
      end;
     |PP_tuple(id_l) -> print_separated_list (print_id verb) ", " id_l);
  print_string " = ";
  print_expr verb e.peq_expr
;;

let print_node verb n =
  print_string "let node ";
  print_id verb n.pn_name;
  print_string " (";
  print_separated_list (print_param verb) ", " n.pn_input;
  print_string ") ";
  print_string "= (";
  print_separated_list (print_param verb) ", " n.pn_output;
  print_string ")";
  if List.length n.pn_local > 0 then begin
    print_string "\nwith ";
    print_separated_list (print_param verb) ", " n.pn_local;
  end;
  print_string " where\n";
  open_hovbox 2;
    print_decl verb n.pn_decl;
    print_string ";\n";
  close_box ();
;;

let print_elustre verb f =
  let (type_decls, nodes) = f in
  open_hovbox 2;
    print_string "-- Enum type declarations\n\n";
    print_separated_list (print_type_decl verb) "\n" (IdentMap.bindings type_decls);
    print_newline ();
    print_newline ();
    print_newline ();
    print_string "-- Nodes declarations\n\n";
    print_separated_list (print_node verb) "\n\n" nodes;
  close_box ()
;;
