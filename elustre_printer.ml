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
open East;;

let print_separated_list print_fn sep l =
  let rec aux = function
    | [] -> ()
    | [h] -> print_fn h;
    | h::t -> print_fn h; print_string sep; aux t;
  in
  aux l
;;

let print_id = print_string;;

let print_const c =
  match c with
  |Cint(i) -> print_int i
  |Creal(f) -> print_float f
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

let rec print_decl d =
  match d.pdecl_desc with
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
  |PE_let_in(decl, expr) ->
    print_string "let ";
    print_decl decl;
    print_string " in ";
    print_expr expr
and print_equation e =
  (match e.peq_patt.ppatt_desc with
     |PP_ident(id) -> print_id id
     |PP_tuple(id_l) -> print_separated_list print_id ", " id_l);
  print_string " = ";
  print_expr e.peq_expr
;;

let print_elustre f =
  open_hovbox 2;
    print_decl f;
  close_box ()
;;
