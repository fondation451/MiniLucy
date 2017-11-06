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

exception Schedule_Error of string;;

let left_expr e =
  let rec left_expr_rec e out =
    match e.pexpr_desc with
    |PE_const(c) -> out
    |PE_ident(id) -> IdentSet.add id out
    |PE_op(op, e1) -> left_expr_rec e1 out
    |PE_binop(op, e1, e2) -> left_expr_rec e1 (left_expr_rec e2 out)
    |PE_app(id, e_l) -> List.fold_left (fun out e1 -> left_expr_rec e1 out) out e_l
    |PE_fby(c, e2) -> out
    |PE_tuple(e_l) -> List.fold_left (fun out e1 -> left_expr_rec e1 out) out e_l
    |PE_when(e1, enum_id, id) -> left_expr_rec e1 (IdentSet.add id out)
    |PE_current(e1) -> left_expr_rec e1 out
    |PE_merge(id, merge_l) -> List.fold_left (fun out (enum_id, e1) -> left_expr_rec e1 out) (IdentSet.add id out) merge_l
  in left_expr_rec e IdentSet.empty
;;

let vars_eq eq =
  match eq.peq_patt.ppatt_desc with
  |PP_ident(id) -> IdentSet.add id IdentSet.empty
  |PP_tuple(id_l) -> List.fold_left (fun out id -> IdentSet.add id out) IdentSet.empty id_l
;;

let vars_f eqs =
  let out =
    List.fold_left
      (fun out eq ->
        let eq_v = vars_eq eq in
        if IdentSet.is_empty (IdentSet.inter out eq_v) then
          IdentSet.union out eq_v
        else
          raise (Schedule_Error (" variables have been defined two times")))
      IdentSet.empty eqs
  in
  IdentSet.elements out
;;

let pick_node degree_m =
  let rec loop i =
    if i < Array.length degree_m then
      if degree_m.(i) = 0 then
        i
      else
        loop (i+1)
    else
      -1
  in loop 0
;;

let decrease_degree n g degree_m =
  let n_l = g.(n) in
  List.iter (fun i -> degree_m.(i) <- degree_m.(i) - 1) n_l
;;

let topological g degree_m =
  let rec loop out =
    let node = pick_node degree_m in
    if node < 0 then
      out
    else begin
      let edges = g.(node) in
      List.iter (fun i -> degree_m.(i) <- degree_m.(i) - 1) edges;
      degree_m.(node) <- -1;
      loop (node::out)
    end
  in loop []
;;

let mk_var_map eqs =
  let rec loop i var_l out =
    match var_l with
    |[] -> out
    |h::t -> loop (i+1) t (IdentMap.add h i out)
  in loop 0 (vars_f eqs) IdentMap.empty
;;

let mk_g eqs =
  let var_map = mk_var_map eqs in
  let nb_node = List.length (IdentMap.bindings var_map) in
  let g = Array.make nb_node [] in
  let degree_m = Array.make nb_node 0 in
  List.iter
    (fun eq ->
      let left_eq = left_expr eq.peq_expr in
      let fill_g id =
        let depends =
          List.filter
            (fun i -> i <> -1)
            (List.rev_map
              (fun id ->
                (*print_endline ("Looking for " ^ id ^ " (depends)");*)
                try
                  IdentMap.find id var_map
                with Not_found -> -1)
              (IdentSet.elements left_eq))
        in
        let id_ind =
          (*print_endline ("Looking for " ^ id ^ "(id_ind)")*)
          IdentMap.find id var_map
        in
        List.iter
          (fun i ->
            degree_m.(id_ind) <- degree_m.(id_ind) + 1;
            g.(i) <- id_ind::(g.(i)))
          depends
      in
      match eq.peq_patt.ppatt_desc with
      |PP_ident(id) -> fill_g id
      |PP_tuple(id_l) -> List.iter fill_g id_l) eqs;
  var_map, g, degree_m
;;

let reverse_map map =
  let (key, value) = List.split (IdentMap.bindings map) in
  List.fold_left2 (fun out k v -> (v, k)::out) [] key value
;;

let remove_l p l =
  let rec loop l out =
    match l with
    |[] -> assert false
    |h::t ->
      if p h then
        h, List.rev_append t out
      else
        loop t (h::out)
  in loop l []
;;

let extract_id eq = match eq.peq_patt.ppatt_desc with |PP_ident(id) -> [id] |PP_tuple(id_l) -> id_l;;

let order_eqs ordered_id eqs =
  let rec loop ordered_id eqs out =
    match ordered_id with
    |[] -> List.rev out
    |id::t ->
      let eq, new_eqs = remove_l (fun eq -> List.mem id (extract_id eq)) eqs in
      let id_of_eq = extract_id eq in
      loop (List.filter (fun id -> not (List.mem id id_of_eq)) t) new_eqs (eq::out)
  in loop ordered_id eqs []
;;

let compare_eqs ordered_id eq1 eq2 =
  let extract_id pat = match pat with |PP_ident(id) -> [id] |PP_tuple(id_l) -> id_l in
  let ids1 = extract_id eq1.peq_patt.ppatt_desc in
  let ids2 = extract_id eq2.peq_patt.ppatt_desc in
  let rec loop ids1 =
    match ids1 with
    |[] -> -1
    |h::t ->
      if List.mem h ids2 then
        1
      else
        loop t
  in loop ids1
;;

let schedule_eqs eqs =
  let var_map, g, degree_m = mk_g eqs in
(*  print_endline "var_map :";
  IdentMap.iter (fun id ind -> print_string (id ^ " : " ^ (string_of_int ind) ^ ", ")) var_map;
  print_newline ();
  print_endline "degree_m : (before)";
  Array.iteri (fun i deg -> print_string ("(" ^ (string_of_int i) ^ ", " ^ (string_of_int deg) ^ "), ")) degree_m;
  print_newline ();*)
  let id_map = reverse_map var_map in
  let ordered_ind_rev = topological g degree_m in
(*  print_endline "degree_m : (after)";
  Array.iteri (fun i deg -> print_string ("(" ^ (string_of_int i) ^ ", " ^ (string_of_int deg) ^ "), ")) degree_m;
  print_newline ();
  print_endline "ordered_ind_rev :";
  List.iter (fun ind -> print_string ((string_of_int ind) ^ ", ")) ordered_ind_rev;
  print_newline ();*)
  if List.length ordered_ind_rev <> Array.length g then
    raise (Schedule_Error "Cycle in the equation's dependancies")
  else
    let ordered_id = List.rev_map (fun i -> List.assoc i id_map) ordered_ind_rev in
(*  print_endline "ordered_id :";
  List.iter (fun id -> print_string (id ^ ", ")) ordered_id;
  print_newline ();
    Lustre_printer.print_separated_list Lustre_printer.print_equation ";\n" eqs;
    print_string "\n\n\n";*)
    (*let out = List.sort (compare_eqs ordered_id) eqs in*)
    let out = order_eqs ordered_id eqs in
(*    Lustre_printer.print_separated_list Lustre_printer.print_equation ";\n" out;
    print_string "\n\n\n";
    print_string "\n\n\n";*)
    out
;;

let schedule_file f =
  let typemap, node_l = f in
  typemap, List.map (fun node -> {node with pn_equs = schedule_eqs node.pn_equs}) node_l
;;
