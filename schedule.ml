(*
########
Copyright © 2017

This file is part of MiniLucy.
MiniLucy is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License v3 as published by
the Free Software Foundation.

Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########
*)

open Ast_type;;
open Ast;;
open Ast_schedule;;

exception Schedule_Error of string;;

let rec remove_fby eqs =
  let fby_out = ref [] in
  let eqs_out = List.fold_left
    (fun out eq ->
      match eq.peq_expr.pexpr_desc with
      |PE_fby(_) -> fby_out := eq::(!fby_out); out
      |_ -> eq::out)
    []
    eqs
  in
  eqs_out, fby_out
;;

let left_expr e var_in =
  let rec left_expr_rec e out =
    match e.pexpr_desc with
    |PE_const(c) -> out
    |PE_ident(id) -> if IdentSet.mem id var_in then out else IdentSet.add id out
    |PE_op(op, e1) -> left_expr_rec e1 out
    |PE_binop(op, e1, e2) -> left_expr_rec e1 (left_expr_rec e2 out)
    |PE_app(id, e_l, id_reset) -> List.fold_left (fun out e1 -> left_expr_rec e1 out) (IdentSet.add id_reset out) e_l
    |PE_fby(c, e2) -> out
(*    |PE_tuple(e_l) -> List.fold_left (fun out e1 -> left_expr_rec e1 out) out e_l*)
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

let rec pick_node degree_m dep_m =
  let candidats = ref [] in
  let out = ref (-1) in
  Array.iteri (fun i d -> if d = 0 then candidats := i::(!candidats)) degree_m;
  List.iter (fun i -> if dep_m.(i) = 0 then out := i) !candidats;
  if !candidats = [] then
    dep_m, -1
  else if !out = -1 then
    (Array.copy degree_m), -2
  else
    dep_m, !out
;;

let decrease_degree n g degree_m =
  let n_l = g.(n) in
  List.iter (fun i -> degree_m.(i) <- degree_m.(i) - 1) n_l
;;

let topological g degree_m =
  let rec loop out dep_m =
    let dep_m, node = pick_node degree_m dep_m in
    Printf.printf "node = %d\n" node;
    if node = -1 then
      out
    else begin
      if node <> -2 then begin
        let edges = g.(node) in
        List.iter (fun i -> degree_m.(i) <- degree_m.(i) - 1) edges;
        degree_m.(node) <- -1
      end;
      loop (node::out) dep_m
    end
  in loop [] (Array.copy degree_m)
;;

let mk_var_map eqs =
  let rec loop i var_l out =
    match var_l with
    |[] -> out
    |h::t -> loop (i+1) t (IdentMap.add h i out)
  in loop 0 (vars_f eqs) IdentMap.empty
;;

let mk_g eqs var_in =
  let var_map = mk_var_map eqs in
  let nb_node = List.length (IdentMap.bindings var_map) in
  let g = Array.make nb_node [] in
  let degree_m = Array.make nb_node 0 in
  List.iter
    (fun eq ->
      let left_eq = left_expr eq.peq_expr var_in in
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
      if id = dumb_id then
        loop t eqs (SP_SKIP::out)
      else
        let eq, new_eqs = remove_l (fun eq -> List.mem id (extract_id eq)) eqs in
        let id_of_eq = extract_id eq in
        loop (List.filter (fun id -> not (List.mem id id_of_eq)) t) new_eqs ((SP_EQ(eq))::out)
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

let delay_fby eq_l =
  let rec loop eq_l fby_l out =
    match eq_l with
    |[] -> List.rev (List.rev_append fby_l (SP_SKIP::out))
    |eq::t -> begin
      match eq with
      |SP_SKIP -> loop t fby_l (eq::out)
      |SP_EQ(eq') -> begin
        match eq'.peq_expr.pexpr_desc with
        |PE_fby(_) -> loop t (eq::fby_l) out
        |_ -> loop t fby_l (eq::out)
      end
    end
  in loop eq_l [] []
;;

let schedule_eqs eqs var_in =
  let var_map, g, degree_m = mk_g eqs var_in in
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
  if List.length ordered_ind_rev < Array.length g then
    raise (Schedule_Error "Cycle in the equation's dependancies")
  else
    let ordered_id = List.rev_map (fun i -> if i <> -2 then List.assoc i id_map else dumb_id) ordered_ind_rev in
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
    delay_fby out
;;

let schedule_file f =
  let typemap, node_l = f in
  typemap, List.map
             (fun node ->
               let var_in = List.fold_left (fun out par -> IdentSet.add par.param_id out) IdentSet.empty node.pn_input in
               {spn_name = node.pn_name;
                spn_input = node.pn_input;
                spn_output = node.pn_output;
                spn_local = node.pn_local;
                spn_equs = schedule_eqs node.pn_equs var_in;
                spn_loc = node.pn_loc})
             node_l
;;
