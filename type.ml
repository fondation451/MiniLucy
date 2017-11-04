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

open Ast;;
open Ast_lustre;;

exception Type_Error of string;;

Random.self_init ();;

(*let gen_new_id () =
  "__" ^ (string_of_int (Random.bits ())) ^ "id" ^ (string_of_int (Random.bits ())) ^ "__"
;;*)

let rec string_ty ty =
  match ty with
  |Tint -> "int"
  |Treal -> "real"
  |Ttype(id) -> id
  |Ttuple(ty_l) -> (List.fold_left (fun out ty' -> out ^ (string_ty ty') ^ ", ") "(" ty_l) ^ ")"
;;

let string_patt pat =
  match pat.ppatt_desc with
  |PP_ident(id) -> id
  |PP_tuple(id_l) -> (List.fold_left (fun out id -> out ^ id ^ ", ") "(" id_l) ^ ")"
;;

let mk_expr_from e e_v ty = {
  pexpr_desc = e_v;
  pexpr_ty = ty;
  pexpr_clk = e.pexpr_lustre_clk;
  pexpr_loc = e.pexpr_lustre_loc;
};;

let type_const c =
  match c with
  |Cint(i) -> Tint
  |Creal(f) -> Treal
  |_ -> assert false
;;

let type_id ty_map env id = try
  Ttype(IdentMap.find id ty_map)
with Not_found -> try
  IdentMap.find id env
with Not_found -> raise (Type_Error ("Variable " ^ id ^ " not found"))
;;

let same_type e_l =
  match e_l with
  |[] -> raise (Type_Error "No arguments")
  |h::t ->
    let rec loop ty e_l =
      match e_l with
      |[] -> ty.pexpr_ty
      |h::t ->
        if ty.pexpr_ty = h.pexpr_ty then
          loop ty t
        else
          raise (Type_Error "Not the same type")
    in loop h t
;;


let type_param ty_map env p =
  let (type_env, var_env, node_env) = env in
  let find id = try IdentMap.find id var_env with Not_found -> raise (Type_Error ("Parameters : " ^ id ^ " does not exist")) in
  match p.ppatt_desc with
  |PP_ident(id) -> find id
  |PP_tuple(id_l) -> Ttuple (List.map find id_l)
;;


let rec type_expr const_map ty_map env e vars eqs =
  let (type_env, var_env, node_env) = env in
  match e.pexpr_lustre_desc with
  |PEL_const c -> (mk_expr_from e (PE_const c) (type_const c), vars, eqs)
  |PEL_ident id -> begin
    try
      let const_v = List.assoc id const_map in
      (mk_expr_from e (PE_const(const_v)) (type_const const_v), vars, eqs)
    with Not_found -> try
      let enum_ty = IdentMap.find id ty_map in
      (mk_expr_from e (PE_const(Cenum(id))) enum_ty, vars, eqs)
    with Not_found -> try
      let id_ty = IdentMap.find id var_env in
      (mk_expr_from e (PE_ident id) id_ty, vars, eqs)
    with Not_found -> raise (Type_Error ("Variable " ^ id ^ " not found"))
  end
  |PEL_op(uop, e') -> begin
    let e'_t, vars', eqs' = type_expr const_map ty_map env e' vars eqs in
    match uop with
    |UOp_not ->
      if e'_t.pexpr_ty = lustre_bool_type then
        (mk_expr_from e (PE_op (uop, e'_t)) e'_t.pexpr_ty, vars', eqs')
      else
        raise (Type_Error "Not : must be applied to bool")
    |UOp_minus -> begin
      match e'_t.pexpr_ty with
      |Tint |Treal -> (mk_expr_from e (PE_op (uop, e'_t)) e'_t.pexpr_ty, vars', eqs')
      |_ -> raise (Type_Error "Minus : must be applied to numbers (int or real)")
    end
  end
  |PEL_binop (op, e1, e2) -> begin
    let e1_t, vars', eqs' = type_expr const_map ty_map env e1 vars eqs in
    let e2_t, vars'', eqs'' = type_expr const_map ty_map env e2 vars' eqs' in
    match op with
    |Op_add |Op_sub |Op_mul |Op_div |Op_mod -> begin
      match e1_t.pexpr_ty, e2_t.pexpr_ty with
      |Tint, Tint -> (mk_expr_from e (PE_binop (op, e1_t, e2_t)) Tint, vars'', eqs'')
      |Treal, Treal -> (mk_expr_from e (PE_binop (op, e1_t, e2_t)) Treal, vars'', eqs'')
      |_ -> raise (Type_Error "Operation : arithmetics should be performed on numbers (int or real)")
    end
    |Op_eq |Op_neq |Op_lt |Op_le |Op_gt |Op_ge -> begin
      match e1_t.pexpr_ty, e2_t.pexpr_ty with
      |Tint, Tint -> (mk_expr_from e (PE_binop (op, e1_t, e2_t)) lustre_bool_type, vars'', eqs'')
      |Treal, Treal -> (mk_expr_from e (PE_binop (op, e1_t, e2_t)) lustre_bool_type, vars'', eqs'')
      |_ -> raise (Type_Error "Operation : comparaison should be performed on numbers (int or real)")
    end
    |Op_and |Op_or |Op_impl ->
      if e1_t.pexpr_ty = lustre_bool_type && e2_t.pexpr_ty = lustre_bool_type then
        (mk_expr_from e (PE_binop (op, e1_t, e2_t)) lustre_bool_type, vars'', eqs'')
      else
        raise (Type_Error "Operation : logicals should be performed on booleans")
  end
  |PEL_if(e1, e2, e3) -> begin
    let e1_t, vars, eqs = type_expr const_map ty_map env e1 vars eqs in
    let e2_t, vars, eqs = type_expr const_map ty_map env e2 vars eqs in
    let e3_t, vars, eqs = type_expr const_map ty_map env e3 vars eqs in
(*    print_endline (string_ty e1_t.pexpr_ty);
    print_endline (string_ty e2_t.pexpr_ty);
    print_endline (string_ty e3_t.pexpr_ty);*)
    if e1_t.pexpr_ty = lustre_bool_type && e2_t.pexpr_ty = e3_t.pexpr_ty then
      let id, vars, eqs =
        match e1_t.pexpr_desc with
        |PE_ident(id) -> id, vars, eqs
        |_ ->
          let new_id = gen_new_id () in
          let new_eq = {
            peq_patt = {ppatt_desc=PP_ident new_id; ppatt_loc=e1_t.pexpr_loc;};
            peq_expr = e1_t;
          } in new_id, (new_id, e1_t.pexpr_ty)::vars, new_eq::eqs
      in
      (mk_expr_from
        e
        (PE_merge
          (id,
          [lustre_bool_true, {pexpr_desc=PE_when(e2_t, lustre_bool_true, id); pexpr_clk=CK_free; pexpr_ty=e2_t.pexpr_ty; pexpr_loc=e2_t.pexpr_loc};
          lustre_bool_false, {pexpr_desc=PE_when(e3_t, lustre_bool_false, id); pexpr_clk=CK_free; pexpr_ty=e3_t.pexpr_ty; pexpr_loc=e3_t.pexpr_loc}]))
        e2_t.pexpr_ty,
      vars, eqs)
    else
      raise (Type_Error "If : invalid types")
  end
  |PEL_app (id, e_l) -> begin
    let e_l_t, vars, eqs = type_expr_l const_map ty_map env e_l vars eqs in
    try
      let f_node = IdentMap.find id node_env in
      let ty =
        match (List.map (fun p -> p.param_ty) f_node.pn_lustre_output) with
        |[ty'] -> ty'
        |_ as l -> Ttuple(l)
      in
      if List.for_all2 (fun e p -> e.pexpr_ty = p.param_ty) e_l_t f_node.pn_lustre_input then
        mk_expr_from e (PE_app (id, e_l_t)) ty, vars, eqs
      else
        raise (Type_Error ("Call of Node " ^ id ^ " : wrong type arguments"))
    with
    |Not_found -> raise (Type_Error ("Invalid identifier : " ^ id ^ " does not exists"))
    |Invalid_argument(_) -> raise (Type_Error ("Node " ^ id ^ " : invalid number of arguments"))
    |Type_Error(str) -> raise (Type_Error(str))
  end
  |PEL_fby (c, e2) -> begin
    try
      let c_ty = match c with |Cenum(id) -> IdentMap.find id ty_map |Cint(_) -> Tint |Creal(_) -> Treal in
      let e2_t, vars, eqs = type_expr const_map ty_map env e2 vars eqs in
      if c_ty = e2_t.pexpr_ty then
        (mk_expr_from e (PE_fby (c, e2_t)) e2_t.pexpr_ty, vars, eqs)
      else
        raise (Type_Error "FBY : not the same type in both side")
    with
      Not_found -> raise (Type_Error "FBY : constant is not a defined")
  end
  |PEL_tuple e_l ->
    let e_l_t, vars, eqs = type_expr_l const_map ty_map env e_l vars eqs in
    let ty = Ttuple (List.map (fun e -> e.pexpr_ty) e_l_t) in
    (mk_expr_from e (PE_tuple e_l_t) ty, vars, eqs)
  |PEL_when (e1, enum_id, e2) -> begin
    let e1_t, vars, eqs = type_expr const_map ty_map env e1 vars eqs in
    let e2_t, vars, eqs = type_expr const_map ty_map env e2 vars eqs in
    try
      let ty_enum_id = IdentMap.find enum_id ty_map in
      if ty_enum_id = e2_t.pexpr_ty then
        let id, vars, eqs =
          match e2_t.pexpr_desc with
          |PE_ident(id) -> id, vars, eqs
          |_ ->
            let new_id = gen_new_id () in
            let new_eq = {
              peq_patt = {ppatt_desc=PP_ident new_id; ppatt_loc=e2_t.pexpr_loc;};
              peq_expr = e2_t;
            } in new_id, (new_id, e2_t.pexpr_ty)::vars, new_eq::eqs
        in
        (mk_expr_from e (PE_when (e1_t, enum_id, id)) e1_t.pexpr_ty, vars, eqs)
      else
        raise (Type_Error "When : type of the id and of the close are not matching")
    with
    |Not_found -> raise (Type_Error "When : id of the enumaration not declared as a type")
    |Type_Error(str) -> raise (Type_Error str)
  end
  |PEL_current e' ->
    let e'_t, vars, eqs = type_expr const_map ty_map env e' vars eqs in
    (mk_expr_from e (PE_current e'_t) e'_t.pexpr_ty, vars, eqs)
  |PEL_merge (id, merge_l) -> begin
    let enum_l, e_l = List.split merge_l in
    let e_l', vars, eqs = List.fold_right
      (fun e' (e_l', vars, eqs) ->
        let e'', vars, eqs = type_expr const_map ty_map env e' vars eqs in
        e''::e_l', vars, eqs) e_l ([], vars, eqs) in
    let merge_l' = List.combine enum_l e_l' in
    try begin
      let ty = same_type e_l' in
      match IdentMap.find id var_env with
      |Ttype ty_id -> begin try
        let ty_enum = IdentMap.find ty_id type_env in
        if List.for_all (fun enum -> List.exists (fun enum_id -> enum_id = enum) enum_l) ty_enum then
          (mk_expr_from e (PE_merge (id, merge_l')) ty, vars, eqs)
        else
          raise (Type_Error "Merge : clause are not exaustives")
      with
      |Not_found -> raise (Type_Error ("Merge : the type " ^ ty_id ^ " does not exist"))
      |Type_Error(str) -> raise (Type_Error(str))
      end
      |_ -> raise (Type_Error "Merge : id doesn't have a consistent type")
    end with
    |Not_found -> raise (Type_Error ("Merge : the variable " ^ id ^ " does not exist"))
    |Type_Error(str) -> raise (Type_Error(str))
  end
and type_expr_l const_map ty_map env e_l vars eqs =
  (List.fold_right
    (fun e' (out, vars, eqs) ->
      let e'_t, vars, eqs = type_expr const_map ty_map env e' vars eqs in
      (e'_t::out, vars, eqs))
    e_l ([], vars, eqs))
;;

let type_eq const_map ty_map env eq vars eqs =
  let ty_p = type_param ty_map env eq.peq_lustre_patt in
  let e_t, vars, eqs = type_expr const_map ty_map env eq.peq_lustre_expr vars eqs in
  if e_t.pexpr_ty = ty_p then
    (vars, {peq_patt = eq.peq_lustre_patt; peq_expr = e_t}::eqs)
  else
    raise (Type_Error ("Equation : " ^ (string_patt eq.peq_lustre_patt) ^ " of type " ^ (string_ty ty_p) ^" doesn't match with the infered type " ^ (string_ty e_t.pexpr_ty)))
;;

let type_eqs const_map ty_map env eq_l =
  let vars, eqs = (List.fold_left (fun (vars, eqs) eq -> type_eq const_map ty_map env eq vars eqs) ([], []) eq_l) in
  vars, List.rev eqs
;;

let type_node const_map ty_map env node =
  let (type_env, var_env, node_env) = env in
  let var_env = List.fold_left (fun map p -> IdentMap.add p.param_id p.param_ty map) var_env node.pn_lustre_input in
  let var_env = List.fold_left (fun map p -> IdentMap.add p.param_id p.param_ty map) var_env node.pn_lustre_output in
  let var_env = List.fold_left (fun map p -> IdentMap.add p.param_id p.param_ty map) var_env node.pn_lustre_local in
  let vars, eqs = type_eqs const_map ty_map (type_env, var_env, node_env) node.pn_lustre_equs in
  let new_local = List.fold_left (fun l (id, ty) -> {param_id = id; param_ty = ty; param_ck = CK_free;}::l) node.pn_lustre_local vars in
  {
    pn_name = node.pn_lustre_name;
    pn_input = node.pn_lustre_input;
    pn_output = node.pn_lustre_output;
    pn_local = new_local;
    pn_equs = eqs;
    pn_loc = node.pn_lustre_loc;
  }
;;

let mk_ty_map f =
  let f_ty, f_const, f_node_l = f in
  let full_ty = IdentMap.bindings f_ty in
  List.fold_left (fun out (ty, l_enum) -> List.fold_left (fun out enum -> IdentMap.add enum (Ttype(ty)) out) out l_enum) IdentMap.empty full_ty
;;

let mk_env f =
  let f_ty, f_const, f_node_l = f in
  let ty_env = f_ty in
  let var_env = List.fold_left (fun out (id, c) -> IdentMap.add id (type_const c) out) IdentMap.empty f_const in
  let node_env = List.fold_left (fun out node -> IdentMap.add node.pn_lustre_name node out) IdentMap.empty f_node_l in
  (ty_env, var_env, node_env)
;;

let type_file f =
  let f_ty, f_const, f_node_l = f in
  let ty_map = mk_ty_map f in
  let env = mk_env f in
  (f_ty, List.map (type_node f_const ty_map env) f_node_l)
;;
