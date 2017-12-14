open Ast_type;;
open Ast;;
open East;;

exception TypingError of string;;
let var_msg = "variable";;
let fun_msg = "function";;
let type_msg = "type";;

let is_ttype t =
  match t with
  |Ttype _ -> true
  | _      -> false
;;

let make_type_env param =
  let rec add_in_env env param_l =
    match param_l with
    |[]   -> env
    |p::q -> add_in_env (IdentMap.add p.param_id p.param_ty env)
                        q in
  add_in_env IdentMap.empty param
;;

let find_in_env env id thing =
  try IdentMap.find id env
  with |Not_found ->
    raise (TypingError (String.concat " " ["No"; thing; "called"; id;
                                           "has been found"]))
;;

let type_of_const env c =
  match c with
  | Cint _  -> Tint
  | Creal _ -> Treal
  | Cenum c ->
    let typ = (List.fold_right (
      fun (typ, const_list) elt -> if List.mem c const_list then typ
                                   else elt
      ) (IdentMap.bindings env) "") in
    if String.equal typ "" then raise Not_found else Ttype typ
;;

let rec ty_equal_aux ty1 ty2 subst =
  match ty1, ty2 with
  |Tint, Tint         -> true
  |Treal, Treal       -> true
  |Ttype i1, Ttype i2 -> String.equal i1 i2
  |Tfun (r1, r2), Tfun (r1', r2') -> (ty_equal_aux r1 r1' subst)
                                  && (ty_equal_aux r2 r2' subst)
  | _ -> false
;;

let ty_equal ty1 ty2 = ty_equal_aux ty1 ty2 [];;
let ty_equal_none ty1 ty2 =
  (ty_equal ty1 ty2 || ty_equal ty1 none_type || ty_equal ty2 none_type);;

let check_ty ty1 ty2 =
  if not ((ty_equal ty1 ty2) || (ty_equal ty1 none_type)
                             || (ty_equal ty2 none_type)) then
    let ty1_str = Elustre_printer.str_of_elustre_ty true ty1 in
    let ty2_str = Elustre_printer.str_of_elustre_ty true ty2 in
    raise (TypingError (String.concat " " ["Types"; ty1_str; "and"; ty2_str; "mismatch"]))
;;

let rec check_exist tdef ty =
  match ty with
  |Ttype id      -> let _ = find_in_env tdef id type_msg in ()
  |Tfun (r1, r2) -> (check_exist tdef r1); (check_exist tdef r2)
  | _ -> ()
;;

let assign_type_e ty e =
  if ty_equal (e.pexpr_ty) (none_type) then
    {e with pexpr_ty = ty}
  else
    let () = check_ty e.pexpr_ty ty in e
;;

let rec type_uop op e =
  let ty =
    match op with
    |UOp_not   -> bool_type
    |UOp_minus -> if ty_equal Treal e.pexpr_ty then Treal
                  else Tint in
  ty, PE_uop (op, e)
and type_bop op e1 e2 =
  let ty =
    match op with
    |Op_add |Op_sub |Op_mul |Op_div |Op_mod ->
      if ty_equal_none Treal e1.pexpr_ty
      && ty_equal_none Treal e2.pexpr_ty then Treal
      else if ty_equal_none Tint e1.pexpr_ty
           && ty_equal_none Tint e2.pexpr_ty then Tint
      else raise (TypingError (String.concat " " [
        "Types"; Elustre_printer.str_of_elustre_ty false e1.pexpr_ty; "and";
                 Elustre_printer.str_of_elustre_ty false e2.pexpr_ty; "are not";
        "compatible with int or real"
        ]))
    |Op_eq |Op_neq |Op_lt |Op_le |Op_gt |Op_ge ->
    if ty_equal_none Treal e1.pexpr_ty
    && ty_equal_none Treal e2.pexpr_ty then bool_type
    else if ty_equal_none Tint e1.pexpr_ty
         && ty_equal_none Tint e2.pexpr_ty then bool_type
    else raise (TypingError (String.concat " " [
      "Types"; Elustre_printer.str_of_elustre_ty false e1.pexpr_ty; "and";
               Elustre_printer.str_of_elustre_ty false e2.pexpr_ty; "are not";
      "compatible with int or real"
      ]))
    |Op_and |Op_or |Op_impl ->
      if ty_equal bool_type e1.pexpr_ty
      && ty_equal bool_type e2.pexpr_ty then bool_type
      else raise (TypingError (String.concat " " [
        "Expected type bool, got types";
        Elustre_printer.str_of_elustre_ty false e1.pexpr_ty; "and";
        Elustre_printer.str_of_elustre_ty false e2.pexpr_ty; "instead"
        ])) in
  ty, PE_bop (op, e1, e2)
and type_expr tdef tenv tfun expr =
  let ty, e =
  match expr.pexpr_desc with
  |PE_const c          -> type_of_const tdef c, PE_const c
  |PE_ident (id, ty)   ->
    let env_ty =
      try
        find_in_env tenv id var_msg
      with TypingError s ->
      try
        type_of_const tdef (Cenum id)
      with TypingError s' -> raise (TypingError s) in
    env_ty, PE_ident (id, env_ty)
  |PE_uop (op, e)      -> let e = type_expr tdef tenv tfun e in type_uop op e
  |PE_bop (op, e1, e2) ->
    let e1 = type_expr tdef tenv tfun e1 in
    let e2 = type_expr tdef tenv tfun e2 in
    type_bop op e1 e2
  |PE_if (e1, e2, e3) ->
    let e1 = type_expr tdef tenv tfun e1 in
    let e2 = type_expr tdef tenv tfun e2 in
    let e3 = type_expr tdef tenv tfun e3 in
    let () = check_ty bool_type e1.pexpr_ty in
    let () = check_ty e2.pexpr_ty e3.pexpr_ty in
    e2.pexpr_ty, PE_if (e1, e2, e3)
  |PE_app ((id, ty), e_l, e) ->
    let e_l = List.map (type_expr tdef tenv tfun) e_l in
    let e   = type_expr tdef tenv tfun e in
    let f_ty = find_in_env tfun id fun_msg in
    let ty  = match f_ty with
              |Tfun (in_ty, out_ty) ->
                let rec unfold_Tfun ty =
                match ty with
                |Tfun (r1, r2) -> (unfold_Tfun r1) @ (unfold_Tfun r2)
                |_ -> [ty] in
                let _ = List.map2 check_ty (List.map (fun e -> e.pexpr_ty) e_l) (unfold_Tfun in_ty) in
                out_ty
              | _ -> raise (TypingError (String.concat " " [
                "Variable"; id; "has type"; Elustre_printer.str_of_elustre_ty true f_ty;
                "which is not a function type"
              ])) in
    let () = check_ty bool_type e.pexpr_ty in
    ty, PE_app ((id, f_ty), e_l, e)
  |PE_fby (c, e) ->
    let c_ty = type_of_const tdef c in
    let e    = type_expr tdef tenv tfun e in
    let _    = check_ty c_ty e.pexpr_ty in
    c_ty, PE_fby(c, e)
  |PE_pre e -> let e = type_expr tdef tenv tfun e in e.pexpr_ty, PE_pre e
  |PE_when (e1, id, e2) ->
    let e1 = type_expr tdef tenv tfun e1 in
    let e2 = type_expr tdef tenv tfun e2 in
    let _  = check_ty e2.pexpr_ty (Ttype id) in
    e1.pexpr_ty, PE_when (e1, id, e2)
  |PE_merge (e, id_e_l) ->
    let e = type_expr tdef tenv tfun e in
    let id_e_l = List.fold_right (
      fun ((id, ty), e_) id_e_l ->
        let e_ = type_expr tdef tenv tfun e_ in
        let id_ty = type_of_const tdef (Cenum id) in
        let () = check_ty e.pexpr_ty id_ty in
        let _ = if List.length id_e_l > 0 then
          let (id, e) = (List.hd (id_e_l)) in check_ty e.pexpr_ty e_.pexpr_ty in
        ((id, id_ty), e_) :: id_e_l
      ) id_e_l [] in
      let (id, e_) = List.hd id_e_l in
      e_.pexpr_ty, PE_merge (e, id_e_l)
  |PE_last (id, ty) ->
    let env_ty = find_in_env tenv id var_msg in
    env_ty, PE_last (id, env_ty) in
  assign_type_e ty {expr with pexpr_desc = e}
;;

let rec type_eq tdef tenv tfun eq =
  let e = type_expr tdef tenv tfun eq.peq_expr in
  let id = (match eq.peq_patt.ppatt_desc with
            |PP_ident id -> id
            |_ -> raise (TypingError ("Tuples are not permitted"))) in
  let () = check_ty (find_in_env tenv id var_msg) e.pexpr_ty in
  PD_eq ({eq with peq_expr = e})
;;

let rec type_match tdef tenv tfun e case_list =
  let case_list = List.fold_right (
    fun ((id, ty), d) (case_list) ->
      let d = type_decl tdef tenv tfun d in
      let () = check_ty ty e.pexpr_ty in
      ((id, e.pexpr_ty), d) :: case_list
    ) case_list ([]) in
  PD_match (e, case_list)
and type_automaton tdef tenv tfun case_list =
  let ((_, ty), _, _) = List.hd case_list in
  let () = check_exist tdef ty in
  let case_list, _ = List.fold_right (
    fun ((id, ty), psv, psc) (case_list, p_ty) ->
      let psv = type_psv tdef tenv tfun p_ty psv in
      let psc = type_psc tdef tenv tfun p_ty psc in
      let () = check_ty ty p_ty in
      (((id, p_ty), psv, psc) :: case_list, ty)
    ) case_list ([], ty) in
  PD_automaton case_list
and type_psv tdef tenv tfun ty psv =
  match psv with
  |PSV_let (d, psv) ->
    let tenv, d = type_let tdef tenv tfun d in
    let psv = type_psv tdef tenv tfun ty psv in
    PSV_let (d, psv)
  |PSV_do (d, pwc) ->
    let d = type_decl tdef tenv tfun d in
    let pwc = type_pwc tdef tenv tfun ty pwc in
    PSV_do (d, pwc)
and type_psc tdef tenv tfun ty psc =
  match psc with
  |PSC_epsilon -> PSC_epsilon
  |PSC_unless_then (e, (id, ty_), psc) ->
    let e = type_expr tdef tenv tfun e in
    let () = check_ty e.pexpr_ty bool_type in
    let () = check_ty ty ty_ in
    let psc = type_psc tdef tenv tfun ty psc in
    PSC_unless_then (e, (id, ty), psc)
  |PSC_unless_cont (e, (id, ty_), psc) ->
    let e = type_expr tdef tenv tfun e in
    let () = check_ty e.pexpr_ty bool_type in
    let () = check_ty ty ty_ in
    let psc = type_psc tdef tenv tfun ty psc in
    PSC_unless_cont (e, (id, ty), psc)
and type_pwc tdef tenv tfun ty pwc =
  match pwc with
  |PWC_epsilon -> PWC_epsilon
  |PWC_until_then (e, (id, ty_), pwc) ->
    let e = type_expr tdef tenv tfun e in
    let () = check_ty e.pexpr_ty bool_type in
    let () = check_ty ty ty_ in
    let pwc = type_pwc tdef tenv tfun ty pwc in
    PWC_until_then (e, (id, ty), pwc)
  |PWC_until_cont (e, (id, ty_), pwc) ->
    let e = type_expr tdef tenv tfun e in
    let () = check_ty e.pexpr_ty bool_type in
    let () = check_ty ty ty_ in
    let pwc = type_pwc tdef tenv tfun ty pwc in
    PWC_until_cont (e, (id, ty), pwc)
and type_let tdef tenv tfun decl =
  let tenv =
    match decl.pdecl_desc with
    |PD_eq (eq)              ->
      (match eq.peq_patt.ppatt_desc with
       |PP_ident id -> let tenv_ = IdentMap.add id none_type tenv in
                       let e = type_expr tdef tenv_ tfun eq.peq_expr in
                       IdentMap.add id e.pexpr_ty tenv
       |_ -> raise (TypingError "Tuples are not permitted"))
    |PD_clk ((id, ty), expr) -> IdentMap.add id ty tenv
    | _ -> tenv in
  tenv, type_decl tdef tenv tfun decl
and type_decl tdef tenv tfun decl =
  print_string "\n\n--------------\n";
  Elustre_printer.print_decl true decl; print_newline (); print_newline ();
  IdentMap.iter (fun k ty -> print_string k; print_string " : "; Elustre_printer.print_typ true ty; print_newline ()) tenv;
  IdentMap.iter (fun k ty -> print_string k; print_string " : "; Elustre_printer.print_typ true ty; print_newline ()) tfun;
  print_string "\n----------------\n\n";
  let d =
      match decl.pdecl_desc with
      |PD_skip                 -> PD_skip
      |PD_and (d1, d2)         -> let d1 = type_decl tdef tenv tfun d1 in
                                  let d2 = type_decl tdef tenv tfun d2 in
                                  PD_and (d1, d2)
      |PD_eq (eq)              -> type_eq tdef tenv tfun eq
      |PD_clk ((id, ty), expr) -> let e = type_expr tdef tenv tfun expr in
                                  PD_clk ((id, e.pexpr_ty), e)
      |PD_let_in (d1, d2)      -> let tenv, d1 = type_let tdef tenv tfun d1 in
                                  let d2 = type_decl tdef tenv tfun d2 in
                                  PD_let_in (d1, d2)
      |PD_match (e, case_list) -> let e = type_expr tdef tenv tfun e in
                                  type_match tdef tenv tfun e case_list
      |PD_reset (d, e)         -> let e = type_expr tdef tenv tfun e in
                                  let d = type_decl tdef tenv tfun d in
                                  PD_reset (d, e)
      |PD_automaton case_list  -> type_automaton tdef tenv tfun case_list in
  { decl with pdecl_desc = d }
;;

let type_node tdef tfun node =
  let tenv = make_type_env (node.pn_input @ node.pn_output @ node.pn_local) in
  let p_ty = List.map (fun param -> param.param_ty) (node.pn_input @ node.pn_output) in
  let rec build_fun_type l =
    match l with
    |[]   -> raise (TypingError "build_fun_type")
    |[q]  -> q
    |q::r -> Tfun (q, build_fun_type r) in
  let tfun = IdentMap.add node.pn_name (build_fun_type p_ty) tfun in
  let d = type_decl tdef tenv tfun node.pn_decl in
  tdef, tfun, {node with pn_decl = d}
;;

let rec type_nodes tdef tfun ln =
  match ln with
  |[]   -> []
  |p::q -> let tdef, tfun, d = type_node tdef tfun p in
           d :: (type_nodes tdef tfun q)
;;

let type_file f =
  let (type_decls, nodes) = f in
    let t_fun = IdentMap.empty in
    type_decls, type_nodes type_decls t_fun nodes
;;
