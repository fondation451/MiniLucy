open Ast_type;;
open Ast;;
open East;;

exception ClockingError of string;;

let make_clock_env param =
  let rec add_in_env env param_l =
    match param_l with
    |[]   -> env
    |p::q -> add_in_env (IdentMap.add p.param_id p.param_ck env)
                        q in
  add_in_env IdentMap.empty param
;;

let find_in_env env ck =
  (try IdentMap.find ck env with |Not_found ->
    raise (ClockingError (String.concat " " ["Couldn't find clock"; ck])))
;;

let rec ck_equal cenv ck1 ck2 =
  match ck1, ck2 with
  |CKbase, CKbase        -> true
  |CKid i, CKid j        -> ck_equal cenv (find_in_env cenv i)
                                          (find_in_env cenv j)
  |CKon (ck1, id1, ck1'),
   CKon (ck2, id2, ck2') ->
    let ck_eq  = ck_equal cenv ck1  ck2  in
    let id_eq  = String.equal  id1  id2  in
    let ck'_eq = ck_equal cenv ck1' ck2' in
    ck_eq && id_eq && ck'_eq
  |CKfun (c1, c2),
   CKfun (k1, k2)        -> (ck_equal cenv c1 k1) && (ck_equal cenv c2 k2)
  | _ -> false
;;

let ck_is_none ck =
  match ck with
  | CKid id -> String.equal id "__None__"
  | _ -> false
;;

let ck_equal_none cenv ck1 ck2 =
  (ck_equal cenv ck1 ck2) || (ck_is_none ck1)
                          || (ck_is_none ck2)
;;

let check_ck cenv ck1 ck2 =
  if not (ck_equal_none cenv ck1 ck2)
  && not (ck_equal cenv ck1 CKbase)
  && not (ck_equal cenv ck2 CKbase) then
    let ck1_str = Elustre_printer.str_of_elustre_ck true ck1 in
    let ck2_str = Elustre_printer.str_of_elustre_ck true ck2 in
    raise (ClockingError (String.concat " " ["Clocks"; ck1_str; "and"; ck2_str; "mismatch"]))
;;

let assign_clock_e cenv ck e =
  if ck_is_none (e.pexpr_ck) then
    {e with pexpr_ck = ck}
  else
    let () = check_ck cenv e.pexpr_ck ck in e
;;

let rec clock_expr tdef cenv cfun e =
  let ck, e_desc =
    match e.pexpr_desc with
    |PE_const c                    -> CKbase, PE_const c
    |PE_ident (id, ty, ck)         ->
      let ck_env = try find_in_env cenv id with |ClockingError _ -> ck in
      ck_env, PE_ident (id, ty, ck_env)
    |PE_uop (op, e)                ->
      let e = clock_expr tdef cenv cfun e in
      e.pexpr_ck, PE_uop (op, e)
    |PE_bop (op, e1, e2)           ->
      let e1 = clock_expr tdef cenv cfun e1 in
      let e2 = clock_expr tdef cenv cfun e2 in
      let ck1 = e1.pexpr_ck in let ck2 = e2.pexpr_ck in
      let () = check_ck cenv ck1 ck2 in
      ck1, PE_bop (op, e1, e2)
    |PE_if (e1, e2, e3)            ->
      let e1 = clock_expr tdef cenv cfun e1 in
      let e2 = clock_expr tdef cenv cfun e2 in
      let e3 = clock_expr tdef cenv cfun e3 in
      let ck1 = e1.pexpr_ck in let ck2 = e2.pexpr_ck in let ck3 = e3.pexpr_ck in
      let ()  = check_ck cenv ck2 (CKon (ck1, "True", ck1)) in
      let ()  = check_ck cenv ck3 (CKon (ck1, "False", ck1)) in
      ck1, PE_if (e1, e2, e3)
    |PE_app ((id, ty, ck), e_l, e) ->
      let ck_f = find_in_env cfun id in
      let e_l  = List.map (clock_expr tdef cenv cfun) e_l in
      let e    = clock_expr tdef cenv cfun e in
      let ck_out = match ck_f with
                  |CKfun (in_ck, out_ck) ->
                    let rec unfold_CKfun ck =
                    match ck with
                    |CKfun (r1, r2) -> (unfold_CKfun r1) @ (unfold_CKfun r2)
                    |_ -> [ck] in
                    let _ = List.map2 (check_ck cenv) (List.map (fun e -> e.pexpr_ck) e_l) (unfold_CKfun in_ck) in
                    out_ck
                  | _ -> raise (ClockingError (String.concat " " [
                    "Variable"; id; "has clock"; Elustre_printer.str_of_elustre_ck true ck_f;
                    "which is not a function clock"
                    ])) in
      ck_out, PE_app ((id, ty, ck_f), e_l, e)
    |PE_fby (c, e)                 ->
      let e = clock_expr tdef cenv cfun e in
      e.pexpr_ck, PE_fby (c, e)
    |PE_pre e                      ->
      let e = clock_expr tdef cenv cfun e in
      e.pexpr_ck, PE_pre e
    |PE_when (e1, id, e2)          ->
      let e1 = clock_expr tdef cenv cfun e1 in
      let e2 = clock_expr tdef cenv cfun e1 in
      let ck1 = e1.pexpr_ck in let ck2 = e2.pexpr_ck in
      (CKon (ck1, id, ck2)), PE_when (e1, id, e2)
    |PE_merge (e, id_e_l)          ->
      let e = clock_expr tdef cenv cfun e in
      let ck = e.pexpr_ck in
      let id_e_l = List.map (fun (id, e) -> id, clock_expr tdef cenv cfun e) id_e_l in
      let () = List.iter (fun (id, e) ->
        let (id, _, _) = id in check_ck cenv e.pexpr_ck (CKon (ck, id, ck))) id_e_l in
      ck, PE_merge (e, id_e_l)
    |PE_last (id, ty, ck)          ->
      let ck_env = try find_in_env cenv id with |ClockingError _ -> ck in
      ck_env, PE_last (id, ty, ck_env) in
  assign_clock_e cenv ck { e with pexpr_desc = e_desc }
;;

let rec clock_match tdef cenv cfun e case_list =
  let cenv, case_list = List.fold_right (
      fun (id, d) (cenv, case_list) ->
        let cenv, d = clock_decl tdef cenv cfun d in
        cenv, (id, d) :: case_list) case_list (cenv, []) in
  cenv, PD_match (e, case_list)
and clock_automaton tdef cenv cfun case_list =
  let cenv, case_list = List.fold_right (
      fun (id, psv, psc) (cenv, case_list) ->
        let cenv, psv = clock_psv tdef cenv cfun psv in
        let cenv, psc = clock_psc tdef cenv cfun psc in
        (cenv, (id, psv, psc) :: case_list)
    ) case_list (cenv, []) in
  cenv, PD_automaton (case_list)
and clock_psv tdef cenv cfun psv =
  match psv with
  |PSV_let (d, psv) ->
    let cenv, d   = clock_decl tdef cenv cfun d in
    let cenv, psv = clock_psv tdef cenv cfun psv in
    cenv, PSV_let (d, psv)
  |PSV_do (d, pwc) ->
    let cenv, d   = clock_decl tdef cenv cfun d in
    let cenv, pwc = clock_pwc tdef cenv cfun pwc in
    cenv, PSV_do (d, pwc)
and clock_psc tdef cenv cfun psc =
  let aux e id psc =
    let e = clock_expr tdef cenv cfun e in
    let cenv, psc = clock_psc tdef cenv cfun psc in
    cenv, e, id, psc in
  match psc with
  |PSC_epsilon              -> cenv, PSC_epsilon
  |PSC_unless_then (e, id, psc) -> let cenv, e, id, psc = aux e id psc in
    cenv, PSC_unless_then (e, id, psc)
  |PSC_unless_cont (e, id, psc) -> let cenv, e, id, psc = aux e id psc in
    cenv, PSC_unless_cont (e, id, psc)
and clock_pwc tdef cenv cfun pwc =
  let aux e id pwc =
    let e = clock_expr tdef cenv cfun e in
    let cenv, pwc = clock_pwc tdef cenv cfun pwc in
    cenv, e, id, pwc in
  match pwc with
  |PWC_epsilon              -> cenv, PWC_epsilon
  |PWC_until_then (e, id, pwc) -> let cenv, e, id, pwc = aux e id pwc in
    cenv, PWC_until_then (e, id, pwc)
  |PWC_until_cont (e, id, pwc) -> let cenv, e, id, pwc = aux e id pwc in
    cenv, PWC_until_cont (e, id, pwc)
and clock_decl tdef cenv cfun d =
  let cenv, d_desc =
    match d.pdecl_desc with
    |PD_skip                     -> cenv, PD_skip
    |PD_and (d1, d2)             -> let cenv, d1 = clock_decl tdef cenv cfun d1 in
                                    let cenv, d2 = clock_decl tdef cenv cfun d2 in
                                    cenv, PD_and (d1, d2)
    |PD_eq (eq)                  -> let e = clock_expr tdef cenv cfun eq.peq_expr in
                                    cenv, PD_eq {eq with peq_expr = e}
    |PD_clk ((id, ty, ck), expr) -> let e = clock_expr tdef cenv cfun expr in
                                    cenv, PD_clk ((id, e.pexpr_ty, e.pexpr_ck), e)
    |PD_let_in (d1, d2)          -> let cenv, d1 = clock_decl tdef cenv cfun d1 in
                                    let cenv, d2 = clock_decl tdef cenv cfun d2 in
                                    cenv, PD_let_in (d1, d2)
    |PD_match (e, case_list)     -> let e = clock_expr tdef cenv cfun e in
                                    clock_match tdef cenv cfun e case_list
    |PD_reset (d, e)             -> let e = clock_expr tdef cenv cfun e in
                                    let cenv, d = clock_decl tdef cenv cfun d in
                                    cenv, PD_reset (d, e)
    |PD_automaton case_list      -> clock_automaton tdef cenv cfun case_list in
  cenv, { d with pdecl_desc = d_desc}
;;

let clock_node tdef cfun node =
  let cenv = make_clock_env (node.pn_input @ node.pn_output @ node.pn_local) in
  let p_ck = List.map (fun param -> param.param_ck) (node.pn_input @ node.pn_output) in
  let rec build_fun_clock l =
    match l with
    |[]   -> raise (ClockingError "build_fun_clock")
    |[q]  -> q
    |q::r -> CKfun (q, build_fun_clock r) in
  let cfun = IdentMap.add node.pn_name (build_fun_clock p_ck) cfun in
  let cenv, d = clock_decl tdef cenv cfun node.pn_decl in
  tdef, cfun, {node with pn_decl = d}
;;

let rec clock_nodes tdef cfun ln =
  match ln with
  |[]   -> []
  |p::q -> let tdef, cfun, d = clock_node tdef cfun p in
           d :: (clock_nodes tdef cfun q)
;;

let clock_file f =
  let (type_decls, nodes) = f in
    let c_fun = IdentMap.empty in
    type_decls, clock_nodes type_decls c_fun nodes
;;
