open Ast;;
open Ast_lustre;;
open Ast_type;;
open East;;

exception NotImplemented
exception TranslationError

let mk_decl decl_desc =
  {
    pdecl_desc = decl_desc;
    pdecl_loc  = (Lexing.dummy_pos, Lexing.dummy_pos);
  }
;;

let mk_expr expr_desc =
  {
    pexpr_desc = expr_desc;
    pexpr_ty   = Regular (Ttype "__None__");
    pexpr_loc  = (Lexing.dummy_pos, Lexing.dummy_pos);
  }
;;

let gen_name pref i = String.concat "_" [pref; string_of_int i];;

let find_fresh pref varS =
  let i = ref 0 in
  let _ = while IdentSet.mem (gen_name pref !i) varS do i := !i + 1 done
  in gen_name pref !i
;;

let rec cResetD (decl : East.p_decl) (expr : East.p_expr) =
  let aux (decl_desc : East.p_decl_desc) (expr : East.p_expr) =
    match decl_desc with
    |PD_skip -> PD_skip
    |PD_and(d1, d2) -> PD_and(cResetD d1 expr, cResetD d2 expr)
    |PD_eq(eq) -> PD_eq({ peq_patt = eq.peq_patt;
                                    peq_expr = cResE eq.peq_expr expr})
    |PD_clk(id, e) -> PD_clk(id, cResE e expr)
    |PD_let_in(d1, d2) -> PD_let_in(cResetD d1 expr, cResetD d2 expr)
    |PD_match(e, case_list) -> PD_match(e,
                                                  List.map (fun (id, decl) -> (id, cResetD decl expr))
                                                            case_list)
    |PD_reset(decl, e) -> PD_reset(decl, e)
    |PD_automaton(case_list) -> PD_automaton(List.map (fun (id, sv, sc) -> (id, cResetU sv expr, cResetS sc expr))
                                                      case_list)
  in
  {pdecl_desc = aux decl.pdecl_desc expr;
   pdecl_loc  = decl.pdecl_loc}
and cResetU (sv : East.p_shared_var) (expr : East.p_expr) =
  match sv with
  |East.PSV_let(d, sv) -> East.PSV_let(cResetD d expr, cResetU sv expr)
  |East.PSV_do(d, wc) -> East.PSV_do(cResetD d expr, cResetW wc expr)
and cResetS (sc : East.p_strong_cond) (expr : East.p_expr) =
  match sc with
  |East.PSC_unless_then(e, id, sc) -> East.PSC_unless_then(cResE e expr, id, cResetS sc expr)
  |East.PSC_unless_cont(e, id, sc) -> East.PSC_unless_cont(cResE e expr, id, cResetS sc expr)
  |East.PSC_epsilon -> East.PSC_epsilon
and cResetW (wc : East.p_weak_cond) (expr : East.p_expr) =
  match wc with
  |East.PWC_until_then(e, id, wc) -> East.PWC_until_then(cResE e expr, id, cResetW wc expr)
  |East.PWC_until_cont(e, id, wc) -> East.PWC_until_cont(cResE e expr, id, cResetW wc expr)
  |East.PWC_epsilon -> East.PWC_epsilon
and cResE (e : East.p_expr) (expr : East.p_expr) =
  let rec cResE_list l =
    List.map (fun (e) -> cResE e expr) l
  in
  let aux (expr_desc : East.p_expr_desc) =
    match expr_desc with
    |East.PE_const(c) -> East.PE_const(c);
    |East.PE_ident(id) -> East.PE_ident(id);
    |East.PE_uop(op, e) -> East.PE_uop(op,
                                   cResE e expr)
    |East.PE_bop(op, e1, e2) -> East.PE_bop(op,
                                   cResE e1 expr, cResE e2 expr)
    |East.PE_if(e, e1, e2) -> East.PE_if(cResE e expr, cResE e1 expr, cResE e2 expr)
    |East.PE_app(id, expr_list) -> East.PE_app(id,
                                     cResE_list expr_list)
    |East.PE_arrow(e1, e2) -> let y = cResE e1 expr in
                          East.PE_if(expr, y, {pexpr_desc = East.PE_arrow(y, cResE e2 expr);
                                               pexpr_ty   = y.pexpr_ty;
                                               pexpr_loc  = y.pexpr_loc})
    |East.PE_pre(e) -> East.PE_pre(cResE e expr)
    |East.PE_tuple(expr_list) -> East.PE_tuple(cResE_list expr_list)
    |East.PE_when(e1, id, e2) -> East.PE_when(cResE e1 expr, id, cResE e2 expr)
    |East.PE_merge(e, case_list) -> East.PE_merge(cResE e expr,
                                        List.map (fun (id, e_) -> (id, cResE e_ expr))
                                                 case_list)
    |East.PE_last(id) -> East.PE_last(id)
  in
  {East.pexpr_desc = aux e.pexpr_desc;
   East.pexpr_ty   = e.pexpr_ty;
   East.pexpr_loc  = e.pexpr_loc}

(* Translation functions *)

let fv_p patt =
  match patt.ppatt_desc with
  |PP_ident id      -> IdentSet.add id IdentSet.empty
  |PP_tuple id_list -> List.fold_left (fun env id -> IdentSet.add id env)
                                      IdentSet.empty
                                      id_list
;;

let rec def_d decl =
  match decl.pdecl_desc with
  |PD_skip                     -> IdentSet.empty
  |PD_and (d1, d2)             -> IdentSet.union (def_d d1) (def_d d2)
  |PD_eq  eq                   -> fv_p eq.peq_patt
  |PD_clk (id, e)              -> IdentSet.add id IdentSet.empty
  |PD_let_in (_, d)            -> def_d d
  |PD_match  (e, id_d_list)    -> List.fold_left (fun env (_, decl) -> IdentSet.union (def_d decl) env)
                                                IdentSet.empty
                                                id_d_list
  |PD_reset (d, e)             -> def_d d
  |PD_automaton (id_psv_psc_l) -> List.fold_left (fun env (_, psv, _) -> IdentSet.union (def_psv psv) env)
                                                 IdentSet.empty
                                                 id_psv_psc_l
and def_psv psv =
  match psv with
  |PSV_let (d, psv) -> def_psv psv
  |PSV_do  (d, pwc) -> def_d d
;;

let rec fv_e exp =
  let fv_e_list e_list =
    List.fold_left (fun env e -> IdentSet.union (fv_e e) env)
                    IdentSet.empty
                    e_list in
  match exp.pexpr_desc with
  |PE_const _          -> IdentSet.empty
  |PE_ident id         -> IdentSet.add id IdentSet.empty
  |PE_uop (_, e)       -> fv_e e
  |PE_bop (_, e1, e2)  -> IdentSet.union (fv_e e1) (fv_e e2)
  |PE_if  (e1, e2, e3) -> IdentSet.union (IdentSet.union (fv_e e1) (fv_e e2)) (fv_e e3)
  |PE_app (id, e_list) -> fv_e_list e_list
  |PE_arrow (e1, e2)   -> IdentSet.union (fv_e e1) (fv_e e2)
  |PE_pre e            -> fv_e e
  |PE_tuple e_list     -> fv_e_list e_list
  |PE_when (e1, _, e2) -> IdentSet.union (fv_e e1) (fv_e e2)
  |PE_merge (e, id_e_list) -> IdentSet.union (fv_e e) (fv_e_list (snd (List.split id_e_list)))
  |PE_last id          -> IdentSet.empty
;;

let rec fv_d decl =
  match decl.pdecl_desc with
  |PD_skip            -> IdentSet.empty
  |PD_and (d1, d2)    -> IdentSet.union (fv_d d1) (fv_d d2)
  |PD_eq  eq          -> IdentSet.union (fv_e eq.peq_expr) IdentSet.empty
  |PD_clk (id, e)     -> IdentSet.union (fv_e e) IdentSet.empty
  |PD_let_in (d1, d2) -> IdentSet.union (fv_d d1) (IdentSet.diff (fv_d d2) (def_d d1))
  |PD_match(e, case_list)  -> IdentSet.union (fv_e e) (
    List.fold_left (fun env (id, d) -> IdentSet.union (fv_d d) env)
                   IdentSet.empty case_list
    )
  |PD_reset(d, e)          -> IdentSet.union (fv_d d) (fv_e e)
  |PD_automaton(case_list) -> (
    List.fold_left (fun env (id, psv, psc) ->
                    IdentSet.union (IdentSet.union (fv_psv psv) (fv_psc psc)) env)
                   IdentSet.empty case_list
    )
and fv_psv psv =
  match psv with
  |PSV_let (d, psv_) -> IdentSet.union (fv_d d) (fv_psv psv_)
  |PSV_do  (d, pwc)  -> IdentSet.union (fv_d d) (fv_pwc pwc)
and fv_psc psc =
  match psc with
  |PSC_unless_then (e, id, psc_) -> IdentSet.union (fv_e e) (fv_psc psc_)
  |PSC_unless_cont (e, id, psc_) -> IdentSet.union (fv_e e) (fv_psc psc_)
  |PSC_epsilon                   -> IdentSet.empty
and fv_pwc pwc =
  match pwc with
  |PWC_until_then (e, id, pwc_) -> IdentSet.union (fv_e e) (fv_pwc pwc_)
  |PWC_until_cont (e, id, pwc_) -> IdentSet.union (fv_e e) (fv_pwc pwc_)
  |PWC_epsilon                   -> IdentSet.empty
;;

let cOn d c_id c_e =
  let rec replace_var_e var_id e =
    let replace_var_e_list e_list =
      List.map (fun e -> replace_var_e var_id e) e_list
    in
    { e with pexpr_desc =
      match e.pexpr_desc with
      |East.PE_const(c)               -> East.PE_const(c)
      |East.PE_ident(id_)             -> if String.compare var_id id_ == 0 then
                                          East.PE_when(e, c_id, c_e)
                                         else East.PE_ident(id_)
      |East.PE_uop(op, e_)            -> East.PE_uop(op, replace_var_e var_id e_)
      |East.PE_bop(op, e1, e2)        -> East.PE_bop(op, replace_var_e var_id e1, replace_var_e var_id e2)
      |East.PE_if(e, e1, e2)          -> East.PE_if(e, replace_var_e var_id e1, replace_var_e var_id e2)
      |East.PE_app(f_id, expr_list)   -> East.PE_app(f_id, replace_var_e_list expr_list)
      |East.PE_arrow(e1, e2)          -> East.PE_arrow(replace_var_e var_id e1, replace_var_e var_id e2)
      |East.PE_pre(e)                 -> East.PE_pre(replace_var_e var_id e)
      |East.PE_tuple(expr_list)       -> East.PE_tuple(replace_var_e_list expr_list)
      |East.PE_when(e1, ck_id, e2)    -> East.PE_when(replace_var_e var_id e1, ck_id, replace_var_e var_id e2)
      |East.PE_merge(e, case_list)    -> East.PE_merge(replace_var_e var_id e, case_list)
      |East.PE_last(id_)              -> if String.compare var_id id_ == 0 then
                                           let pre_e = {e with pexpr_desc = PE_pre e} in
                                           East.PE_when (pre_e, c_id, c_e)
                                         else East.PE_last(id_)
    }
  and replace_var_d var_id decl =
    { decl with pdecl_desc =
      match decl.pdecl_desc with
      |PD_skip                   -> PD_skip
      |PD_and(d1, d2)            -> PD_and(replace_var_d var_id d1, replace_var_d var_id d2)
      |PD_eq(eq)                 -> PD_eq({ eq with peq_expr = replace_var_e var_id eq.peq_expr })
      |PD_clk(ck_id, expr)       -> PD_clk(ck_id, replace_var_e var_id expr)
      |PD_let_in(d1, d2)         -> PD_let_in(replace_var_d var_id d1, replace_var_d var_id d2)
      |PD_match(expr, case_list) -> PD_match(replace_var_e var_id expr, case_list)
      |PD_reset(decl, expr)      -> PD_reset(replace_var_d var_id decl, replace_var_e var_id expr)
      |PD_automaton(case_list)   -> PD_automaton(
        List.map (fun (id, psv, psc) -> (id, replace_var_psv var_id psv, replace_var_psc var_id psc))
                 case_list
        )
    }
  and replace_var_psv var_id psv =
    match psv with
    |PSV_let (d, psv_) -> PSV_let (replace_var_d var_id d, replace_var_psv var_id psv_)
    |PSV_do  (d, pwc)  -> PSV_do  (replace_var_d var_id d, replace_var_pwc var_id pwc)
  and replace_var_psc var_id psc =
    match psc with
    |PSC_unless_then (e, id, psc_) -> PSC_unless_then (replace_var_e var_id e,
                                                       id,
                                                       replace_var_psc var_id psc_)
    |PSC_unless_cont (e, id, psc_) -> PSC_unless_cont (replace_var_e var_id e,
                                                       id,
                                                       replace_var_psc var_id psc_)
    |PSC_epsilon                   -> PSC_epsilon
  and replace_var_pwc var_id pwc =
    match pwc with
    |PWC_until_then (e, id, pwc_) -> PWC_until_then (replace_var_e var_id e,
                                                       id,
                                                       replace_var_pwc var_id pwc_)
    |PWC_until_cont (e, id, pwc_) -> PWC_until_cont (replace_var_e var_id e,
                                                       id,
                                                       replace_var_pwc var_id pwc_)
    |PWC_epsilon                   -> PWC_epsilon
  in
  IdentSet.fold (fun var_id d -> replace_var_d var_id d) (fv_d d) d
;;

let rec split varN decl =
  match decl.pdecl_desc with
  |PD_and (d1, d2) ->
    let (in1, out1) = split varN d1 in
    let (in2, out2) = split varN d2 in
    (in1 @ in2, out1 @ out2)
  |d               ->
    let varD = def_d decl in
      if IdentSet.is_empty (IdentSet.inter varD varN) then ([], [decl])
      else ([decl], [])
;;

let proj var c_id c_e decl =
  let in_pat v p =
    match p.ppatt_desc with
    |PP_ident id      -> String.compare id v == 0
    |PP_tuple id_list -> List.mem v id_list
  in
  let rec find v d =
    match d.pdecl_desc with
    |PD_skip                   -> None
    |PD_and(d1, d2)            -> let vD1 = find v d1 in
                                  if vD1 == None then find v d2 else vD1
    |PD_eq(eq)                 -> if in_pat v eq.peq_patt then Some eq.peq_expr
                                  else None
    |PD_clk(ck_id, expr)       -> if String.compare ck_id v == 0 then Some expr
                                  else None
    |PD_let_in(d1, d2)         -> find v d2
    |PD_match(expr, case_list) -> List.fold_left (
      fun prev (_, d) -> let curr = find v d in
                         if curr == None then prev else curr
      ) None case_list
    |PD_reset(decl, expr)      -> find v decl
    |PD_automaton(case_list)   -> List.fold_left (
      fun prev (_, psv, _) -> match psv with |PSV_let (d,_) |PSV_do (d,_) ->
                              let curr = find v d in
                              if curr == None then prev else curr
      ) None case_list
  in
  match find var decl with
  |None -> mk_expr (
            PE_when (
              mk_expr (
                PE_pre (mk_expr (PE_ident var) )
                      ),
              c_id, c_e
                    )
                   )
  |Some e -> e
;;

let cMatch e case_list =
  let find_fresh expr d_l =
    let fv_d_l = List.fold_left (
      fun fv_d_p d -> IdentSet.union (fv_d d) fv_d_p
      ) IdentSet.empty d_l in
    let fv_d_e = IdentSet.union (fv_e e) fv_d_l in
    find_fresh "clk_match" fv_d_e
  in

  let rec flatten_d_l d_l =
    match d_l with
    |[]   -> mk_decl PD_skip
    |[p]  -> p
    |p::q -> { p with pdecl_desc = PD_and (p, flatten_d_l q) }
  in

  let make_merge var cond ids gi =
    let cond_e = mk_expr (PE_ident cond) in

    let rec make_cases ids gi =
      match ids, gi with
      |[], []         -> []
      |p1::q1, p2::q2 ->
        (p1, proj var p1 cond_e p2) :: (make_cases q1 q2)
      |_              -> raise TranslationError
    in

    mk_decl (PD_eq {
      peq_patt = {
        ppatt_desc = PP_ident var;
        ppatt_loc = (Lexing.dummy_pos, Lexing.dummy_pos);
      };
      peq_expr = mk_expr (PE_merge (cond_e, make_cases ids gi))
    })
  in

  let (ids, di_l, ni_l) = List.fold_left (
    fun (ids, di_p, ni_p) (id, d) ->
      let ni = def_d d in (id :: ids, d :: di_p, ni :: ni_p)
    ) ([], [], []) case_list in
  let (d'i_l, gi_l) = List.fold_left (
    fun (d'i_p, gi_p) (di, ni) ->
      let (d'i, gi) = split ni di in ((flatten_d_l d'i) :: d'i_p, (flatten_d_l gi) :: gi_p)
    ) ([], []) (List.combine di_l ni_l) in
  let and_d' = flatten_d_l d'i_l in
  let clk_name = find_fresh e di_l in
  let clk_d = { (List.hd d'i_l) with pdecl_desc = PD_clk (clk_name, e)} in
  let ni_u = List.fold_left IdentSet.union IdentSet.empty ni_l in

  let merge_list = IdentSet.fold (
    fun var m_l -> (make_merge var clk_name ids gi_l) :: m_l
    ) ni_u [] in

  let merge_d = flatten_d_l merge_list in

  mk_decl (PD_and (and_d', mk_decl (PD_and (clk_d, merge_d))))
;;

let rec translate_reg_ty ty =
  let rec translate_reg_ty_list l =
    List.map translate_reg_ty l in
  match ty with
  |Tint       -> Ast_type.Tint
  |Treal      -> Ast_type.Treal
  |Ttype t    -> Ast_type.Ttype t
  |Ttuple tys -> Ast_type.Ttuple (translate_reg_ty_list tys)
  |Tvar  _    -> raise NotImplemented
  |Tcfun _    -> raise NotImplemented
  |Tsfun _    -> raise NotImplemented
;;

let translate_elustre_ty ty =
  match ty with
  |Regular ty -> translate_reg_ty ty
  |_          -> raise NotImplemented

let rec translate_param p =
  {
    Ast.param_id = p.param_id;
    Ast.param_ty = translate_elustre_ty p.param_ty;
    Ast.param_ck = p.param_ck;
  }

let rec translate_eq eq =
  { Ast_lustre.peq_lustre_patt = { ppatt_desc = eq.peq_patt.ppatt_desc;
                                   ppatt_loc  = eq.peq_patt.ppatt_loc};
    Ast_lustre.peq_lustre_expr = translate_expr eq.peq_expr}
and translate_decl d =
  let rec translate_aux d =
    match d.pdecl_desc with
    |PD_skip        -> []
    |PD_and(d1, d2) -> (translate_decl d1) @ (translate_decl d2)
    |PD_eq(eq) -> [translate_eq eq]
    |PD_clk(id, expr) -> [{ peq_lustre_patt = { ppatt_desc = PP_ident(id);
                                                ppatt_loc  = d.pdecl_loc };
                                 peq_lustre_expr = translate_expr expr}]
    |PD_let_in(d1, d2) -> (translate_decl d1) @ (translate_decl d2)
    |PD_match(expr, case_list) -> translate_decl (cMatch expr case_list)
    |PD_reset(decl, expr) -> raise NotImplemented
    |PD_automaton(case_list) -> raise NotImplemented
  in
  translate_aux d
and translate_expr e =
  let rec translate_desc desc =
    match e.pexpr_desc with
    |East.PE_const(c) -> Ast_lustre.PEL_const(c)
    |East.PE_ident(id) -> Ast_lustre.PEL_ident(id)
    |East.PE_uop(op, e) -> Ast_lustre.PEL_op(op, translate_expr e)
    |East.PE_bop(op, e1, e2) -> Ast_lustre.PEL_binop(op, translate_expr e1, translate_expr e2)
    |East.PE_if(e, e1, e2) -> Ast_lustre.PEL_if (translate_expr e,
                                                 translate_expr e1,
                                                 translate_expr e2)
    |East.PE_app(id, expr_list) -> raise NotImplemented
    |East.PE_arrow(e1, e2) -> raise NotImplemented
    |East.PE_pre(e) -> raise NotImplemented
    |East.PE_tuple(expr_list) -> raise NotImplemented
    |East.PE_when(e1, id, e2) -> Ast_lustre.PEL_when (translate_expr e1,
                                                      id,
                                                      translate_expr e2)
    |East.PE_merge(e, case_list) -> raise NotImplemented
    |East.PE_last(id) -> raise NotImplemented
  in
  { Ast_lustre.pexpr_lustre_desc = translate_desc e.pexpr_desc;
    Ast_lustre.pexpr_lustre_clk  = CK_base;
    Ast_lustre.pexpr_lustre_loc  = e.pexpr_loc}
and translate_shared sv =
  match sv with
  |PSV_let(decl, sc) -> raise NotImplemented
  |PSV_do(decl, wc) -> raise NotImplemented
and translate_strong sc =
  match sc with
  |PSC_unless_then(expr, id, sc) -> raise NotImplemented
  |PSC_unless_cont(expr, id, sc) -> raise NotImplemented
  |PSC_epsilon -> raise NotImplemented
and translate_weak wc =
  match wc with
  |PWC_until_then(expr, id, wc) -> raise NotImplemented
  |PWC_until_cont(expr, id, wc) -> raise NotImplemented
  |PWC_epsilon -> raise NotImplemented
;;

let translate_node n =
  {
    pn_lustre_name = n.pn_name;
    pn_lustre_input = List.map translate_param n.pn_input;
    pn_lustre_output = List.map translate_param n.pn_output;
    pn_lustre_local = List.map translate_param n.pn_local;
    pn_lustre_equs = translate_decl n.pn_decl;
    pn_lustre_loc = n.pn_loc;
  }

let rec translate_nodes ln =
  match ln with
  |[]   -> []
  |p::q -> (translate_node p) :: (translate_nodes q)
;;

let translate_file f =
  let (type_decls, nodes) = f in
  let eqs = translate_nodes nodes in
  (type_decls, [], eqs)
;;
