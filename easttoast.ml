open Ast;;
open Ast_lustre;;
open Ast_type;;
open East;;

exception TranslationError of string;;

let mk_decl decl_desc =
  {
    pdecl_desc = decl_desc;
    pdecl_loc  = (Lexing.dummy_pos, Lexing.dummy_pos);
  }
;;

let rec translate_elustre_ty ty =
  match ty with
  |Tint       -> Ast_type.Tint
  |Treal      -> Ast_type.Treal
  |Ttype t    -> Ast_type.Ttype t
  | _         -> raise (TranslationError "Cannot translate fun type")
;;

let rec translate_elustre_ck env ck =
  match ck with
  |CKbase              -> CK_base
  |CKid id             -> (match (try IdentMap.find id env with
                                  |Not_found -> raise (TranslationError (String.concat " " [
                                    "Couldn't find clock"; id]))) with
                           |Ttype id_ -> CK_on (CK_base, id_, id)
                           | t -> raise (TranslationError (String.concat " " [
                             id; "has type"; Elustre_printer.str_of_elustre_ty false t;
                             "which is not an enum type. Cannot use it as a clock"
                           ])))
  |CKon (ck1, id1, ck2) -> (match ck2 with
                            |CKid id -> CK_on (translate_elustre_ck env ck, id1, id)
                            | _ -> raise (TranslationError "translate_elustre_ck"))
  | _                  -> raise (TranslationError "Cannot translate fun click")

let mk_expr expr_desc ty ck =
  {
    pexpr_desc = expr_desc;
    pexpr_ty   = ty;
    pexpr_ck   = ck;
    pexpr_loc  = (Lexing.dummy_pos, Lexing.dummy_pos);
  }
;;

let mk_eq id e =
  {
    peq_patt = {
      ppatt_desc = PP_ident id;
      ppatt_loc  = Lexing.dummy_pos, Lexing.dummy_pos;
    };
    peq_expr = e;
  }
;;

let mk_param cenv name ty ck =
  {
    Ast.param_id = name;
    Ast.param_ty = translate_elustre_ty ty;
    Ast.param_ck = translate_elustre_ck cenv ck;
  }
;;

let gen_name pref i = String.concat "_" [pref; string_of_int i];;

let find_fresh pref varS =
  let search_in name varS =
    IdentTySet.fold (
      fun elt b -> let (id, _, _) = elt in
        b || (String.equal name id)
      ) varS false in
  let i = ref 0 in
  let _ = while search_in (gen_name pref !i) varS do i := !i + 1 done
  in gen_name pref !i
;;

let find_const type_decls e =
  match e.pexpr_ty with
  |Tint -> Cint 0
  |Treal -> Creal 0.0
  |Ttype id -> Cenum (try List.hd (IdentMap.find id type_decls) with
                      | _ -> raise (TranslationError (
                        String.concat "" ["Cannot find type of "; id])))
  | _ -> raise (TranslationError "find_const")
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
    |East.PE_uop(op, e) -> East.PE_uop(op,
                                   cResE e expr)
    |East.PE_bop(op, e1, e2) -> East.PE_bop(op,
                                   cResE e1 expr, cResE e2 expr)
    |East.PE_if(e, e1, e2) -> East.PE_if(cResE e expr, cResE e1 expr, cResE e2 expr)
    |East.PE_app(id, expr_list, expr) ->
       let cond = match expr.pexpr_desc with
                  |PE_const c -> (match c with
                                  |Cenum c -> if String.equal c lustre_bool_false then expr
                                              else {e with pexpr_desc = PE_bop(Op_or, e, expr)}
                                  | _ -> {e with pexpr_desc = PE_bop(Op_or, e, expr)})
                  | _ -> {e with pexpr_desc = PE_bop(Op_or, e, expr)} in
        East.PE_app(id, cResE_list expr_list, cond)
    |East.PE_fby(c, e2) ->
        let e_c = {e with pexpr_desc = PE_const c} in
        East.PE_if(expr, e_c, { e with pexpr_desc = East.PE_fby(c, cResE e2 expr)})
    |East.PE_when(e1, id, e2) -> East.PE_when(cResE e1 expr, id, cResE e2 expr)
    |East.PE_merge(e, case_list) -> East.PE_merge(cResE e expr,
                                        List.map (fun (id, e_) -> (id, cResE e_ expr))
                                                 case_list)
    | _ -> expr_desc
  in
  { e with pexpr_desc = aux e.pexpr_desc }

(* Translation functions *)

let fv_p patt =
  match patt.ppatt_desc with
  |PP_ident id      -> IdentSet.add id IdentSet.empty
  |PP_tuple id_list -> List.fold_right (fun id env -> IdentSet.add id env)
                                        id_list
                                        IdentSet.empty
;;

let rec def_d decl =
  match decl.pdecl_desc with
  |PD_skip                     -> IdentTySet.empty
  |PD_and (d1, d2)             -> IdentTySet.union (def_d d1) (def_d d2)
  |PD_eq  eq                   -> IdentSet.fold (fun elt set -> IdentTySet.add (elt,
                                                                                eq.peq_expr.pexpr_ty,
                                                                                eq.peq_expr.pexpr_ck) set)
                                                (fv_p eq.peq_patt) IdentTySet.empty
  |PD_clk (id, e)              -> IdentTySet.add id IdentTySet.empty
  |PD_let_in (_, d)            -> def_d d
  |PD_match  (e, id_d_list)    -> List.fold_right (fun (_, decl) env -> IdentTySet.union (def_d decl) env)
                                                   id_d_list
                                                   IdentTySet.empty
  |PD_reset (d, e)             -> def_d d
  |PD_automaton (id_psv_psc_l) -> List.fold_right (fun (_, psv, _) env -> IdentTySet.union (def_psv psv) env)
                                                   id_psv_psc_l
                                                   IdentTySet.empty

and def_psv psv =
  match psv with
  |PSV_let (d, psv) -> def_psv psv
  |PSV_do  (d, pwc) -> def_d d
;;

let rec fv_e type_decls exp =
  let fv_e_list e_list =
    List.fold_right (fun e env -> IdentTySet.union (fv_e type_decls e) env)
                     e_list
                     IdentTySet.empty in
  match exp.pexpr_desc with
  |PE_const _              -> IdentTySet.empty
  |PE_ident id             -> let (id_, _, _) = id in
                              (try let _ = Etype.type_of_const type_decls (Cenum id_) in IdentTySet.empty
                               with |Not_found -> IdentTySet.add id IdentTySet.empty)
  |PE_uop (_, e)           -> fv_e type_decls e
  |PE_bop (_, e1, e2)      -> IdentTySet.union (fv_e type_decls e1) (fv_e type_decls e2)
  |PE_if  (e1, e2, e3)     -> IdentTySet.union (IdentTySet.union (fv_e type_decls e1) (fv_e type_decls e2)) (fv_e type_decls e3)
  |PE_app (id, e_list, e)  -> IdentTySet.union (fv_e_list e_list) (fv_e type_decls e)
  |PE_fby (c, e)           -> fv_e type_decls e
  |PE_pre (e)              -> fv_e type_decls e
  |PE_when (e1, _, e2)     -> IdentTySet.union (fv_e type_decls e1) (fv_e type_decls e2)
  |PE_merge (e, id_e_list) -> IdentTySet.union (fv_e type_decls e) (fv_e_list (snd (List.split id_e_list)))
  |PE_last id              -> IdentTySet.add id IdentTySet.empty
;;

let rec fv_d type_decls decl =
  match decl.pdecl_desc with
  |PD_skip            -> IdentTySet.empty
  |PD_and (d1, d2)    -> IdentTySet.union (fv_d type_decls d1) (fv_d type_decls d2)
  |PD_eq  eq          -> IdentTySet.union (fv_e type_decls eq.peq_expr) IdentTySet.empty
  |PD_clk (id, e)     -> IdentTySet.union (fv_e type_decls e) IdentTySet.empty
  |PD_let_in (d1, d2) -> IdentTySet.union (fv_d type_decls d1) (IdentTySet.diff (fv_d type_decls d2) (def_d d1))
  |PD_match(e, case_list)  -> IdentTySet.union (fv_e type_decls e) (
    List.fold_right (fun (id, d) env -> IdentTySet.union (fv_d type_decls d) env)
                     case_list IdentTySet.empty
    )
  |PD_reset(d, e)          -> IdentTySet.union (fv_d type_decls d) (fv_e type_decls e)
  |PD_automaton(case_list) -> (
    List.fold_right (fun (id, psv, psc) env -> IdentTySet.union (IdentTySet.union (fv_psv type_decls psv) (fv_psc type_decls psc)) env)
                     case_list
                     IdentTySet.empty
    )
and fv_psv type_decls psv =
  match psv with
  |PSV_let (d, psv_) -> IdentTySet.union (fv_d type_decls d) (fv_psv type_decls psv_)
  |PSV_do  (d, pwc)  -> IdentTySet.union (fv_d type_decls d) (fv_pwc type_decls pwc)
and fv_psc type_decls psc =
  match psc with
  |PSC_unless_then (e, id, psc_) -> IdentTySet.union (fv_e type_decls e) (fv_psc type_decls psc_)
  |PSC_unless_cont (e, id, psc_) -> IdentTySet.union (fv_e type_decls e) (fv_psc type_decls psc_)
  |PSC_epsilon                   -> IdentTySet.empty
and fv_pwc type_decls pwc =
  match pwc with
  |PWC_until_then (e, id, pwc_) -> IdentTySet.union (fv_e type_decls e) (fv_pwc type_decls pwc_)
  |PWC_until_cont (e, id, pwc_) -> IdentTySet.union (fv_e type_decls e) (fv_pwc type_decls pwc_)
  |PWC_epsilon                   -> IdentTySet.empty
;;

let cOn type_decls d c_id c_e =
  let rec replace_var_e var_id e =
    let replace_var_e_list e_list =
      List.map (fun e -> replace_var_e var_id e) e_list
    in
    { e with pexpr_desc = let (c_id, _, _) = c_id in
      match e.pexpr_desc with
      |East.PE_const(c)                -> East.PE_const(c)
      |East.PE_ident(id_)              -> let (_id_, _, _) = id_ in
                                          if String.equal var_id _id_ then
                                           East.PE_when(e, c_id, c_e)
                                          else East.PE_ident(id_)
      |East.PE_uop(op, e_)             -> East.PE_uop(op, replace_var_e var_id e_)
      |East.PE_bop(op, e1, e2)         -> East.PE_bop(op, replace_var_e var_id e1, replace_var_e var_id e2)
      |East.PE_if(e, e1, e2)           -> East.PE_if(e, replace_var_e var_id e1, replace_var_e var_id e2)
      |East.PE_app(f_id, expr_list, e) -> East.PE_app(f_id, replace_var_e_list expr_list, replace_var_e var_id e)
      |East.PE_fby(c, e)               -> East.PE_fby(c, replace_var_e var_id e)
      |East.PE_pre(e)                  -> East.PE_pre(replace_var_e var_id e)
      |East.PE_when(e1, ck_id, e2)     -> East.PE_when(replace_var_e var_id e1, ck_id, replace_var_e var_id e2)
      |East.PE_merge(e, case_list)     -> East.PE_merge(replace_var_e var_id e, case_list)
      |East.PE_last(id_)               -> let (_id_, _, _) = id_ in
                                          if String.equal var_id _id_ then
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

  IdentTySet.fold (fun var_id d ->
      let (var_id, _, _) = var_id in replace_var_d var_id d) (fv_d type_decls d) d
;;

let rec split varN decl =
  match decl.pdecl_desc with
  |PD_skip             -> ([], [])
  |PD_let_in (d1, d2)
  |PD_and (d1, d2)     -> let (in1, out1) = split varN d1 in
                                let (in2, out2) = split varN d2 in
                                (in1 @ in2, out1 @ out2)
  |PD_eq(eq)           -> (match eq.peq_patt.ppatt_desc with
                                 |PP_ident id ->
                                  if IdentTySet.mem (id,
                                                     eq.peq_expr.pexpr_ty,
                                                     eq.peq_expr.pexpr_ck) varN then
                                    ([decl], []) else ([], [decl])
                                 |_ -> raise (TranslationError "Tuples are not permitted"))
  |PD_clk(ck_id, expr) -> if IdentTySet.mem ck_id varN then
                                ([decl], []) else ([], [decl])
  |PD_reset (d, e)     -> let def_ = def_d d in
                                if IdentTySet.equal def_ varN then
                                ([decl], []) else ([], [decl])
  |_                   -> ([], [decl])
;;

let proj var c_id c_e decl =
  let in_pat v p =
    let (v, _, _) = v in
    match p.ppatt_desc with
    |PP_ident id      -> String.equal id v
    |PP_tuple id_list -> List.mem v id_list
  in
  let rec find v d =
    match d.pdecl_desc with
    |PD_skip                   -> None
    |PD_and(d1, d2)            -> let vD1 = find v d1 in
                                  if vD1 == None then find v d2 else vD1
    |PD_eq(eq)                 -> if in_pat v eq.peq_patt then Some eq.peq_expr
                                  else None
    |PD_clk(ck_id, expr)       -> let (ck_id, _, _) = ck_id in
                                  let (v,     _, _) = v in
                                  if String.equal ck_id v then Some expr
                                  else None
    |PD_let_in(d1, d2)         -> find v d2
    |PD_match(expr, case_list) -> List.fold_right (
      fun (_, d) prev -> let curr = find v d in
                         if curr == None then prev else curr
      ) case_list None
    |PD_reset(decl, expr)      -> find v decl
    |PD_automaton(case_list)   -> List.fold_right (
      fun (_, psv, _) prev -> match psv with |PSV_let (d,_) |PSV_do (d,_) ->
                              let curr = find v d in
                              if curr == None then prev else curr
      ) case_list None
  in
  match find var decl with
  |None -> let (id, ty, ck) = var in
           let (c_id, _, _) = c_id in
           mk_expr (
            PE_when (
              mk_expr (
                PE_pre (mk_expr (PE_ident var) ty ck)
                      ) ty ck,
                  c_id, c_e
                    )
                   ) ty ck
  |Some e -> e
;;

let cMatch type_decls cenv vars e case_list =
  let find_fresh expr d_l =
    let fv_d_l = List.fold_right (
      fun d fv_d_p -> IdentTySet.union (fv_d type_decls d) fv_d_p
      ) d_l IdentTySet.empty in
    let fv_d_e = IdentTySet.union (fv_e type_decls e) fv_d_l in
    find_fresh "clk_match" fv_d_e
  in

  let rec flatten_d_l d_l =
    match d_l with
    |[]   -> mk_decl PD_skip
    |[p]  -> p
    |p::q -> { p with pdecl_desc = PD_and (p, flatten_d_l q) }
  in

  let make_merge var cond ids gi =
    let (c_id, c_ty, c_ck) = cond in
    let cond_e = mk_expr (PE_ident cond) c_ty c_ck in

    let rec make_cases ids gi =
      match ids, gi with
      |[], []         -> []
      |p1::q1, p2::q2 ->
        (p1, proj var p1 cond_e p2) :: (make_cases q1 q2)
      |_              -> raise (TranslationError "make_merge")
    in
    let (id, ty, ck) = var in

    mk_decl (PD_eq {
      peq_patt = {
        ppatt_desc = PP_ident id;
        ppatt_loc = (Lexing.dummy_pos, Lexing.dummy_pos);
      };
      peq_expr = mk_expr (PE_merge (cond_e, make_cases ids gi)) ty ck
    })
  in

  let (ids, di_l, ni_l) = List.fold_right (
    fun (id, d) (ids, di_p, ni_p) ->
      let ni = def_d d in (id :: ids, d :: di_p, ni :: ni_p)
    ) case_list ([], [], []) in
  let clk_id, decl, clk_d, vars, cenv =
    match e.pexpr_desc with
      |PE_ident (i, ty, ck) -> let cenv = IdentMap.add i ty cenv in
        (i, ty, ck), [], mk_decl PD_skip, vars, cenv
      |_ ->
        let clk_name = find_fresh e di_l in
        let clk_id = (clk_name, e.pexpr_ty, e.pexpr_ck) in
        let cenv = IdentMap.add clk_name e.pexpr_ty cenv in
        let clk_d = { (List.hd di_l) with pdecl_desc = PD_clk (clk_id, e)} in
        clk_id, [mk_param cenv clk_name e.pexpr_ty e.pexpr_ck],
                clk_d,
                IdentTySet.add clk_id vars,
                cenv in
  let (c_id, c_ty, c_ck) = clk_id in
  let (gi_l, d'i_l) = List.fold_right (
    fun (id, (di, ni)) (gi_p, d'i_p) ->
      let (gi, d'i) = split ni (cOn type_decls di id (mk_expr (PE_ident clk_id) c_ty c_ck)) in
      ((flatten_d_l gi) :: gi_p, (flatten_d_l d'i) :: d'i_p)
    ) (List.combine ids (List.combine di_l ni_l)) ([], []) in
  let and_d' = flatten_d_l d'i_l in
  let ni_u = List.fold_right IdentTySet.union ni_l IdentTySet.empty in

  let merge_list = IdentTySet.fold (
    fun var m_l -> (make_merge var clk_id ids gi_l) :: m_l
    ) ni_u [] in

  let merge_d = flatten_d_l merge_list in

  decl, cenv, vars, mk_decl (PD_and (and_d', mk_decl (PD_and (clk_d, merge_d))))
;;

let cAutomaton type_decls cenv vars id_psv_psc_l =
  let rec translate_aux new_name n_loc d se re s expr const =
    let new_id   = (new_name, expr.pexpr_ty, expr.pexpr_ck) in
    let new_loc = mk_param cenv new_name expr.pexpr_ty expr.pexpr_ck in
    let x_e = mk_decl (PD_eq {
      peq_patt = { ppatt_desc = PP_ident new_name;
                   ppatt_loc  = Lexing.dummy_pos, Lexing.dummy_pos };
      peq_expr = expr;
    }) in
    let (id, ty, ck) = s in
    let d  = mk_decl (PD_and(x_e, d)) in
    let se = mk_expr (PE_if(mk_expr (PE_ident new_id) expr.pexpr_ty expr.pexpr_ck,
                            (mk_expr (PE_const (Cenum id)) ty ck),
                            se)) ty ck in
    let re = mk_expr (PE_if(mk_expr (PE_ident new_id) expr.pexpr_ty expr.pexpr_ck,
                            (mk_expr (PE_const (Cenum const)) bool_type ck),
                            re)) bool_type ck in
    (n_loc @ [new_loc], d, se, re)
  and translate_shared vars s sv =
    match sv with
    |PSV_let(decl, sv_) -> let (n_loc, vars, d', se, re) = translate_shared vars s sv_ in
      (n_loc, vars, {decl with pdecl_desc = PD_and(decl, d')}, se, re)
    |PSV_do(decl, wc) -> let (n_loc, vars, d', se, re) = translate_weak vars s wc in
      (n_loc, vars, {decl with pdecl_desc = PD_and(decl, d')}, se, re)
  and translate_strong vars s sc =
    match sc with
    |PSC_unless_then(expr, id, sc) ->
      let n_loc, vars, d, se, re = translate_strong vars s sc in
      let vars_ = IdentTySet.union (fv_psc type_decls sc) (IdentTySet.union (fv_e type_decls expr) (vars)) in
      let new_name = find_fresh "strong_cond" vars_ in
      let decl, d, se, re = translate_aux new_name n_loc d se re id expr lustre_bool_true in
      decl, IdentTySet.add (new_name, expr.pexpr_ty, expr.pexpr_ck) vars, d, se, re
    |PSC_unless_cont(expr, id, sc) ->
      let n_loc, vars, d, se, re = translate_strong vars s sc in
      let vars_ = IdentTySet.union (fv_psc type_decls sc) (IdentTySet.union (fv_e type_decls expr) (vars)) in
      let new_name = find_fresh "strong_cond" vars_ in
      let decl, d, se, re = translate_aux new_name n_loc d se re id expr lustre_bool_false in
      decl, IdentTySet.add (new_name, expr.pexpr_ty, expr.pexpr_ck) vars, d, se, re
    |PSC_epsilon -> let id, ty, ck = s in
                    ([], vars,
                         mk_decl PD_skip,
                         mk_expr (PE_ident s) ty ck,
                         mk_expr (PE_const const_false) bool_type ck)
  and translate_weak vars s wc =
    match wc with
    |PWC_until_then(expr, id, wc) ->
      let n_loc, vars, d, se, re = translate_weak vars s wc in
      let vars_ = IdentTySet.union (fv_pwc type_decls wc) (IdentTySet.union (fv_e type_decls expr) (vars)) in
      let new_name = find_fresh "weak_cond" vars_ in
      let decl, d, se, re = translate_aux new_name n_loc d se re id expr lustre_bool_true in
      decl, IdentTySet.add (new_name, expr.pexpr_ty, expr.pexpr_ck) vars, d, se, re
    |PWC_until_cont(expr, id, wc) ->
      let n_loc, vars, d, se, re = translate_weak vars s wc in
      let vars_ = IdentTySet.union (fv_pwc type_decls wc) (IdentTySet.union (fv_e type_decls expr) (vars)) in
      let new_name = find_fresh "weak_cond" vars_ in
      let decl, d, se, re = translate_aux new_name n_loc d se re id expr lustre_bool_false in
      decl, IdentTySet.add (new_name, expr.pexpr_ty, expr.pexpr_ck) vars, d, se, re
    |PWC_epsilon -> let _, ty, ck = s in
                    ([], vars,
                         mk_decl PD_skip,
                         mk_expr (PE_ident s) ty ck,
                         mk_expr (PE_const const_false) bool_type ck)
  in
  let make_match_case d se re pnr esc res=
    let   _, ty, ck = pnr in
    let esc,  _,  _ = esc in
    let res,  _,  _ = res in
    mk_decl (PD_reset (
      mk_decl (PD_and ( mk_decl (PD_and (
        mk_decl (PD_eq (mk_eq esc se)),
        mk_decl (PD_eq (mk_eq res re)))),
        d)),
      mk_expr (PE_ident pnr) ty ck
      )) in
  let translate_case (id, d, d') pnr pns_s pns_r s_ns s_nr =
    let n_loc, d, se, re = d in
    let n_loc', d', se', re' = d' in
    let d  = make_match_case d se re pnr pns_s pns_r in
    let d' = make_match_case d' se' re' pnr s_ns s_nr in
    (n_loc @ n_loc', id, d, d') in
  let translations, vars = List.fold_right (
    fun (id, psv, psc) (trans, vars)->
      let decl, vars, d, se, re = translate_shared vars id psv in
      let psv = (decl, d, se, re) in
      let decl, vars, d, se, re = translate_strong vars id psc in
      let psc = (decl, d, se, re) in
      ((id, psv, psc) :: trans, vars)
    ) id_psv_psc_l ([], vars) in
  let vars = IdentTySet.union vars (List.fold_left (
    fun set (id, d, d') ->
    let (_, d, se, re) = d in let (_, d', se', re') = d' in
    IdentTySet.union (IdentTySet.union (IdentTySet.union (IdentTySet.union
      (IdentTySet.union (IdentTySet.union (IdentTySet.union set (fv_d type_decls d)) (fv_d type_decls d'))
      (fv_e type_decls se)) (fv_e type_decls se')) (fv_e type_decls se')) (fv_e type_decls re)) (fv_e type_decls re')
    ) IdentTySet.empty translations) in
  let s1, _, _ = List.hd id_psv_psc_l in
  let (s_id, s_ty, s_ck) = s1 in
  let pns_name   = find_fresh "pns" vars in
  let pnr_name   = find_fresh "pnr" vars in
  let pns_s_name = find_fresh "pns_s" vars in
  let pns_r_name = find_fresh "pns_r" vars in
  let s_ns_name  = find_fresh "s_ns" vars in
  let s_nr_name  = find_fresh "s_nr" vars in
  let pns        = (pns_name, s_ty, s_ck) in
  let pnr        = (pnr_name, bool_type, s_ck) in
  let pns_s      = (pns_s_name, s_ty, s_ck) in
  let pns_r      = (pns_r_name, bool_type, s_ck) in
  let s_ns       = (s_nr_name, s_ty, s_ck) in
  let s_nr       = (s_ns_name, bool_type, s_ck) in
  let pns_loc    = mk_param cenv pns_name s_ty s_ck in
  let pnr_loc    = mk_param cenv pnr_name bool_type s_ck in
  let pns_s_loc  = mk_param cenv pns_s_name s_ty s_ck in
  let pns_r_loc  = mk_param cenv pns_r_name bool_type s_ck in
  let s_ns_loc   = mk_param cenv s_ns_name s_ty s_ck in
  let s_nr_loc   = mk_param cenv s_nr_name bool_type s_ck in
  let vars = IdentTySet.add pnr vars in
  let vars = IdentTySet.add pns vars in
  let vars = IdentTySet.add pns_s vars in
  let vars = IdentTySet.add pns_r vars in
  let vars = IdentTySet.add s_ns vars in
  let vars = IdentTySet.add s_nr vars in
  let cases = List.map (
    fun elt -> translate_case elt pnr pns_s pns_r s_ns s_nr
    ) translations in
  let n_loc, match_pns, match_s = List.fold_right (
    fun (loc, id, d, d') (n_loc, m_pns, m_s) ->
      (loc @ n_loc, (id, d) :: m_pns, (id, d') :: m_s)
    ) cases ([], [], []) in
  let clk_pns   = mk_decl (PD_clk(pns, mk_expr (PE_fby(Cenum s_id, mk_expr (PE_ident s_ns) s_ty s_ck)) s_ty s_ck)) in
  let clk_pnr   = mk_decl (PD_clk(pnr, mk_expr (PE_fby(const_false, mk_expr (PE_ident s_nr) bool_type s_ck)) bool_type s_ck)) in
  let cenv = IdentMap.add pns_name s_ty cenv in
  let cenv = IdentMap.add pnr_name bool_type cenv in
  let match_pns = mk_decl (PD_match( mk_expr (PE_ident pns) s_ty s_ck, match_pns )) in
  let match_s   = mk_decl (PD_match( mk_expr (PE_ident pns_s) s_ty s_ck, match_s )) in
  (pnr_loc :: pns_loc :: pns_s_loc :: pns_r_loc :: s_ns_loc :: s_nr_loc :: n_loc,
    cenv,
    vars,
    mk_decl (PD_and (
      mk_decl (PD_and (match_pns, match_s)),
      mk_decl (PD_and (clk_pns, clk_pnr))
    )))
;;

let rec translate_param cenv p =
  {
    Ast.param_id = p.param_id;
    Ast.param_ty = translate_elustre_ty p.param_ty;
    Ast.param_ck = translate_elustre_ck cenv p.param_ck;
  }

let rec translate_eq type_decls cenv vars eq =
  let (decl, vars, e) = translate_expr type_decls cenv vars eq.peq_expr in
  (decl, vars, { Ast_lustre.peq_lustre_patt = { ppatt_desc = eq.peq_patt.ppatt_desc;
                                                ppatt_loc  = eq.peq_patt.ppatt_loc};
                 Ast_lustre.peq_lustre_expr = e})
and translate_decl type_decls cenv vars d =
  let rec translate_aux d =
    match d.pdecl_desc with
    |PD_skip        -> [], cenv, vars, []
    |PD_and(d1, d2) ->
      let (decls1, cenv, vars, d1) = translate_decl type_decls cenv vars d1 in
      let (decls2, cenv, vars, d2) = translate_decl type_decls cenv vars d2 in
      (decls1 @ decls2, cenv, vars, d1 @ d2)
    |PD_eq(eq) ->
      let (decl, vars, eq_) = translate_eq type_decls cenv vars eq in
      let (n_loc, n_vars) = List.split decl in
      (n_loc, cenv, vars, n_vars @ [eq_])
    |PD_clk(id, expr) ->
      let (decl, vars, e) = translate_expr type_decls cenv vars expr in
      let (c_id, _, _) = id in
      let cenv = IdentMap.add c_id expr.pexpr_ty cenv in
      let (n_loc, n_vars) = List.split decl in
      let vars = IdentTySet.add id vars in
      (n_loc, cenv, vars, n_vars @
      [{ peq_lustre_patt = { ppatt_desc = PP_ident(c_id);
                             ppatt_loc  = d.pdecl_loc };
         peq_lustre_expr = e }])
    |PD_let_in(d1, d2) ->
      let d_vars = def_d d1 in
      let vars = IdentTySet.union d_vars vars in
      let (decls1, cenv, vars, d1) = translate_decl type_decls cenv vars d1 in
      let (decls2, cenv, vars, d2) = translate_decl type_decls cenv vars d2 in
      ((decls1 @ decls2), cenv, vars, (d1 @ d2))
    |PD_match(expr, case_list) ->
      let (decls1, cenv, vars, d) = cMatch type_decls cenv vars expr case_list in
      let (decls2, cenv, vars, d) = translate_decl type_decls cenv vars d in
      (decls1 @ decls2, cenv, vars, d)
    |PD_reset(decl, expr) ->
      let (decls, cenv, vars, decl) = translate_decl type_decls cenv vars (cResetD decl expr) in
      (decls, cenv, vars, decl)
    |PD_automaton(case_list) ->
      let n_loc, cenv, vars, d = cAutomaton type_decls cenv vars case_list in
      let n_loc', cenv, vars, d' = translate_decl type_decls cenv vars d in
      (n_loc @ n_loc', cenv, vars, d')
  in
  translate_aux d
and translate_expr type_decls cenv vars e =
  let rec translate_desc desc =
    match desc with
    |East.PE_const(c) -> ([], vars, Ast_lustre.PEL_const(c))
    |East.PE_ident(id) -> let id, _, _ = id in
                          ([], vars, Ast_lustre.PEL_ident(id))
    |East.PE_uop(op, e) -> let (decls, vars, e') = translate_expr type_decls cenv vars e in
      (decls, vars, Ast_lustre.PEL_op(op, e'))
    |East.PE_bop(op, e1, e2) ->
      let (decls1, vars, e1) = translate_expr type_decls cenv vars e1 in
      let (decls2, vars, e2) = translate_expr type_decls cenv vars e2 in
      ((decls1 @ decls2), vars, Ast_lustre.PEL_binop(op, e1, e2))
    |East.PE_if(e, e1, e2) ->
      let (decls,  vars, e)  = translate_expr type_decls cenv vars e in
      let (decls1, vars, e1) = translate_expr type_decls cenv vars e1 in
      let (decls2, vars, e2) = translate_expr type_decls cenv vars e2 in
      ((decls @ decls1 @ decls2, vars, Ast_lustre.PEL_if(e, e1, e2)))
    |East.PE_app(id, expr_list, e) ->
      let a_id, _, _ = id in
      let (decl, vars, e_l) = List.fold_right (
        fun expr (decls, vars, e_l) ->
          let d, vars, e = translate_expr type_decls cenv vars expr in
          (d @ decls, vars, e :: e_l)
        ) expr_list ([], vars, []) in
      let new_name = find_fresh "app_every" vars in
      let new_id   = (new_name, e.pexpr_ty, e.pexpr_ck) in
      let vars = IdentTySet.add new_id vars in
      let (decl_e, vars, cond) = translate_expr type_decls cenv vars e in
      let new_decl = [{
        Ast.param_id = new_name;
        Ast.param_ty = translate_elustre_ty e.pexpr_ty;
        Ast.param_ck = cond.pexpr_lustre_clk;
      },
      {
        peq_lustre_patt = {
          ppatt_desc = PP_ident new_name;
          ppatt_loc  = Lexing.dummy_pos, Lexing.dummy_pos;
        };
        peq_lustre_expr = cond;
      }] in
      (match e.pexpr_desc with
       |PE_ident id_ -> let id_, _, _ = id_ in
        (decl, vars, Ast_lustre.PEL_app(a_id, e_l, id_))
       |_ -> ((decl @ decl_e @ new_decl), vars, Ast_lustre.PEL_app(a_id, e_l, new_name)))
    |East.PE_fby(c, e) ->
      let (decl, vars, e) = translate_expr type_decls cenv vars e in
      (decl, vars, Ast_lustre.PEL_fby(c, e))
    |East.PE_pre(e) ->
      let c = find_const type_decls e in
      let (decl, vars, e) = translate_expr type_decls cenv vars e in
      (decl, vars, Ast_lustre.PEL_fby(c, e))
    |East.PE_when(e1_, id, e2_) ->
      let new_name = find_fresh "when_expr" vars in
      let new_ck   = CKon (e2_.pexpr_ck, id, CKid new_name) in
      let new_id   = (new_name, e2_.pexpr_ty, new_ck) in
      let new_e2 = mk_expr (PE_ident new_id) e2_.pexpr_ty new_ck in
      let (decl1, vars, e1) = translate_expr type_decls cenv vars e1_ in
      let (decl2, vars, e2) = translate_expr type_decls cenv vars new_e2 in
      let new_param = mk_param cenv new_name e2_.pexpr_ty new_ck in
      let new_decl = {
        peq_lustre_patt = {
          ppatt_desc = PP_ident new_name;
          ppatt_loc  = Lexing.dummy_pos, Lexing.dummy_pos;
        };
        peq_lustre_expr = e2;
      } in
      (((new_param, new_decl) :: decl1 @ decl2), vars, Ast_lustre.PEL_when (e1, id, e2))
    |East.PE_merge(e, case_list) ->
      let (decl, vars, cases) = List.fold_right (
        fun ((id, ty, ck), e) (decls, vars, cases) ->
          let d, vars, e = translate_expr type_decls cenv vars e in
          (d @ decls, vars, (id, e) :: cases)
        ) case_list ([], vars, []) in
      (match e.pexpr_desc with
        |East.PE_ident id -> let id, _, _ = id in
          (decl, vars, Ast_lustre.PEL_merge(id, cases))
        |_ ->
          let new_name = find_fresh "when" vars in
          let new_id   = (new_name, e.pexpr_ty, e.pexpr_ck) in
          let vars = IdentTySet.add new_id vars in
          let (merge_decl, vars, merge_expr) = translate_expr type_decls cenv vars e in
          let new_decl =
            [{
              Ast.param_id = new_name;
              Ast.param_ty = translate_elustre_ty e.pexpr_ty;
              Ast.param_ck = translate_elustre_ck cenv e.pexpr_ck;
            },
            {
              peq_lustre_patt = {ppatt_desc = PP_ident new_name;
                                 ppatt_loc  = (Lexing.dummy_pos, Lexing.dummy_pos)};
              peq_lustre_expr = merge_expr;
            }] in
          let decl = decl @ merge_decl @ new_decl in
          (decl, vars, Ast_lustre.PEL_merge(new_name, cases)))
    |East.PE_last(id) -> let (id, _, ck) = id in
      let id_e = { Ast_lustre.pexpr_lustre_desc = Ast_lustre.PEL_ident id;
                   Ast_lustre.pexpr_lustre_clk  = translate_elustre_ck cenv ck;
                   Ast_lustre.pexpr_lustre_loc  = e.pexpr_loc } in
      ([], vars, Ast_lustre.PEL_current (id_e))
  in
  let (decl, vars, e_lustre) = translate_desc e.pexpr_desc in
  (decl, vars, { Ast_lustre.pexpr_lustre_desc = e_lustre;
                Ast_lustre.pexpr_lustre_clk  = CK_base;
                Ast_lustre.pexpr_lustre_loc  = e.pexpr_loc})
;;

let translate_node type_decls n =
  let cenv = IdentMap.empty in let vars = IdentTySet.empty in
  let (new_locals, cenv, _, d) = translate_decl type_decls cenv vars n.pn_decl in
  {
    pn_lustre_name = n.pn_name;
    pn_lustre_input = List.map (translate_param cenv) n.pn_input;
    pn_lustre_output = List.map (translate_param cenv) n.pn_output;
    pn_lustre_local = (List.map (translate_param cenv) n.pn_local) @ new_locals;
    pn_lustre_equs = d;
    pn_lustre_loc = n.pn_loc;
  }

let rec translate_nodes type_decls ln =
  match ln with
  |[]   -> []
  |p::q -> (translate_node type_decls p) :: (translate_nodes type_decls q)
;;

let translate_file f =
  let (type_decls, nodes) = f in
  let eqs = translate_nodes type_decls nodes in
  (type_decls, [], eqs)
;;
