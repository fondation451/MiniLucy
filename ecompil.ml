open East;;

exception NotImplemented

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

let rec cResetD decl expr =
  let aux decl_desc expr =
    match decl_desc with
    |PD_and(d1, d2) -> PD_and(cResetD d1 expr, cResetD d2 expr)
    |PD_eq(eq) -> PD_eq({peq_patt = eq.peq_patt;
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
and cResetU sv expr =
  match sv with
  |PSV_let(d, sv) -> PSV_let(cResetD d expr, cResetU sv expr)
  |PSV_do(d, wc) -> PSV_do(cResetD d expr, cResetW wc expr)
and cResetS sc expr =
  match sc with
  |PSC_unless_then(e, id, sc) -> PSC_unless_then(cResE e expr, id, cResetS sc expr)
  |PSC_unless_cont(e, id, sc) -> PSC_unless_cont(cResE e expr, id, cResetS sc expr)
  |PSC_epsilon -> PSC_epsilon
and cResetW wc expr =
  match wc with
  |PWC_until_then(e, id, wc) -> PWC_until_then(cResE e expr, id, cResetW wc expr)
  |PWC_until_cont(e, id, wc) -> PWC_until_cont(cResE e expr, id, cResetW wc expr)
  |PWC_epsilon -> PWC_epsilon
and cResE e expr =
  let rec cResE_list l =
    List.map (fun (e) -> cResE e expr) l
  in
  let aux expr_desc =
    match expr_desc with
    |PE_const(c) -> PE_const(c);
    |PE_ident(id) -> PE_ident(id);
    |PE_op(op, expr_list) -> PE_op(op,
                                   cResE_list expr_list)
    |PE_if(e, e1, e2) -> PE_if(cResE e expr, cResE e1 expr, cResE e2 expr)
    |PE_app(id, expr_list) -> PE_app(id,
                                     cResE_list expr_list)
    |PE_arrow(e1, e2) -> let y = cResE e1 expr in
                          PE_if(expr, y, {pexpr_desc = PE_arrow(y, cResE e2 expr);
                                          pexpr_clk  = y.pexpr_clk;
                                          pexpr_loc  = y.pexpr_loc})
    |PE_pre(e) -> PE_pre(cResE e expr)
    |PE_tuple(expr_list) -> PE_tuple(cResE_list expr_list)
    |PE_when(e1, id, e2) -> PE_when(cResE e1 expr, id, cResE e2 expr)
    |PE_merge(e, case_list) -> PE_merge(cResE e expr,
                                        List.map (fun (id, e_) -> (id, cResE e_ expr))
                                                 case_list)
    |PE_last(id) -> PE_last(id)
    |PE_let_in(d, e) -> PE_let_in(cResetD d expr, cResE e expr)
  in
  {pexpr_desc = aux e.pexpr_desc;
   pexpr_clk  = e.pexpr_clk;
   pexpr_loc  = e.pexpr_loc}

let rec translate_decl d =
  match d.pdecl_desc with
  |PD_and(d1, d2) -> String.concat "" [translate_decl d1;
                                       ";\n";
                                       translate_decl d2]
  |PD_eq(eq) -> ""
  |PD_clk(id, expr) -> ""
  |PD_let_in(d1, d2) -> ""
  |PD_match(expr, case_list) -> ""
  |PD_reset(decl, expr) -> ""
  |PD_automaton(case_list) -> ""
and translate_expr e =
  let rec translate_expr_list l =
    List.map (fun e -> translate_expr e) l
  in
  match e.pexpr_desc with
  |PE_const(c) -> (match c with
                   |Cint i  -> string_of_int i
                   |Creal r -> string_of_float r)
  |PE_ident(id) -> id
  |PE_op(op, expr_list) -> String.concat (string_of_op op) (translate_expr_list expr_list)
  |PE_if(e, e1, e2) -> String.concat "" ["if ("; translate_expr e; ")\n\t";
                                         "then "; translate_expr e1; "\n";
                                         "else "; translate_expr e2]
  |PE_app(id, expr_list) -> String.concat "" [id;
                                              "(";
                                              String.concat ", " (translate_expr_list expr_list);
                                              ")"]
  |PE_arrow(e1, e2) -> String.concat "" [translate_expr e1; " -> "; translate_expr e2]
  |PE_pre(e) -> String.concat "" ["pre "; translate_expr e]
  |PE_tuple(expr_list) -> String.concat "" ["("; String.concat ", " (translate_expr_list expr_list); ")"]
  |PE_when(e1, id, e2) -> String.concat "" [translate_expr e1; " when "; id; "("; translate_expr e2; ")"]
  |PE_merge(e, case_list) -> String.concat "" ["merge\n\t"; translate_expr e;
                                               String.concat "\n\t" (List.map (fun (id, e) -> String.concat " -> " [id; translate_expr e]) case_list)]
  |PE_last(id) -> ""
  |PE_let_in(decl, expr) -> String.concat "" ["let "; translate_decl decl; " in "; translate_expr expr]
and translate_shared sv =
  match sv with
  |PSV_let(decl, sc) -> ""
  |PSV_do(decl, wc) -> ""
and translate_strong sc =
  match sc with
  |PSC_unless_then(expr, id, sc) -> ""
  |PSC_unless_cont(expr, id, sc) -> ""
  |PSC_epsilon -> ""
and translate_weak wc =
  match wc with
  |PWC_until_then(expr, id, wc) -> ""
  |PWC_until_cont(expr, id, wc) -> ""
  |PWC_epsilon -> ""
;;

let translate_file f = translate_decl f;;
