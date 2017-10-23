(*
########
Copyright � 2017

This file is part of MiniLucy.
MiniLucy is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License v3 as published by
the Free Software Foundation.

Nicolas ASSOUAD <nicolas.assouad@ens.fr>
Cl�ment PASCUTTO <clement.pascutto@ens.fr>
########
*)

%{

  open East;;
  open Parsing;;

  let loc () = symbol_start_pos (), symbol_end_pos ();;
  let mk_expr e = {pexpr_desc = e; pexpr_clk = None; pexpr_loc = loc ()};;
  let mk_decl d = {pdecl_desc = d; pdecl_loc = loc ()}
  let mk_patt p = {ppatt_desc = p; ppatt_loc = loc ()};;
  let mk_param id ty ck = {param_id = id; param_ty = ty; param_ck = ck;};;

%}

%token AND
%token ARROW
%token COMMA
%token <East.op> COMP
%token <int> CONST_INT
%token <float> CONST_REAL
%token DIV
%token ELSE
%token EOF
%token EQUAL
%token FBY
%token NEQ
%token <string> IDENT
%token IF
%token IMPL
%token LET
%token LPAREN
%token MINUS
%token MERGE
%token MOD
%token NOT
%token OR
%token PLUS
%token RPAREN
%token SEMICOL
%token SLASH
%token STAR
%token THEN
%token WHEN

%token CLK
%token LAST
%token UNTIL
%token UNLESS
%token IN
%token AUTOMATON
%token EVERY
%token VBAR
%token CONTINUE
%token DO
%token WITH
%token RESET
%token MATCH


%nonassoc ELSE
%right ARROW
%left IMPL
%left OR
%left AND
%left COMP EQUAL NEQ                          /* < <= > >= <> = <> */
%left PLUS MINUS                              /* + -  */
%left STAR SLASH DIV MOD                      /* * /  mod */
%nonassoc NOT                                 /* not */

/* Point d'entr�e */

%start file
%type <East.p_file> file

%%

file: decl EOF {$1}
;

decl:
|decl AND decl {mk_decl (PD_and($1, $3))}
|decl SEMICOL decl {mk_decl (PD_and($1, $3))}
|pattern EQUAL expr {mk_decl (PD_eq({peq_patt = $1; peq_expr = $3}))}
|CLK IDENT EQUAL expr {mk_decl (PD_clk($2, $4))}
|LET decl IN decl {mk_decl (PD_let_in($2, $4))}
|MATCH expr WITH match_case_list {mk_decl (PD_match($2, $4))}
|RESET decl EVERY expr {mk_decl (PD_reset($2, $4))}
|AUTOMATON automaton_case_list {mk_decl (PD_automaton($2))}
;

pattern:
|IDENT {mk_patt (PP_ident $1)}
|LPAREN ident_comma_list RPAREN {mk_patt (PP_tuple($2))}
;

ident_comma_list:
|separated_nonempty_list(COMMA, IDENT) {$1}
;

match_case_list:
|VBAR separated_nonempty_list(VBAR, match_case) {$2}
|separated_nonempty_list(VBAR, match_case) {$1}
;

match_case:
|IDENT ARROW decl {($1, $3)}
;

automaton_case_list:
|VBAR separated_nonempty_list(VBAR, automaton_case) {$2}
|separated_nonempty_list(VBAR, automaton_case) {$1}
;

automaton_case:
|IDENT ARROW shared_var strong_condition {($1, $3, $4)}
;

shared_var:
|LET decl IN shared_var {PSV_let($2, $4)}
|DO decl weak_condition {PSV_do($2, $3)}
;

weak_condition:
|/*empty*/ {PWC_epsilon}
|UNTIL expr THEN IDENT weak_condition {PWC_until_then($2, $4, $5)}
|UNTIL expr CONTINUE IDENT weak_condition {PWC_until_cont($2, $4, $5)}
;

strong_condition:
|/*empty*/ {PSC_epsilon}
|UNLESS expr THEN IDENT strong_condition {PSC_unless_then($2, $4, $5)}
|UNLESS expr CONTINUE IDENT strong_condition {PSC_unless_cont($2, $4, $5)}
;

expr:
|LPAREN expr RPAREN {$2}
|const {$1}
|IDENT {mk_expr (PE_ident $1)}
|IDENT LPAREN expr_comma_list RPAREN {mk_expr (PE_app ($1, $3))}
|IF expr THEN expr ELSE expr {mk_expr (PE_if ($2, $4, $6))}
|expr PLUS expr {mk_expr (PE_op (Op_add, [$1; $3]))}
|expr MINUS expr {mk_expr (PE_op (Op_sub, [$1; $3]))}
|expr STAR expr {mk_expr (PE_op (Op_mul, [$1; $3]))}
|expr SLASH expr {mk_expr (PE_op (Op_div, [$1; $3]))}
|expr DIV expr {mk_expr (PE_op (Op_div, [$1; $3]))}
|expr MOD expr {mk_expr (PE_op (Op_mod, [$1; $3]))}
|expr COMP expr {mk_expr (PE_op ($2, [$1; $3]))}
|expr EQUAL expr {mk_expr (PE_op (Op_eq, [$1; $3]))}
|expr NEQ expr {mk_expr (PE_op (Op_neq, [$1; $3]))}
|expr AND expr {mk_expr (PE_op (Op_and, [$1; $3]))}
|expr OR expr {mk_expr (PE_op (Op_or, [$1; $3]))}
|expr IMPL expr {mk_expr (PE_op (Op_impl, [$1; $3]))}
|expr ARROW expr {mk_expr (PE_arrow ($1, $3))}
|expr FBY expr {mk_expr (PE_arrow ($1, mk_expr (PE_pre ($3))))}
|MINUS expr /* %prec uminus */ {mk_expr (PE_op (Op_sub, [$2]))}
|NOT expr {mk_expr (PE_op (Op_not, [$2]))}
|LPAREN expr_comma_list RPAREN {mk_expr (PE_tuple $2)}
|expr WHEN IDENT LPAREN expr RPAREN {mk_expr (PE_when ($1, $3, $5))}
|LAST IDENT {mk_expr (PE_last($2))}
|LET decl IN expr {mk_expr (PE_let_in($2, $4))}
|MERGE expr merge_case_list {mk_expr (PE_merge ($2, $3))}
;

merge_case_list:
|VBAR separated_nonempty_list(VBAR, merge_case) {$2}
|separated_nonempty_list(VBAR, merge_case) {$1}
;

merge_case:
|id = IDENT; ARROW; e = expr {(id, e)}
;

const:
|CONST_INT {mk_expr (PE_const (Cint $1))}
|CONST_REAL {mk_expr (PE_const (Creal $1))}
;

expr_comma_list:
|separated_list(COMMA, expr) {$1}
;
