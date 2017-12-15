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

  open Ast;;
  open Ast_type;;
  open East;;
  open Parsing;;

  let ty_map = Hashtbl.create 5;;
  let _ =
    List.iter (fun (s,k) -> Hashtbl.add ty_map s k) [
      "int", Tint;
      "real", Treal;
    ];;

  let ty_env = Hashtbl.create 100;;
  let add_ty_env id ty = Hashtbl.add ty_env id ty;;
  let clear_ty_env () = Hashtbl.clear ty_env;;

  let ck_env = Hashtbl.create 100;;
  let add_ck_env id ck = Hashtbl.add ck_env id ck;;
  let clear_ck_env () = Hashtbl.clear ck_env;;

  let const_ty_env = Hashtbl.create 100;;
  let add_ty_const c ty = Hashtbl.add const_ty_env c ty;;
  let _ =
    List.iter (fun (s,k) -> add_ty_const s k) [
      "True", bool_type;
      "False", bool_type;
    ];;

  let loc ()            = symbol_start_pos (), symbol_end_pos ();;
  let mk_ty ty          = try Hashtbl.find ty_map ty with Not_found -> Ttype ty;;
  let mk_ckid id        = if String.equal (String.lowercase_ascii id) "base" then CKbase else CKid id;;
  let mk_id id          = (id, (try Hashtbl.find ty_env id with |Not_found ->
                                try Hashtbl.find const_ty_env id with |Not_found -> none_type),
                               try Hashtbl.find ck_env id with |Not_found -> CKbase);;
  let mk_expr e         = {pexpr_desc = e; pexpr_ty = none_type;
                           pexpr_ck = none_clock; pexpr_loc = loc ()};;
  let mk_decl d         = {pdecl_desc = d; pdecl_loc = loc ()}
  let mk_patt p         = {ppatt_desc = p; ppatt_loc = loc ()};;
  let mk_param id ty ck = {param_id = id; param_ty = ty; param_ck = ck};;
    (* Dummy values for clock and ty, they will be set during typing and clocking*)

%}

%token <Ast_type.op> COMP
%token <int> CONST_INT
%token <float> CONST_REAL
%token <string> IDENT

%token NODE
%token COMMA ARROW IMPL SEMICOL SLASH VBAR COLON
%token IF THEN ELSE LET IN FBY MERGE WHEN LAST UNTIL UNLESS AUTOMATON EVERY
%token CONTINUE DO WITH RESET MATCH WHERE TYPE PRE END TEL ON
%token LPAREN RPAREN
%token CLK
%token EQUAL NEQ PLUS MINUS STAR MOD AND NOT OR
%token EOF

%nonassoc ELSE
%right ARROW
%left IMPL
%left OR
%left AND
%left COMP EQUAL NEQ                          /* < <= > >= <> = <> */
%left PLUS MINUS                              /* + -  */
%left STAR SLASH MOD                      /* * /  mod */
%nonassoc NOT                                 /* not */

/* Point d'entr�e */

%start file
%type <East.p_file> file

%%

file: enum_types node_decs EOF {($1,$2)}
;

enum_types:
|/* empty */                  {IdentMap.add "bool" ["True"; "False"]
                                            IdentMap.empty}
|enum_type SEMICOL enum_types {let (id,cons) = $1 in
                               IdentMap.add id cons $3}
;

enum_type:
|TYPE IDENT {($2, [])}
|TYPE IDENT EQUAL separated_list(PLUS, IDENT) {
  let _ = List.iter (fun id -> add_ty_const id (Ttype $2)) $4 in
   ($2, $4)
  }
;

node_decs:
|/* empty */    {[]}
|node node_decs {let _ = clear_ty_env (); clear_ck_env () in $1 :: $2}
;

node:
|LET NODE IDENT LPAREN in_params RPAREN
 EQUAL LPAREN param_list RPAREN WHERE
 decl TEL
  {{pn_name = $3;
    pn_input = $5;
    pn_output = $9;
    pn_local = [];
    pn_decl = $12;
    pn_loc = loc();}}
|LET NODE IDENT LPAREN in_params RPAREN
 EQUAL LPAREN param_list RPAREN
 WITH local_params WHERE
 decl TEL
  {{pn_name = $3;
    pn_input = $5;
    pn_output = $9;
    pn_local = $12;
    pn_decl = $14;
    pn_loc = loc();}}
;

in_params:
|/* empty */     {[]}
|param_list {$1}
;

local_params:
|/* empty */     {[]}
|param_list {$1}
;

param_list:
|param                  {$1}
|param COMMA param_list {$1 @ $3}
;

param:
|ident_space_list COLON typ {List.map (
    fun id -> let _ = add_ty_env id $3; add_ck_env id CKbase in mk_param id $3 CKbase
    ) $1}
|ident_space_list COLON typ COLON clock {List.map (
    fun id -> let _ = add_ty_env id $3; add_ck_env id $5 in mk_param id $3 $5
    ) $1}
;

typ:
|IDENT   {mk_ty $1}
;

clock:
|IDENT   {mk_ckid $1}
|clock ON IDENT LPAREN clock RPAREN {CKon ($1, $3, $5)}
;

decl:
|/* empty */ {mk_decl PD_skip}
|LPAREN decl RPAREN {$2}
|decl SEMICOL {$1}
|decl SEMICOL decl {mk_decl (PD_and($1, $3))}
|pattern EQUAL expr {mk_decl (PD_eq({peq_patt = $1; peq_expr = $3}))}
|CLK IDENT EQUAL expr {mk_decl (PD_clk(mk_id $2, $4))}
|CLK IDENT COLON typ EQUAL expr {mk_decl (PD_clk(($2, $4, CKbase), $6))}
|CLK IDENT COLON typ COLON clock EQUAL expr {mk_decl (PD_clk(($2, $4, $6), $8))}
|LET decl IN decl {mk_decl (PD_let_in($2, $4))}
|MATCH expr WITH match_case_list {mk_decl (PD_match($2, $4))}
|RESET decl EVERY expr {mk_decl (PD_reset($2, $4))}
|AUTOMATON automaton_case_list END {mk_decl (PD_automaton($2))}
;

pattern:
|IDENT {mk_patt (PP_ident $1)}
;

ident_comma_list:
|separated_nonempty_list(COMMA, IDENT) {$1}
;

ident_space_list:
|IDENT                  {[$1]}
|IDENT ident_space_list {$1 :: $2}
;

match_case_list:
|VBAR separated_nonempty_list(VBAR, match_case) {$2}
|separated_nonempty_list(VBAR, match_case) {$1}
;

match_case:
|IDENT ARROW decl {(mk_id $1, $3)}
;

automaton_case_list:
|VBAR separated_nonempty_list(VBAR, automaton_case) {$2}
|separated_nonempty_list(VBAR, automaton_case) {$1}
;

automaton_case:
|IDENT ARROW shared_var strong_condition {(mk_id $1, $3, $4)}
;

shared_var:
|LET decl IN shared_var {PSV_let($2, $4)}
|DO decl weak_condition {PSV_do($2, $3)}
;

weak_condition:
|/*empty*/ {PWC_epsilon}
|UNTIL expr THEN IDENT weak_condition {PWC_until_then($2, mk_id $4, $5)}
|UNTIL expr CONTINUE IDENT weak_condition {PWC_until_cont($2, mk_id $4, $5)}
;

strong_condition:
|/*empty*/ {PSC_epsilon}
|UNLESS expr THEN IDENT strong_condition {PSC_unless_then($2, mk_id $4, $5)}
|UNLESS expr CONTINUE IDENT strong_condition {PSC_unless_cont($2, mk_id $4, $5)}
;

expr:
|LPAREN expr RPAREN {$2}
|const {$1}
|IDENT {mk_expr (PE_ident (mk_id $1))}
|IDENT LPAREN expr_comma_list RPAREN {mk_expr (PE_app (mk_id $1, $3, mk_expr (PE_const (Cenum "False"))))}
|IDENT LPAREN expr_comma_list RPAREN EVERY expr {mk_expr (PE_app (mk_id $1, $3, $6))}
|IF expr THEN expr ELSE expr {mk_expr (PE_if ($2, $4, $6))}
|expr PLUS expr {mk_expr (PE_bop (Op_add, $1, $3))}
|expr MINUS expr {mk_expr (PE_bop (Op_sub, $1, $3))}
|expr STAR expr {mk_expr (PE_bop (Op_mul, $1, $3))}
|expr SLASH expr {mk_expr (PE_bop (Op_div, $1, $3))}
|expr MOD expr {mk_expr (PE_bop (Op_mod, $1, $3))}
|expr COMP expr {mk_expr (PE_bop ($2, $1, $3))}
|expr EQUAL expr {mk_expr (PE_bop (Op_eq, $1, $3))}
|expr NEQ expr {mk_expr (PE_bop (Op_neq, $1, $3))}
|expr AND expr {mk_expr (PE_bop (Op_and, $1, $3))}
|expr OR expr {mk_expr (PE_bop (Op_or, $1, $3))}
|expr IMPL expr {mk_expr (PE_bop (Op_impl, $1, $3))}
|expr ARROW expr {mk_expr (PE_if (mk_expr (PE_fby (Cenum "True", mk_expr (PE_const (Cenum "False")))), $1, $3))}
|const_fby FBY expr {mk_expr (PE_fby ($1, $3))}
|PRE expr {mk_expr (PE_pre ($2))}
|MINUS expr /* %prec uminus */ {mk_expr (PE_uop (UOp_minus, $2))}
|NOT expr {mk_expr (PE_uop (UOp_not, $2))}
|expr WHEN IDENT LPAREN expr RPAREN {mk_expr (PE_when ($1, $3, $5))}
|LAST IDENT {mk_expr (PE_last(mk_id $2))}
|MERGE expr merge_case_list {mk_expr (PE_merge ($2, $3))}
;

const_fby:
|CONST_INT {Cint $1}
|CONST_REAL {Creal $1}
|IDENT {Cenum $1}
;

merge_case_list:
|VBAR separated_nonempty_list(VBAR, merge_case) {$2}
|separated_nonempty_list(VBAR, merge_case) {$1}
;

merge_case:
|IDENT ARROW expr {(mk_id $1, $3)}
;

const:
|CONST_INT {mk_expr (PE_const (Cint $1))}
|CONST_REAL {mk_expr (PE_const (Creal $1))}
;

expr_comma_list:
|separated_list(COMMA, expr) {$1}
;
