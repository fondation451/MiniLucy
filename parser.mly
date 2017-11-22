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

  open Ast_type;;
  open Ast;;
  open Ast_lustre;;
  open Parsing;;

  let loc () = symbol_start_pos (), symbol_end_pos ();;
  let mk_expr e = {pexpr_lustre_desc = e; pexpr_lustre_clk = CK_free; pexpr_lustre_loc = loc ()};;
  let mk_patt p = {ppatt_desc = p; ppatt_loc = loc ()};;
  let mk_param id ty ck = {param_id = id; param_ty = ty; param_ck = ck;};;

%}

%token AND
%token ARROW
%token BOOL
%token CONST
%token COLON
%token COMMA
%token <Ast_type.op> COMP
%token <int> CONST_INT
%token <float> CONST_REAL
%token CURRENT
%token DIV
%token ELSE
%token END
%token EOF
%token EQUAL
%token EVERY
%token FBY
%token NEQ
%token REAL
%token <string> IDENT
%token IF
%token IMPL
%token INT
%token LET
%token LPAREN
%token MINUS
%token MERGE
%token MOD
%token NODE
%token NOT
%token OR
%token PLUS
%token PRE
%token RESET
%token RETURNS
%token RPAREN
%token SEMICOL
%token SLASH
%token STAR
%token TEL
%token THEN
%token TYPE
%token VAR
%token WHEN


%nonassoc THEN
%nonassoc ELSE
%right ARROW
%left IMPL
%left OR
%left AND
%left COMP EQUAL NEQ                          /* < <= > >= <> = <> */
%left PLUS MINUS                              /* + -  */
%left STAR SLASH DIV MOD                      /* * /  mod */
%nonassoc uminus                              /* - */
%nonassoc NOT PRE                             /* not pre */
%left DOT

/* Point d'entre */

%start file
%type <Ast_lustre.p_file_lustre> file

%%

file: type_decs const_decs node_decs EOF {($1, $2, $3)}
;

type_decs:
  |/* empty */ {IdentMap.add "bool" ["False" ; "True"] IdentMap.empty}
  |type_dec type_decs {let ty, ty_val = $1 in IdentMap.add ty ty_val $2}
;

type_dec:
  |TYPE IDENT SEMICOL {($2, [])}
  |TYPE; ty = IDENT; EQUAL; enum_val = separated_list(PLUS, IDENT); SEMICOL {(ty, enum_val)}
;

const_decs:
|/* empty */ {[]}
|const_dec const_decs {$1 :: $2}
;

node_decs:
|/* empty */    {[]}
|node node_decs {$1 :: $2}
;

const_dec:
|CONST IDENT EQUAL const_val SEMICOL {($2, $4)}
;

const_val:
|CONST_INT {Cint $1}
|CONST_REAL {Creal $1}
|MINUS; CONST_INT {Cint (-1 * $2)}
|MINUS; CONST_REAL {Creal (-1.0 *. $2)}
;


node:
|NODE IDENT LPAREN in_params RPAREN
 RETURNS LPAREN out_params RPAREN SEMICOL
 local_params
 LET eq_list TEL semi_opt
  {{pn_lustre_name = $2;
    pn_lustre_input = $4;
    pn_lustre_output = $8;
    pn_lustre_local = $11;
    pn_lustre_equs = $13;
    pn_lustre_loc = loc();}}
;

in_params:
|/* empty */ {[]}
|param_list {$1}
;


out_params:
|param_list {$1}
;

local_params:
|/* empty */ {[]}
|VAR param_list_semicol {$2}
;

param_list:
|param {$1}
|param SEMICOL param_list {$1 @ $3}
;

param_list_semicol:
|param  SEMICOL {$1}
|param SEMICOL param_list_semicol {$1 @ $3}
;


param:
  |param = ident_comma_list; COLON; ty = typ; WHEN; id1 = IDENT; LPAREN; id2 = IDENT; RPAREN
    {List.map (fun name -> mk_param name ty (CK_on(CK_base, id1, id2))) param}
  |param = ident_comma_list; COLON; ty = typ
    {List.map (fun name -> mk_param name ty CK_base) param}
;

clock_expr:
  |IDENT {CK_on(CK_base, true, $1)}
  |NOT IDENT {CK_on(CK_base, false, $2)}
;

eq_list:
|eq {[$1]}
|eq eq_list {$1 :: $2}
;

eq:
|pattern EQUAL expr SEMICOL {{peq_lustre_patt = $1; peq_lustre_expr = $3;}}
;

pattern:
|IDENT {mk_patt (PP_ident $1)}
|LPAREN IDENT COMMA ident_comma_list RPAREN {mk_patt (PP_tuple($2::$4))}
;

expr:
|LPAREN expr RPAREN {$2}
|const {$1}
|IDENT {mk_expr (PEL_ident $1)}
|f = IDENT; LPAREN; arg = expr_comma_list_empty; RPAREN; EVERY; reset = IDENT; {mk_expr (PEL_app (f, arg, reset))}
|f = IDENT; LPAREN; arg = expr_comma_list_empty; RPAREN; {mk_expr (PEL_app (f, arg, lustre_bool_false))}
|IF expr THEN expr ELSE expr {mk_expr (PEL_if ($2, $4, $6))}
|expr PLUS expr {mk_expr (PEL_binop (Op_add, $1, $3))}
|expr MINUS expr {mk_expr (PEL_binop (Op_sub, $1, $3))}
|expr STAR expr {mk_expr (PEL_binop (Op_mul, $1, $3))}
|expr SLASH expr {mk_expr (PEL_binop (Op_div, $1, $3))}
|expr DIV expr {mk_expr (PEL_binop (Op_div, $1, $3))}
|expr MOD expr {mk_expr (PEL_binop (Op_mod, $1, $3))}
|expr COMP expr {mk_expr (PEL_binop ($2, $1, $3))}
|expr EQUAL expr {mk_expr (PEL_binop (Op_eq, $1, $3))}
|expr NEQ expr {mk_expr (PEL_binop (Op_neq, $1, $3))}
|expr AND expr {mk_expr (PEL_binop (Op_and, $1, $3))}
|expr OR expr {mk_expr (PEL_binop (Op_or, $1, $3))}
|expr IMPL expr {mk_expr (PEL_binop (Op_impl, $1, $3))}
|expr ARROW expr {mk_expr (PEL_if (mk_expr (PEL_fby(Cenum("True"), mk_expr (PEL_ident("False")))), $1, $3))}
|const_fby FBY expr {mk_expr (PEL_fby ($1, $3))}
|MINUS expr /* %prec uminus */ {mk_expr (PEL_op (UOp_minus, $2))}
|NOT expr {mk_expr (PEL_op (UOp_not, $2))}
|LPAREN expr COMMA expr_comma_list RPAREN {mk_expr (PEL_tuple ($2::$4))}
|e1 = expr; WHEN; id = IDENT; LPAREN; e2 = expr; RPAREN {mk_expr (PEL_when (e1, id, e2))}
|CURRENT expr {mk_expr (PEL_current ($2))}
|MERGE; id = IDENT; e_l = list(merge_clause) {mk_expr (PEL_merge (id, e_l))}
;

merge_clause:
  |LPAREN; id = IDENT; ARROW; e = expr; RPAREN {(id, e)}
;

const:
|CONST_INT {mk_expr (PEL_const (Cint $1))}
|CONST_REAL {mk_expr (PEL_const (Creal $1))}
;

const_fby:
|CONST_INT {Cint $1}
|CONST_REAL {Creal $1}
|IDENT {Cenum $1}
;

ident_comma_list:
|IDENT COMMA ident_comma_list {$1 :: $3}
|IDENT {[$1]}
;

expr_comma_list_empty:
{[]}
|expr_comma_list {$1}
;

expr_comma_list:
|expr COMMA expr_comma_list {$1 :: $3}
|expr {[$1]}
;

typ:
  |INT {Tint}
  |REAL {Treal}
  |IDENT {Ttype($1)}
;

semi_opt:
{()}
|SEMICOL {()}
;
