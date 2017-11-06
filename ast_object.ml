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

open Ast_type;;

type obj_memory = ident;;

type obj_var = ident * lustre_ty;;

type obj_instance = ident * ident;;

type obj_expr =
  |OBJ_const of const
  |OBJ_ident of ident
  |OBJ_state of ident
  |OBJ_op of uop * obj_expr
  |OBJ_binop of op * obj_expr * obj_expr
;;

type obj_instr =
  |IOBJ_var_affect of ident * obj_expr
  |IOBJ_state_affect of ident * obj_expr
  |IOBJ_skip
  |IOBJ_reset of ident
  |IOBJ_step of ident list * ident * obj_expr list
  |IOBJ_case of ident * (ident * obj_instr) list
  |IOBJ_sequence of obj_instr * obj_instr
;;

type obj_reset = obj_instr list;;

type obj_step = obj_var list * obj_var list * obj_var list * obj_instr;;

type obj_def = ident * obj_var list * obj_instance list * obj_reset * obj_step;;

type obj_file = enum_ty IdentMap.t * obj_def list;;
