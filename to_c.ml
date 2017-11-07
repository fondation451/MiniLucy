(*
########
Copyright © 2017

This file is part of MiniLucy.
MiniLucy is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License v3 as published by
the Free Software Foundation.

Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########
*)

open Ast_type;;
open Ast;;
open Ast_object;;

exception Impl_Op;;

let (<<<) = Buffer.add_string;;

let str_c c =
  match c with
  |Cint(i) -> string_of_int i
  |Creal(r) -> string_of_float r
  |Cenum(id) -> id
;;

let str_ty ty =
  match ty with
  |Tint -> "int"
  |Treal -> "double"
  |Ttype(id) -> id
  |Ttuple(id_l) -> assert false
;;

let str_uop uop =
  match uop with
  |UOp_not -> "!"
  |UOp_minus -> "-"
;;

let str_op op =
  match op with
  |Op_eq -> "==" |Op_neq -> "!=" |Op_lt -> "<" |Op_le -> "<=" |Op_gt -> ">" |Op_ge -> ">="
  |Op_add -> "+" |Op_sub -> "-" |Op_mul -> "*" |Op_div -> "/" |Op_mod -> "%"
  |Op_and -> "&&" |Op_or -> "||" |Op_impl -> raise Impl_Op
;;


let rec expr_to_c e code =
  match e with
  |OBJ_const(c) -> code <<< (str_c c)
  |OBJ_ident(id) -> code <<< id
  |OBJ_state(id) ->
    code <<< "self->";
    code <<< id
  |OBJ_op(uop, e1) ->
    code <<< (str_uop uop);
    code <<< "(";
    expr_to_c e1 code;
    code <<< ")"
  |OBJ_binop(op, e1, e2) -> begin
    try
      let op_s = str_op op in
      code <<< "(";
      expr_to_c e1 code;
      code <<< op_s;
      expr_to_c e2 code;
      code <<< ")"
    with Impl_Op ->
      code <<< "(!(";
      expr_to_c e1 code;
      code <<< ") || ";
      expr_to_c e2 code;
      code <<< ")"
  end
;;

let rec instr_to_c instance i code =
  match i with
  |IOBJ_var_affect(id, e) ->
    code <<< id;
    code <<< " = ";
    expr_to_c e code;
    code <<< ";\n"
  |IOBJ_state_affect(id, e) ->
    code <<< "self->";
    code <<< id;
    code <<< " = ";
    expr_to_c e code;
    code <<< ";\n"
  |IOBJ_skip -> ()
  |IOBJ_reset(id) ->
    let node_name = List.assoc id instance in
    code <<< node_name;
    code <<< "_reset(inst->";
    code <<< id;
    code <<< ")"
  |IOBJ_step(id_l, id, e_l) ->
    let node_name = List.assoc id instance in
    let acc = gen_new_id () in
    code <<< acc;
    code <<< " = ";
    code <<< node_name;
    code <<< "_step(";
    let rec loop e_l =
      match e_l with
      |[] -> code <<< ";\n" |[h] -> expr_to_c h code; loop []
      |h::t -> expr_to_c h code; code <<< ", "; loop t
    in loop e_l;
    List.iteri
      (fun i pid ->
        code <<< pid;
        code <<< " = ";
        code <<< acc;
        code <<< "->arg";
        code <<< (string_of_int i);
        code <<< ";\n")
      id_l
  |IOBJ_case(id, case_l) ->
    List.iter
      (fun (id_case, i1) ->
        code <<< "if(";
        code <<< id;
        code <<< " == ";
        code <<< id_case;
        code <<< ") {";
        instr_to_c instance i1 code;
        code <<< "}\n")
      case_l
  |IOBJ_sequence(i1, i2) ->
    instr_to_c instance i1 code;
    code <<< "\n";
    instr_to_c instance i2 code
;;

let def_to_c def code =
  let name, memory, instance, reset, step = def in
  let var_in, var_out, var_loc, step_i = step in
  (* memory *)
  code <<< "typedef struct {\nchar ____dummy___;\n";
  List.iter
    (fun (id, ty) ->
      code <<< (str_ty ty);
      code <<< " ";
      code <<< id;
      code <<< ";\n")
    memory;
    List.iter
      (fun (id, f_name) ->
        code <<< f_name;
        code <<< "_mem* ";
        code <<< id;
        code <<< ";\n")
      instance;
  code <<< "} ";
  code <<< name;
  code <<< "_mem;\n\n";

  (* step_return *)
  code <<< "typedef struct {";
  List.iteri
    (fun i (id, ty) ->
      code <<< (str_ty ty);
      code <<< " arg";
      code <<< (string_of_int i);
      code <<< "; ")
    var_out;
  code <<< "} ";
  code <<< name;
  code <<< "_out;\n\n";

  (* reset *)
  if reset <> [] then begin
    code <<< "void ";
    code <<< name;
    code <<< "_reset(";
    code <<< name;
    code <<< "_mem* self) {\n";
    List.iter (fun i -> instr_to_c instance i code) reset;
    code <<< "}\n\n"
  end;

  (* step *)
  code <<< name;
  code <<< "_out ";
  code <<< name;
  code <<< "_step(";
  List.iter
    (fun (id, ty) ->
      code <<< (str_ty ty);
      code <<< " ";
      code <<< id;
      code <<< ", ")
    var_in;
  code <<< name;
  code <<< "_mem* self) {\n";
  List.iter
    (fun (id, ty) ->
      code <<< (str_ty ty);
      code <<< " ";
      code <<< id;
      code <<< "; ")
    var_loc;
  code <<< "\n";
  List.iter
    (fun (id, ty) ->
      code <<< (str_ty ty);
      code <<< " ";
      code <<< id;
      code <<< "; ")
    var_out;
  code <<< "\n";
  instr_to_c instance step_i code;
  code <<< name;
  code <<< "_out ____out____ = {";
  let rec loop var_out =
    match var_out with
    |[] -> code <<< "};\n"
    |[(id, ty)] -> code <<< id; loop []
    |(id, ty)::t -> code <<< id; code <<< ", "; loop t
  in loop var_out;
  code <<< "return ____out____;\n";
  code <<< "}\n"
;;

let file_to_c f =
  let ty_map, def_l = f in
  let code = Buffer.create 100 in
  IdentMap.iter
    (fun ty enum_l ->
      code <<< "typedef enum {";
      let rec loop l =
        match l with
        |[] -> code <<< "} "; code <<< ty; code <<< ";\n"
        |[h] -> code <<< h; loop []
        |h::t -> code <<< h; code <<< ", "; loop t
      in loop enum_l)
    ty_map;
  List.iter (fun def -> def_to_c def code) def_l;
  code
;;
