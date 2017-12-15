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

open Printf;;

exception Impl_Op;;
exception No_Main;;

let (<<<) = Buffer.add_string;;

let str_dumb_c ty ty_map =
  match ty with
  |Tint -> "0"
  |Treal -> "0.0"
  |Ttype(tid) -> List.hd (IdentMap.find tid ty_map)
  |Ttuple(_) -> assert false
;;

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

let str_id id = String.lowercase_ascii id;;

let str_uop uop =
  match uop with
  |UOp_not -> "my_not"
  |UOp_minus -> "-"
  |UOp_minus_f -> "-."
;;

let str_op op =
  match op with
  |Op_eq -> "<=>" |Op_neq -> "<<>>" |Op_lt -> "<<>" |Op_le -> "<<=>" |Op_gt -> "<>>" |Op_ge -> "<>=>"
  |Op_add -> "+" |Op_sub -> "-" |Op_mul -> "*" |Op_div -> "/" |Op_mod -> "mod"
  |Op_add_f -> "+." |Op_sub_f -> "-." |Op_mul_f -> "*." |Op_div_f -> "/."
  |Op_and -> "&&&" |Op_or -> "|||" |Op_impl -> raise Impl_Op
;;

let rec expr_to_caml e =
  match e with
  |OBJ_const(c) -> str_c c
  |OBJ_ident(id) -> sprintf "!%s" (str_id id)
  |OBJ_state(id) -> sprintf "!%s" (str_id id)
  |OBJ_op(uop, e1) -> sprintf "%s(%s)" (str_uop uop) (expr_to_caml e1)
  |OBJ_binop(op, e1, e2) ->
    try
      sprintf "(%s %s %s)" (expr_to_caml e1) (str_op op) (expr_to_caml e2)
    with Impl_Op ->
      sprintf "(not %s || %s)" (expr_to_caml e1) (expr_to_caml e2)
;;

let str_tuple l =
  let rec loop l out =
    match l with
    |[] -> out ^ ")"
    |[x] -> loop [] (out ^ (str_id x))
    |h::t -> loop t (out ^ (sprintf "%s, " (str_id h)))
  in loop l "("
;;

let rec instr_to_caml instance i =
  let n = ref 0 in
  let rec loop i =
    match i with
    |IOBJ_var_affect(id, e) ->
      n := !n + 1;
      sprintf "let ____th____%d = Thread.create (fun () -> %s := %s) () in\n" !n (str_id id) (expr_to_caml e)
    |IOBJ_state_affect(id, e) ->
      n := !n + 1;
      sprintf "let ____th____%d = Thread.create (fun () -> mem.%s <- %s) () in\n" !n (str_id id) (expr_to_caml e)
    |IOBJ_skip -> ""
    |IOBJ_reset(id) ->
      n := !n + 1;
      sprintf "let ____th____%d = Thread.create (fun () -> mem.%s <- %s_reset ()) () in\n" !n id (List.assoc id instance)
    |IOBJ_step(id_l, id, e_l) ->
      n := !n + 1;
      let node_name = List.assoc id instance in
      let tmp_arg = Array.to_list (Array.init (List.length id_l) (fun i -> sprintf "arg%d" i)) in
      let out = sprintf "let ____th____%d = Thread.create (fun () -> " !n in
      let out = out ^ (sprintf "let %s = %s_step mem.%s " (str_tuple tmp_arg) node_name (str_id id)) in
      let out = ref (out ^ (List.fold_left (fun tmp e -> tmp ^ (expr_to_caml e) ^ " ") "" e_l) ^ "in\n") in
      List.iteri (fun i id_v -> out := !out ^ (sprintf "%s := arg%d;\n" id_v i)) id_l;
      !out ^ ") () in\n"
    |IOBJ_case(id, case_l) ->
      n := !n + 1;
      let out = ref (sprintf "let ____th____%d = Thread.create (fun () -> (match !%s with\n" !n (str_id id)) in
      List.iter (fun (id_case, i1) -> out := !out ^ (sprintf "|%s -> %s" id_case (instr_to_caml instance i1))) case_l;
      !out ^ "\n|_ -> ())) () in\n"
    |IOBJ_sequence(i1, i2) -> sprintf "%s%s\n" (loop i1) (loop i2)
    |IOBJ_concurrent -> ""
  in
  let rec write_join n out =
    if n = 0 then
      out
    else
      write_join (n-1) (out ^ (sprintf "Thread.join ____th____%d;\n" n))
  in write_join !n (loop i)
;;

let cut_conc step_i =
  let rec unwrap step_i =
    match step_i with
    |IOBJ_sequence(i1, i2) -> List.append (unwrap i1) (unwrap i2)
    |_ -> [step_i]
  in
  let rec cut l tmp out =
    match l with
    |[] -> if tmp = IOBJ_skip then List.rev out else List.rev (tmp::out)
    |h::t ->
      if h = IOBJ_concurrent then
        cut t IOBJ_skip (tmp::out)
      else
        cut t (IOBJ_sequence(tmp, h)) out
  in
  cut (unwrap step_i) IOBJ_skip []
;;

let def_to_caml def ty_map code =
  let name, memory, instance, reset, step = def in
  let var_in, var_out, var_loc, step_i = step in
  (* memory *)
  code <<< (sprintf "type %s_mem = {\n____dummy____ : int;\n" name);
  List.iter (fun (id, (ty, _)) -> code <<< (sprintf "mutable %s : %s;\n" (str_id id) (str_ty ty))) memory;
  List.iter (fun (id, f_name) -> code <<< (sprintf "mutable %s : %s_mem;\n" (str_id id) f_name)) instance;
  code <<< "};;\n\n";

  (* reset *)
  code <<< (sprintf "let %s_reset () = {\n____dummy____ = 0;\n" name);
  List.iter (fun (id, (_, c)) -> code <<< (sprintf "%s = %s;\n" (str_id id) (str_c c))) memory;
  List.iter (fun (id, f_name) -> code <<< (sprintf "%s = %s_reset ();\n" (str_id id) f_name)) instance;
  code <<< "};;\n\n";

  (* step *)
  code <<< (sprintf "let %s_step mem " name);
  List.iter (fun (id, ty) -> code <<< (sprintf "%s " (str_id id))) var_in;
  code <<< "=\n";
  List.iter (fun (id, ty) -> code <<< (sprintf "let %s = ref %s in\n" (str_id id) (str_id id))) var_in;
  List.iter (fun (id, ty) -> code <<< (sprintf "let %s = ref %s in\n" (str_id id) (str_dumb_c ty ty_map))) var_loc;
  List.iter (fun (id, _) -> code <<< (sprintf "let %s = ref mem.%s in\n" (str_id id) (str_id id))) memory;

  let step_i_conc = cut_conc step_i in
  List.iter (fun step_i -> code <<< (instr_to_caml instance step_i)) step_i_conc;

  code <<< "(";
  let rec loop var_out =
    match var_out with
    |[] -> code <<< ")\n"
    |[(id, ty)] -> code <<< (sprintf "!%s" (str_id id)); loop []
    |(id, ty)::t -> code <<< (sprintf "!%s, " (str_id id)); loop t
  in loop var_out;
  code <<< ";;\n\n"
;;

let rec find_step_main def_l =
  match def_l with
  |[] -> raise No_Main
  |(id, _, _, _, step)::t -> if id = "main" then step else find_step_main t
;;

let mk_main step_main ty_map =
  let vinput, voutput, vvar, _ = step_main in
  "let _ =\nlet ____buffer____ = ref (____read_file____ Sys.argv.(1)) in\nlet mem = main_reset () in\n" ^
  (List.fold_left (fun out (id, ty) -> out ^ (sprintf "let %s = ref %s in\n" (str_id id) (str_dumb_c ty ty_map))) "" vinput) ^
  "let rec loop () = try\n" ^
  (List.fold_left
    (fun out (id, ty) ->
      out ^
      (match ty with
      |Tint -> sprintf "%s := int_of_string (List.hd !____buffer____);\n" (str_id id)
      |Treal -> sprintf "%s := float_of_string (List.hd !____buffer____);\n" (str_id id)
      |Ttype(tid) -> sprintf "%s := scan_%s (List.hd !____buffer____);\n" (str_id id) tid
      |Ttuple(_) -> assert false) ^
      "____buffer____ := List.tl !____buffer____;\n")
    "" vinput) ^
  (sprintf "let %s = main_step mem " (str_tuple (List.map fst voutput))) ^
  (List.fold_left (fun out (id, _) -> out ^ (sprintf "!%s " (str_id id))) "" vinput) ^
  "in\n" ^
  (List.fold_left
    (fun out (id, ty) ->
      out ^
      (match ty with
      |Tint -> sprintf "print_int %s; print_newline ();\n" (str_id id)
      |Treal -> sprintf "print_float %s; print_newline ();\n" (str_id id)
      |Ttype(tid) -> sprintf "print_%s %s; print_newline ();\n" tid (str_id id)
      |_ -> assert false))
    "" voutput) ^
  "loop ()\n" ^
  "with Failure _ -> ()\nin loop ()\n;;"
;;

let read_file f =
  let fd = open_in f in
  let rec loop out =
    try
      loop (out ^ (input_line fd) ^ "\n")
    with End_of_file -> close_in fd; out
  in loop ""
;;

let file_to_caml f =
  let ty_map, def_l = f in
  let code = Buffer.create 100 in
  code <<< "(* EN TETE *)\n\nopen Printf;;\n\n";
  (* ENUM TYPE *)
  IdentMap.iter
    (fun ty enum_l ->
      code <<< (sprintf "type %s =\n" ty);
      List.iter (fun enum -> code <<< (sprintf "|%s\n" enum)) enum_l;
      code <<< ";;\n\n")
    ty_map;
  (* ENUM TYPE PRINTING *)
  IdentMap.iter
    (fun ty enum_l ->
      code <<< (sprintf "let print_%s x = match x with\n" ty);
      List.iter (fun enum -> code <<< (sprintf "|%s -> print_string \"%s\"\n" enum enum)) enum_l;
      code <<< ";;\n\n")
    ty_map;
  (* ENUM TYPE SCANNING *)
  IdentMap.iter
    (fun ty enum_l ->
      code <<< (sprintf "let scan_%s x = match x with\n" ty);
      List.iter (fun enum -> code <<< (sprintf "|\"%s\" -> %s\n" enum enum)) enum_l;
      code <<< "|_ -> assert false\n;;\n\n")
    ty_map;
  code <<< (sprintf "%s\n\n" (read_file "header.ml"));
  List.iter (fun def -> def_to_caml def ty_map code) def_l;
  code <<< mk_main (find_step_main def_l) ty_map;
  code
;;
