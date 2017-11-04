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

type location = Lexing.position * Lexing.position;;

type ident = string;;

let ident_from_string str = str;;

let gen_new_id =
  let cpt = ref 0 in fun () -> incr cpt; "__aux"^(string_of_int !cpt)
;;

module IdentMap = Map.Make(struct
  type t = ident;;
  let compare = String.compare;;
end);;

module IdentSet = Set.Make(struct
  type t = ident;;
  let compare = String.compare;;
end);;

type lustre_ty =
  |Tint
  |Treal
  |Ttype of ident
  |Ttuple of lustre_ty list
;;

let lustre_bool_type = Ttype "bool";;
let lustre_bool_true = "True";;
let lustre_bool_false = "False";;

type enum_ty = string list;;

type ty = lustre_ty list;;

type const =
  |Cint of int
  |Creal of float
  |Cenum of ident
;;

type clock_t =
  |CK_base
  |CK_free
  |CK_on of clock_t * ident * ident
  |CK_tuple of clock_t list
;;

type uop =
  |UOp_not
  |UOp_minus
;;

type op =
  |Op_eq |Op_neq |Op_lt |Op_le |Op_gt |Op_ge
  |Op_add |Op_sub |Op_mul |Op_div |Op_mod
  |Op_and |Op_or |Op_impl
;;
