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


type sp_equation =
  |SP_EQ of p_equation
  |SP_SKIP
;;

type sp_node = {
  spn_name: ident;
  spn_input: param list;
  spn_output: param list;
  spn_local: param list;
  spn_equs: sp_equation list;
  spn_loc: location;
};;

type p_file = enum_ty IdentMap.t * sp_node list;;
