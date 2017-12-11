open Ast_type;;
open Ast;;
open East;;

let is_ttype t =
  match t with
  |Ttype _ -> true
  | _      -> false
;;

let make_type_env param =
  let rec add_in_env env param_l =
    match param_l with
    |[]   -> env
    |p::q -> add_in_env (IdentMap.add p.param_id p.param_ty env)
                        q in
  add_in_env IdentMap.empty param
;;

let find_in_env global local id =
  try
    IdentMap.find id local
  with
  | Not_found -> IdentMap.find id global
;;

let type_decl tdef tenv tenv_loc decl =
  42;;

let type_node tdef tenv node =
  let tenv_local = make_type_env (node.pn_input @ node.pn_output @ node.pn_local)
  in type_decl tdef tenv tenv_local node.pn_decl
;;

let rec type_nodes tenv ln =
  match ln with
  |[]   -> []
  |p::q -> (type_node tenv p) :: (type_nodes tenv q)
;;

let mk_type_def type_decls =
  let rec aux env t_decls =
    match t_decls with
    |[]   -> env
    |p::q -> let (id,const) = p in
      aux (IdentMap.add id const env) q in

  aux IdentMap.empty type_decls
;;

let type_file f =
  let (type_decls, nodes) = f in
    let t_def = mk_type_def type_decls in
    let tenv  = 2 in
    type_nodes t_def nodes
;;
