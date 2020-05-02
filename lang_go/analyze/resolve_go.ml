(* Yoann Padioleau
 *
 * Copyright (C) 2020 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Ast_go
module Ast = Ast_go
module V = Visitor_go
module G = Ast_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Identifiers tagger (so we can colorize them differently in codemap/efuns).
 *
 *)

(*****************************************************************************)
(* Type *)
(*****************************************************************************)
type context = 
  | AtToplevel
  | InFunction (* or Method *)

type resolved_name = Ast_generic.resolved_name

type env = {
  ctx: context ref;
  names: (string * resolved_name) list ref;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* because we use a Visitor instead of a clean recursive 
 * function passing down an environment, we need to emulate a scoped
 * environment by using save_excursion.
 *)
let with_added_env xs env f = 
  let newnames = xs @ !(env.names) in
  Common.save_excursion env.names newnames f

let add_name_env name kind env =
  env.names := (Ast.str_of_id name, kind)::!(env.names)

let with_new_context ctx env f = 
  Common.save_excursion env.ctx ctx f

let default_env () = {
  ctx = ref AtToplevel;
  names = ref [];
}

let params_of_parameters xs = 
  xs |> Common.map_filter (function
     | ParamClassic { pname = Some id; _ } ->
        Some (Ast.str_of_id id, (G.Param, G.sid_TODO))
     | _ -> None
    )
let local_or_global env id =
  if !(env.ctx) = AtToplevel 
  then G.Global [id], G.sid_TODO
  else G.Local, G.sid_TODO

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let resolve prog =
  let env = default_env () in

  (* would be better to use a classic recursive with environment visit *)
  let visitor = V.mk_visitor { V.default_visitor with
    (* No need to resolve at the definition sites (for parameters, locals).
     * This will be pattern-matched specially anyway in the highlighter. What
     * you want is to tag the use sites, and to maintain the right environment.
     *)

    (* defs *)
    V.kprogram = (fun (k, _) x ->

      let file = Parse_info.file_of_info (fst x.package), fst x.package in
      let packid = snd x.package in
      add_name_env packid (G.ImportedModule (G.FileName file), G.sid_TODO) env;

      x.imports |> List.iter (fun { i_path = (path, ii); i_kind = kind; _ } ->
          match kind with
          | ImportOrig -> 
            add_name_env (Filename.basename path, ii) 
              (G.ImportedModule (G.FileName (path,ii)), G.sid_TODO) env
          | ImportNamed id -> 
            (* TODO: hacky, but right now we transform
             * import sub "x.y.z" in sub = [z] so later 
             * sub.bar will be allowed to match z.bar
             *)
            (* add_name_env id
              (G.ImportedModule (G.FileName (path,ii))) env *)
             add_name_env id
               (G.ImportedModule (G.DottedName [(Filename.basename path, ii)]),
                    G.sid_TODO)
                  env;
          | ImportDot _ -> ()
      );
      k x
    );
    V.ktop_decl = (fun (k, _) x ->
      (match x with 
      | DFunc (id, _) ->
         env |> add_name_env id (G.Global [id], G.sid_TODO); (* could add package name?*)
         with_new_context InFunction env (fun () ->
           k x
         )
      | DMethod (id, receiver, _) ->
         env |> add_name_env id (G.Global [id], G.sid_TODO); (* could add package name?*)
         let new_names = params_of_parameters [ParamClassic receiver] in
         with_added_env new_names env (fun () ->
          with_new_context InFunction env (fun () ->
           k x
          ))
      | D _ -> k x
      )
    );
    V.kdecl = (fun (k, _) x -> 
      (match x with
      | DConst (id, _, _) | DVar (id, _, _) ->
         env |> add_name_env id (local_or_global env id)
        (* we do care about types because sometimes we don't know an Id
         * is actually a type, e.g., when passed to make()
         * less: could hardcode recognizing make()? or other cases where
         * you can pass a type as an argument in Go?
         *)
      | DTypeAlias (id, _, _)  | DTypeDef (id, _) ->
         env |> add_name_env id (G.TypeName, G.sid_TODO)
      );
      k x
    );
    V.kstmt = (fun (k, _) x ->
      (match x with
      | SimpleStmt (DShortVars (xs, _, _)) | Range (_, Some (xs, _),_, _, _) ->
         xs |> List.iter (function
           | Id (id, _) -> env |> add_name_env id (local_or_global env id)
           | _ -> ()
         )

       (* general case *)
       | _ -> ()
      );
      k x
    );
    V.kfunction = (fun (k, _) x ->
     let (ft, _) = x in
     let new_params = params_of_parameters ft.fparams in
      with_added_env new_params env (fun () ->
       k x
     )
    );

    (* uses *)
    V.kexpr = (fun (k, _) x ->
      (match x with
      | Id (id, resolved) ->
        let s = Ast.str_of_id id in
        (match List.assoc_opt s !(env.names) with
          | Some x -> resolved := Some x
          | None -> () (* will be tagged as Error by highlighter later *)
        )
      | _ -> ()
      );
      k x
    );

  } in
  visitor (P prog)
