open Common
open Ast_generic

let test_cfg_generic file =
  let ast = Parse_generic.parse_program file in
  ast |> List.iter (fun item ->
   (match item with
   | DefStmt (_ent, FuncDef def) ->
     (try 
       let flow = Controlflow_build.cfg_of_func def in
       Controlflow.display_flow flow;
      with Controlflow_build.Error err ->
         Controlflow_build.report_error err
      )
    | _ -> ()
   )
 )

let test_dfg_generic file =
  let ast = Parse_generic.parse_program file in
  ast |> List.iter (fun item ->
   (match item with
   | DefStmt (_ent, FuncDef def) ->
      let flow = Controlflow_build.cfg_of_func def in
      pr2 "Reaching definitions";
      let mapping = Dataflow_reaching.fixpoint flow in
      Dataflow.display_mapping flow mapping Dataflow.ns_to_str;
      pr2 "Liveness";
      let mapping = Dataflow_liveness.fixpoint flow in
      Dataflow.display_mapping flow mapping (fun () -> "()");

    | _ -> ()
   )
 )

let test_naming_generic file =
  let ast = Parse_generic.parse_program file in
  let lang = Common2.some (Lang.lang_of_filename_opt file) in
  Naming_ast.resolve lang ast;
  let v = Meta_ast.vof_any (Ast_generic.Pr ast) in
  let s = Ocaml.string_of_v v in
  pr2 s


let actions () = [
  "-cfg_generic", " <file>",
  Common.mk_action_1_arg test_cfg_generic;
  "-dfg_generic", " <file>",
  Common.mk_action_1_arg test_dfg_generic;
  "-naming_generic", " <file>",
  Common.mk_action_1_arg test_naming_generic;
]
