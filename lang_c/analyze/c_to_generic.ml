(* Yoann Padioleau
 *
 * Copyright (C) 2019 r2c
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
open Common

module G = Ast_generic

open Cst_cpp
open Ast_c

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Ast_c to Ast_generic.
 *
 * See ast_generic.ml for more information.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let id = fun x -> x
let option = Common.map_opt
let list = List.map
let either f g x = 
  match x with 
  | Left x -> Left (f x) 
  | Right x -> Right (g x)

let string = id

let fake s = Parse_info.fake_info s

let opt_to_ident opt =
  match opt with
  | None -> "FakeNAME", Parse_info.fake_info "FakeNAME"
  | Some n -> n

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let info x = x

let wrap = fun _of_a (v1, v2) ->
  let v1 = _of_a v1 and v2 = info v2 in 
  (v1, v2)

let bracket of_a (t1, x, t2) = (info t1, of_a x, info t2)

let name v = wrap string v


let rec unaryOp (a, tok) =
  match a with
  | GetRef -> (fun e -> G.Ref (tok,e))
  | DeRef -> (fun e -> G.DeRef (tok, e))
  | UnPlus -> (fun e -> 
          G.Call (G.IdSpecial (G.ArithOp G.Plus, tok), [G.Arg e]))
  | UnMinus -> (fun e -> 
          G.Call (G.IdSpecial (G.ArithOp G.Minus, tok), [G.Arg e]))
  | Tilde -> (fun e -> 
          G.Call (G.IdSpecial (G.ArithOp G.BitNot, tok), [G.Arg e]))
  | Not ->  (fun e -> 
          G.Call (G.IdSpecial (G.ArithOp G.Not, tok), [G.Arg e]))
  | GetRefLabel -> (fun e -> G.OtherExpr (G.OE_GetRefLabel, [G.E e]))
and assignOp =
  function 
  | SimpleAssign _tok -> None
  | OpAssign (v1, _tok) -> let v1 = arithOp v1 in Some v1

and fixOp = function | Dec -> G.Decr | Inc -> G.Incr
and binaryOp = function
  | Arith v1 -> let v1 = arithOp v1 in v1
  | Logical v1 -> let v1 = logicalOp v1 in v1
and arithOp =
  function
  | Plus -> G.Plus
  | Minus -> G.Minus
  | Mul -> G.Mult
  | Div -> G.Div
  | Mod -> G.Mod
  | DecLeft -> G.LSL
  | DecRight -> G.LSR
  | And -> G.BitAnd
  | Or -> G.BitOr
  | Xor -> G.BitXor
and logicalOp =
  function
  | Inf -> G.Lt
  | Sup -> G.Gt
  | InfEq -> G.LtE
  | SupEq -> G.GtE
  | Eq -> G.Eq
  | NotEq -> G.NotEq

  | AndLog -> G.And
  | OrLog -> G.Or


let rec type_ =
  function
  | TBase v1 -> let v1 = name v1 in G.TyBuiltin v1
  | TPointer (t, v1) -> let v1 = type_ v1 in G.TyPointer (t, v1)
  | TArray ((v1, v2)) ->
      let v1 = option const_expr v1 and v2 = type_ v2 in
      G.TyArray (v1, v2)
  | TFunction v1 -> let (ret, params) = function_type v1 in 
      G.TyFun (params, ret)
  | TStructName ((v1, v2)) ->
      let v1 = struct_kind v1 and v2 = name v2 in
      G.OtherType (v1, [G.I v2])
  | TEnumName v1 -> let v1 = name v1 in
      G.OtherType (G.OT_EnumName, [G.I v1])
  | TTypeName v1 -> 
      let v1 = name v1 in 
      G.TyName ((v1, G.empty_name_info))

and function_type (v1, v2) =
  let v1 = type_ v1 and v2 = list parameter v2 in 
  v1, v2

and parameter { p_type = p_type; p_name = p_name } =
  let arg1 = type_ p_type in 
  let arg2 = option name p_name in 
  { G.ptype = Some arg1; pname = arg2;
    pattrs = []; pinfo = G.empty_id_info (); pdefault = None }
and struct_kind = function 
  | Struct -> G.OT_StructName
  | Union -> G.OT_UnionName


and expr =
  function
  | Int v1 -> let v1 = wrap string v1 in G.L (G.Int v1)
  | Float v1 -> let v1 = wrap string v1 in G.L (G.Float v1)
  | String v1 -> let v1 = wrap string v1 in G.L (G.String v1)
  | Char v1 -> let v1 = wrap string v1 in G.L (G.Char v1)

  | Id v1 -> let v1 = name v1 in 
             G.Id (v1, G.empty_id_info())
  | Ellipses v1 -> let v1 = info v1 in G.Ellipsis (v1)
  | Call ((v1, v2)) -> let v1 = expr v1 and v2 = list argument v2 in
      G.Call (v1, v2)
  | Assign ((v1, v2, v3)) ->
      let v1 = wrap assignOp v1
      and v2 = expr v2
      and v3 = expr v3
      in
      (match v1 with
      | None, tok -> G.Assign (v2, tok, v3)
      | Some op, tok -> G.AssignOp (v2, (op, tok), v3)
      )
  | ArrayAccess ((v1, v2)) -> let v1 = expr v1 and v2 = expr v2 in
      G.ArrayAccess (v1, v2) 
  | RecordPtAccess ((v1, t, v2)) -> 
      let v1 = expr v1 and t = info t and v2 = name v2 in
      G.DotAccess (G.DeRef (t, v1), t, G.FId v2)
  | Cast ((v1, v2)) -> let v1 = type_ v1 and v2 = expr v2 in
      G.Cast (v1, v2)
  | Postfix ((v1, (v2, v3))) ->
      let v1 = expr v1 and v2 = fixOp v2 in 
      G.Call (G.IdSpecial (G.IncrDecr (v2, G.Postfix), v3), [G.Arg v1]) 
  | Infix ((v1, (v2, v3))) ->
      let v1 = expr v1 and v2 = fixOp v2 in
      G.Call (G.IdSpecial (G.IncrDecr (v2, G.Prefix), v3), [G.Arg v1]) 
  | Unary ((v1, v2)) ->
      let v1 = expr v1 and v2 = unaryOp v2 in 
      v2 v1
  | Binary ((v1, (v2, tok), v3)) ->
      let v1 = expr v1
      and v2 = binaryOp v2
      and v3 = expr v3
      in G.Call (G.IdSpecial (G.ArithOp v2, tok), [G.Arg v1; G.Arg v3])
  | CondExpr ((v1, v2, v3)) ->
      let v1 = expr v1 and v2 = expr v2 and v3 = expr v3 in
      G.Conditional (v1, v2, v3)
  | Sequence ((v1, v2)) -> let v1 = expr v1 and v2 = expr v2 in
      G.Seq [v1;v2]
  | SizeOf v1 -> let v1 = either expr type_ v1 in
      G.Call (G.IdSpecial (G.Sizeof, fake "sizeof"), 
       (match v1 with
       | Left e -> [G.Arg e]
       | Right t -> [G.ArgType t]
       ))
  | ArrayInit v1 ->
      let v1 =
        bracket (list
          (fun (v1, v2) ->
             let v1 = option expr v1 and v2 = expr v2 in
             (match v1 with
             | None -> v2
             | Some e ->
                  G.OtherExpr (G.OE_ArrayInitDesignator, [G.E e; G.E v2])
            )
        ))
          v1
      in G.Container (G.Array, v1)
  | RecordInit v1 ->
      let v1 =
        bracket (list (fun (v1, v2) -> let v1 = name v1 and v2 = expr v2 in 
            G.basic_field v1 (Some v2) None
        ))
          v1
      in G.Record v1
  | GccConstructor ((v1, v2)) -> let v1 = type_ v1 and v2 = expr v2 in
      G.Call (G.IdSpecial (G.New, fake "new"), 
        (G.ArgType v1)::([v2] |> List.map G.expr_to_arg))

and argument v = 
  let v = expr v in
  G.Arg v

and const_expr v = 
  expr v
  
let rec stmt =
  function
  | ExprSt v1 -> let v1 = expr v1 in G.ExprStmt v1
  | Block v1 -> let v1 = list stmt v1 in G.Block v1
  | If ((t, v1, v2, v3)) ->
      let v1 = expr v1 and v2 = stmt v2 and v3 = stmt v3 in
      G.If (t, v1, v2, v3)
  | Switch ((v0, v1, v2)) -> 
      let v0 = info v0 in
      let v1 = expr v1 and v2 = list case v2 in
      G.Switch (v0, Some v1, v2)
  | While ((t, v1, v2)) -> let v1 = expr v1 and v2 = stmt v2 in
      G.While (t, v1, v2)
  | DoWhile ((t, v1, v2)) -> let v1 = stmt v1 and v2 = expr v2 in 
      G.DoWhile (t, v1, v2)
  | For ((t, v1, v2, v3, v4)) ->
      let v1 = option expr v1
      and v2 = option expr v2
      and v3 = option expr v3
      and v4 = stmt v4
      in
      let init = match v1 with None -> [] | Some e -> [G.ForInitExpr e] in
      let header = G.ForClassic (init, v2, v3) in
      G.For (t, header, v4)
  | Return (t, v1) -> let v1 = option expr v1 in G.Return (t, v1)
  | Continue t -> G.Continue (t, G.LNone)
  | Break t -> G.Break (t, G.LNone)
  | Label ((v1, v2)) -> let v1 = name v1 and v2 = stmt v2 in
      G.Label (v1, v2)
  | Goto (t, v1) -> let v1 = name v1 in G.Goto (t, v1)
  | Vars v1 -> let v1 = list var_decl v1 in
      G.stmt1 (v1 |> List.map (fun v -> G.DefStmt v))
  | Asm v1 -> let v1 = list expr v1 in 
      G.OtherStmt (G.OS_Asm, v1 |> List.map (fun e -> G.E e))

and case =
  function
  | Case ((t, v1, v2)) -> let v1 = expr v1 and v2 = list stmt v2 in 
      [G.Case (t, G.expr_to_pattern v1)], G.stmt1 v2
  | Default (t, v1) -> let v1 = list stmt v1 in 
      [G.Default t], G.stmt1 v1
and
  var_decl {
               v_name = xname;
               v_type = xtype;
               v_storage = xstorage;
               v_init = init
             } =
  let v1 = name xname in
  let v2 = type_ xtype in
  let v3 = storage xstorage in
  let v4 = option initialiser init in 
  let entity = G.basic_entity v1 v3 in
  entity, G.VarDef {G.vinit = v4; vtype = Some v2}

and initialiser v = expr v

and storage = function 
  | Extern -> [G.attr G.Extern (fake "extern")] 
  | Static -> [G.attr G.Static (fake "static")]
  | DefaultStorage -> []

let func_def {
                 f_name = f_name;
                 f_type = f_type;
                 f_body = f_body;
                 f_static = f_static
               } =
  let v1 = name f_name in
  let (ret, params) = function_type f_type in
  let v3 = list stmt f_body in 
  let v4 = if f_static then [G.attr G.Static (fake "static")] else [] in
  let entity = G.basic_entity v1 v4 in
  entity, G.FuncDef { G.
    fparams = params |> List.map (fun x -> G.ParamClassic x);
    frettype = Some ret;
    fbody = G.stmt1 v3;
    }

let rec
  struct_def { s_name = s_name; s_kind = s_kind; s_flds = s_flds } =
  let v1 = name s_name in
  let v3 = bracket (list field_def) s_flds in 
  let entity = G.basic_entity v1 [] in
  (match s_kind with
  | Struct -> 
        let fields = bracket (List.map (fun (n, t) -> 
              G.basic_field n None (Some t))) v3 in
        entity, G.TypeDef ({ G.tbody = G.AndType fields })
  | Union ->
        let ctors = v3 |> G.unbracket |> (List.map (fun (n, t) -> 
              G.OrUnion (n,t)))   in
        entity, G.TypeDef ({ G.tbody = G.OrType ctors })
  )

  
and field_def { fld_name = fld_name; fld_type = fld_type } =
  let v1 = option name fld_name in 
  let v2 = type_ fld_type in
  opt_to_ident v1, v2
  

let enum_def (v1, v2) =
  let v1 = name v1
  and v2 =
    list
      (fun (v1, v2) ->
         let v1 = name v1 and v2 = option const_expr v2 in v1, v2)
      v2
  in
  let entity = G.basic_entity v1 [] in
  let ors = v2 |> List.map (fun (n, eopt) -> G.OrEnum (n, eopt))
  in
  entity, G.TypeDef ({ G.tbody = G.OrType ors})

let type_def (v1, v2) = let v1 = name v1 and v2 = type_ v2 in
  let entity = G.basic_entity v1 [] in
  entity, G.TypeDef ({ G.tbody = G.AliasType v2 })

let define_body =
  function
  | CppExpr v1 -> let v1 = expr v1 in G.E v1
  | CppStmt v1 -> let v1 = stmt v1 in G.S v1

let toplevel =
  function
  | Include (t, v1) -> let v1 = wrap string v1 in 
      G.DirectiveStmt (G.ImportAs (t, G.FileName v1, None))
  | Define ((v1, v2)) -> 
    let v1 = name v1 and v2 = define_body v2 in
    let ent = G.basic_entity v1 [] in
    G.DefStmt (ent, G.MacroDef { G.macroparams = []; G.macrobody = [v2]})
  | Macro ((v1, v2, v3)) ->
      let v1 = name v1
      and v2 = list name v2
      and v3 = define_body v3
      in
      let ent = G.basic_entity v1 [] in
      G.DefStmt (ent, G.MacroDef { G.macroparams = v2; G.macrobody = [v3]})
  | StructDef v1 -> let v1 = struct_def v1 in
      G.DefStmt v1
  | TypeDef v1 -> let v1 = type_def v1 in
      G.DefStmt v1
  | EnumDef v1 -> let v1 = enum_def v1 in
      G.DefStmt v1
  | FuncDef v1 -> let v1 = func_def v1 in
      G.DefStmt v1
  | Global v1 -> let v1 = var_decl v1 in
      G.DefStmt v1
  | Prototype v1 -> let v1 = func_def v1 in 
      G.DefStmt v1

let program v = 
 list toplevel v

let any =
  function
  | Expr v1 -> let v1 = expr v1 in G.E v1
  | Stmt v1 -> let v1 = stmt v1 in G.S v1
  | Stmts v1 -> let v1 = list stmt v1 in G.Ss v1
  | Type v1 -> let v1 = type_ v1 in G.T v1
  | Toplevel v1 -> let v1 = toplevel v1 in G.S v1
  | Program v1 -> let v1 = program v1 in G.Pr v1

