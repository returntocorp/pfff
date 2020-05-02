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

open Ast_python
module G = Ast_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Ast_python to Ast_generic.
 *
 * See ast_generic.ml for more information.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let id = fun x -> x
let option = Common.map_opt
let list = List.map
let vref f x = ref (f !x)

let string = id
let bool = id

let fake s = Parse_info.fake_info s
let fake_bracket x = fake "(", x, fake ")"

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let info x = x

let wrap = fun _of_a (v1, v2) ->
  let v1 = _of_a v1 and v2 = info v2 in 
  (v1, v2)

let bracket of_a (t1, x, t2) = (info t1, of_a x, info t2)

let name v = wrap string v

let dotted_name v = list name v

let module_name (v1, dots) = 
  let v1 = dotted_name v1 in
  match dots with
  | None -> G.DottedName v1
  (* transforming '. foo.bar' in G.Filename "./foo/bar" *)
  | Some toks ->
      let count = 
        toks |> List.map Parse_info.str_of_info 
          |> String.concat "" |> String.length in
      let tok = List.hd toks in
      let elems = v1 |> List.map fst in
      let prefixes = 
        match count with
        | 1 -> ["."]
        | 2 -> [".."]
        | n -> Common2.repeat ".." (n -1)
      in
      let s = String.concat "/" (prefixes @ elems) in
      G.FileName (s, tok)


let resolved_name =
  function
  | LocalVar -> Some (G.Local, G.sid_TODO)
  | Parameter -> Some (G.Param, G.sid_TODO)
  | GlobalVar -> Some (G.Global, G.sid_TODO)
  | ClassField -> None
  | ImportedModule xs -> Some (G.ImportedModule (G.DottedName xs), G.sid_TODO)
  | ImportedEntity xs -> Some (G.ImportedEntity xs, G.sid_TODO)
  | NotResolved -> None

let expr_context =
  function
  | Load -> ()
  | Store -> ()
  | Del -> ()
  | AugLoad -> ()
  | AugStore -> ()
  | Param -> ()


let rec expr (x: expr) =
  match x with
  | Bool v1 -> 
    let v1 = wrap bool v1 in
     G.L (G.Bool v1)
  | None_ x ->
     let x = info x in
     G.L (G.Null x)
  | Ellipsis x ->
     let x = info x in
     G.Ellipsis x
  | Num v1 -> 
      let v1 = number v1 in 
      G.L v1
  | Str (v1) -> 
      let v1 = wrap string v1 in
      G.L (G.String (v1))
  | EncodedStr (v1, pre) ->
      let v1 = wrap string v1 in
      (* reuse same tok *)
      let tok = snd v1 in
      G.Call (G.IdSpecial (G.EncodedString (pre, tok), tok),
        [G.Arg (G.L (G.String (v1)))])

  | InterpolatedString xs ->
    G.Call (G.IdSpecial (G.Concat, fake "concat"), 
      xs |> List.map (fun x -> let x = expr x in G.Arg (x))
    )
  | TypedExpr (v1, v2) ->
     let v1 = expr v1 in
     let v2 = type_ v2 in
     G.Cast (v2, v1)
  | TypedMetavar (v1, v2, v3) ->
     let v1 = name v1 in
     let v3 = type_ v3 in
     G.TypedMetavar (v1, v2, v3)
  | ExprStar v1 ->
    let v1 = expr v1 in
    G.Call (G.IdSpecial (G.Spread, fake "spread"), [G.expr_to_arg v1])

  | Name ((v1, v2, v3)) ->
      let v1 = name v1
      and _v2TODO = expr_context v2
      and v3 = vref resolved_name v3
      in 
      G.Id (v1 ,{ (G.empty_id_info ()) with G.id_resolved = v3 })
          
  | Tuple ((CompList v1, v2)) ->
      let (_, v1, _) = bracket (list expr) v1 
      and _v2TODO = expr_context v2 in 
      G.Tuple v1

  | Tuple ((CompForIf (v1, v2), v3)) ->
      let e1 = comprehension expr v1 v2 in
      let _v4TODO = expr_context v3 in 
      G.Tuple e1

  | List ((CompList v1, v2)) ->
      let v1 = bracket (list expr) v1 
      and _v2TODO = expr_context v2 in 
      G.Container (G.List, v1)

  | List ((CompForIf (v1, v2), v3)) ->
      let e1 = comprehension expr v1 v2 in
      let _v3TODO = expr_context v3 in 
      G.Container (G.List, fake_bracket e1)

  | Subscript ((v1, v2, v3)) ->
      let e = expr v1 
      and _v3TODO = expr_context v3 in 
      (match v2 with
      | [x] -> slice e x
      | _ -> 
        let xs = list (slice e) v2 in
        G.OtherExpr (G.OE_Slices, xs |> List.map (fun x -> G.E x))
      )
  | Attribute ((v1, t, v2, v3)) ->
      let v1 = expr v1 
      and t = info t 
      and v2 = name v2 
      and _v3TODO = expr_context v3 in 
      G.DotAccess (v1, t, G.FId v2)

  | DictOrSet (CompList v) -> 
      let v = bracket (list dictorset_elt) v in 
      (* less: could be a Set if alls are Key *)
      G.Container (G.Dict, v)

  | DictOrSet (CompForIf (v1, v2)) -> 
      let e1 = comprehension2 dictorset_elt v1 v2 in
      G.Container (G.Dict, fake_bracket e1)

  | BoolOp (((v1,tok), v2)) -> 
      let v1 = boolop v1 
      and v2 = list expr v2 in 
      G.Call (G.IdSpecial (G.ArithOp v1, tok), v2 |> List.map G.expr_to_arg)
  | BinOp ((v1, (v2, tok), v3)) ->
      let v1 = expr v1 and v2 = operator v2 and v3 = expr v3 in
      G.Call (G.IdSpecial (G.ArithOp v2, tok), [v1;v3] |> List.map G.expr_to_arg)
  | UnaryOp (((v1, tok), v2)) -> let v1 = unaryop v1 and v2 = expr v2 in 
      (match v1 with
      | Left op ->
            G.Call (G.IdSpecial (G.ArithOp op, tok), [v2] |> List.map G.expr_to_arg)
      | Right oe ->
            G.OtherExpr (oe, [G.E v2])
      )
  | Compare ((v1, v2, v3)) ->
      let v1 = expr v1
      and v2 = list cmpop v2
      and v3 = list expr v3 in
      (match v2, v3 with
      | [Left op, tok], [e] ->
        G.Call (G.IdSpecial (G.ArithOp op, tok), [v1;e] |> List.map G.expr_to_arg)
      | [Right oe, _tok], [e] ->
        G.OtherExpr (oe, [G.E v1; G.E e])
      | _ ->  
        let anyops = 
           v2 |> List.map (function
            | Left arith, tok -> G.E (G.IdSpecial (G.ArithOp arith, tok))
            | Right other, _tok -> G.E (G.OtherExpr (other, []))
            ) in
        let any = anyops @ (v3 |> List.map (fun e -> G.E e)) in
        G.OtherExpr (G.OE_CmpOps, any)
      )
  | Call (v1, v2) -> let v1 = expr v1 in let v2 = list argument v2 in 
      G.Call (v1, v2)

  | Lambda ((v1, v2)) -> let v1 = parameters v1 and v2 = expr v2 in 
      G.Lambda ({G.fparams = v1; fbody = G.ExprStmt v2; frettype = None})
  | IfExp ((v1, v2, v3)) ->
      let v1 = expr v1 and v2 = expr v2 and v3 = expr v3 in
      G.Conditional (v1, v2, v3)
  | Yield ((t, v1, v2)) ->
      let v1 = option expr v1
      and v2 = v2 in
      G.Yield (t, v1, v2)
  | Await (t, v1) -> let v1 = expr v1 in
      G.Await (t, v1)
  | Repr v1 -> let (_, v1, _) = bracket expr v1 in
      G.OtherExpr (G.OE_Repr, [G.E v1])

and argument = function
  | Arg e -> let e = expr e in 
      G.Arg e
  | ArgStar e -> let e = expr e in
      G.Arg (G.Call (G.IdSpecial (G.Spread, fake "spread"), [G.expr_to_arg e]))
  | ArgPow e -> 
      let e = expr e in
      G.ArgOther (G.OA_ArgPow, [G.E e])
  | ArgKwd (n, e) -> let n = name n in let e = expr e in
      G.ArgKwd (n, e)
  | ArgComp (e, xs) ->
      let e = expr e in
      G.ArgOther (G.OA_ArgComp, (G.E e)::list for_if xs)

and for_if = function
  | CompFor (e1, e2) -> 
      let e1 = expr e1 in let e2 = expr e2 in
      G.E (G.OtherExpr (G.OE_CompFor, [G.E e1; G.E e2]))
  | CompIf (e1) -> 
      let e1 = expr e1 in
      G.E (G.OtherExpr (G.OE_CompIf, [G.E e1]))


and dictorset_elt = function
  | KeyVal (v1, v2) -> let v1 = expr v1 in let v2 =  expr v2 in 
      G.Tuple [v1; v2]
  | Key (v1) -> 
      let v1 = expr v1 in
      v1
  | PowInline (v1) -> 
      let v1 = expr v1 in
      G.Call (G.IdSpecial (G.Spread, fake "spread"), [G.expr_to_arg v1])
  
and number =
  function
  | Int v1     -> let v1 = wrap id v1 in G.Int v1
  | LongInt v1 -> let v1 = wrap id v1 in G.Int v1
  | Float v1   -> let v1 = wrap id v1 in G.Float v1
  | Imag v1    -> let v1 = wrap string v1 in G.Imag v1


and boolop = function 
  | And -> G.And
  | Or  -> G.Or

and operator =
  function
  | Add      -> G.Plus
  | Sub      -> G.Minus
  | Mult     -> G.Mult
  | Div      -> G.Div
  | Mod      -> G.Mod
  | Pow      -> G.Pow
  | FloorDiv -> G.FloorDiv
  | LShift   -> G.LSL
  | RShift   -> G.LSR
  | BitOr    -> G.BitOr
  | BitXor   -> G.BitXor
  | BitAnd   -> G.BitAnd
  | MatMult  -> G.MatMult

and unaryop = function 
  | Invert -> Right G.OE_Invert
  | Not    -> Left G.Not
  | UAdd   -> Left G.Plus
  | USub   -> Left G.Minus

and cmpop (a,b) =
  match a with
  | Eq    -> Left G.Eq, b
  | NotEq -> Left G.NotEq, b
  | Lt    -> Left G.Lt, b
  | LtE   -> Left G.LtE, b
  | Gt    -> Left G.Gt, b
  | GtE   -> Left G.GtE, b
  | Is    -> Left G.PhysEq, b
  | IsNot -> Left G.NotPhysEq, b
  | In    -> Right G.OE_In, b
  | NotIn -> Right G.OE_NotIn, b

and comprehension f v1 v2 =
  let v1 = f v1 in
  let v2 = list for_if v2 in
  [G.OtherExpr (G.OE_CompForIf, (G.E v1)::v2)]

and comprehension2 f v1 v2 =
  let v1 = f v1 in
  let v2 = list for_if v2 in
  [G.OtherExpr (G.OE_CompForIf, (G.E v1)::v2)]

and slice e =
  function
  | Index v1 -> let v1 = expr v1 in G.ArrayAccess (e, v1)
  | Slice ((v1, v2, v3)) ->
      let v1 = option expr v1
      and v2 = option expr v2
      and v3 = option expr v3
      in
      G.SliceAccess (e, v1, v2, v3)

and parameters xs =
  xs |> List.map (function
  | ParamClassic ((n, topt), eopt) ->
     let n = name n in
     let topt = option type_ topt in
     let eopt = option expr eopt in
     G.ParamClassic { (G.param_of_id n) with G.ptype = topt; pdefault = eopt; }
  | ParamStar (n, topt) ->
     let n = name n in
     let topt = option type_ topt in
     G.ParamClassic { (G.param_of_id n) with
       G.ptype = topt; pattrs = [G.attr G.Variadic (fake "...")]; }
   | ParamPow (n, topt) ->
     let n = name n in
     let topt = option type_ topt in
     G.OtherParam (G.OPO_KwdParam, 
            [G.I n] @ (match topt with None -> [] | Some t -> [G.T t]))
   | ParamEllipsis tok -> G.ParamEllipsis tok
   | ParamSingleStar tok ->
     G.OtherParam (G.OPO_SingleStarParam, [G.Tk tok])
  )
 

and type_ v = 
  let v = expr v in
  G.expr_to_type v

and type_parent v = 
  let v = argument v in
  G.OtherType (G.OT_Arg, [G.Ar v])

and list_stmt1 xs =
  match (list stmt xs) with
  (* bugfix: We do not want actually to optimize and remove the
   * intermediate Block because otherwise sgrep will not work
   * correctly with a list of stmt. 
   *
   * old: | [e] -> e
   *
   * For example
   * if $E:
   *   ...
   *   foo()
   *
   * will not match code like
   *
   * if True:
   *   foo()
   * 
   * because above we have a Block ([Ellipsis; foo()] and down we would
   * have just (foo()). We do want Block ([foo()]].
   *
   * Unless the body is actually just a metavar, in which case we probably
   * want to match a list of stmts, as in
   *
   *  if $E:
   *    $S
   *
   * in which case we remove the G.Block around it.
   * hacky ...
   *)
  | [G.ExprStmt (G.Id (_)) as x] -> x

  | xs -> G.Block xs

and stmt_aux x =
  match x with
  | FunctionDef ((v1, v2, v3, v4, v5)) ->
      let v1 = name v1
      and v2 = parameters v2
      and v3 = option type_ v3
      and v4 = list_stmt1 v4
      and v5 = list decorator v5
      in
      let ent = G.basic_entity v1 v5 in
      let def = { G.fparams = v2; frettype = v3; fbody = v4; } in
      [G.DefStmt (ent, G.FuncDef def)]
  | ClassDef ((v1, v2, v3, v4)) ->
      let v1 = name v1
      and v2 = list type_parent v2
      and v3 = list stmt v3
      and v4 = list decorator v4
      in 
      let ent = G.basic_entity v1 v4 in
      let def = { G.ckind = G.Class; cextends = v2; 
                  cimplements = []; cmixins = [];
                  cbody = fake_bracket (v3 |> List.map(fun x ->G.FieldStmt x);)
                } in
      [G.DefStmt (ent, G.ClassDef def)]

  (* TODO: should turn some of those in G.LocalDef (G.VarDef ! ) *)
  | Assign ((v1, v2, v3)) -> 
      let v1 = list expr v1 and v2 = info v2 and v3 = expr v3 in
      (match v1 with
      | [] -> raise Impossible
      | [a] -> [G.ExprStmt (G.Assign (a, v2, v3))]
      | xs -> [G.ExprStmt (G.Assign (G.Tuple xs, v2, v3))]
      )
  | AugAssign ((v1, (v2, tok), v3)) ->
      let v1 = expr v1 and v2 = operator v2 and v3 = expr v3 in
      [G.ExprStmt (G.AssignOp (v1, (v2, tok), v3))]
  | Return (t, v1) -> let v1 = option expr v1 in 
      [G.Return (t, v1)]

  | Delete (_t, v1) -> let v1 = list expr v1 in
      [G.OtherStmt (G.OS_Delete, v1 |> List.map (fun x -> G.E x))]
  | If ((t, v1, v2, v3)) ->
      let v1 = expr v1
      and v2 = list_stmt1 v2
      and v3 = list_stmt1 v3
      in
      [G.If (t, v1, v2, v3)]

  | While ((t, v1, v2, v3)) ->
      let v1 = expr v1
      and v2 = list_stmt1 v2
      and v3 = list stmt v3
      in
      (match v3 with
      | [] -> [G.While (t, v1, v2)]
      | _ -> [G.Block [
              G.While (t, v1,v2); 
              G.OtherStmt (G.OS_WhileOrElse, v3 |> List.map (fun x -> G.S x))]]
      )
            
  | For ((t, v1, t2, v2, v3, v4)) ->
      let foreach = pattern v1
      and ins = expr v2
      and body = list_stmt1 v3
      and orelse = list stmt v4
      in
      let header = G.ForEach (foreach, t2, ins) in
      (match orelse with
      | [] -> [G.For (t, header, body)]
      | _ -> [G.Block [
              G.For (t, header, body);
              G.OtherStmt (G.OS_ForOrElse, orelse|> List.map (fun x -> G.S x))]]
      )
  (* TODO: unsugar in sequence? *)
  | With ((_t, v1, v2, v3)) ->
      let v1 = expr v1
      and v2 = option expr v2
      and v3 = list_stmt1 v3
      in
      let e =
        match v2 with
        | None -> v1
        | Some e2 -> G.LetPattern (G.expr_to_pattern e2, v1)
      in
      [G.OtherStmtWithStmt (G.OSWS_With, e, v3)]

  | Raise (t, v1) ->
      (match v1 with
      | Some (e, None) -> 
        let e = expr e in 
        [G.Throw (t, e)]
      | Some (e, Some from) -> 
        let e = expr e in
        let from = expr from in
        let st = G.Throw (t, e) in
        [G.OtherStmt (G.OS_ThrowFrom, [G.E from; G.S st])]
      | None ->
        [G.OtherStmt (G.OS_ThrowNothing, [G.Tk t])]
      )
                  
  | TryExcept ((t, v1, v2, v3)) ->
      let v1 = list_stmt1 v1
      and v2 = list excepthandler v2
      and orelse = list stmt v3
      in
      (match orelse with
      | [] -> [G.Try (t, v1, v2, None)]
      | _ -> [G.Block [
              G.Try (t, v1, v2, None);
              G.OtherStmt (G.OS_TryOrElse, orelse |> List.map (fun x -> G.S x))
              ]]
      )

  | TryFinally ((t, v1, t2, v2)) ->
      let v1 = list_stmt1 v1 and v2 = list_stmt1 v2 in
      (* could lift down the Try in v1 *)
      [G.Try (t, v1, [], Some (t2, v2))]

  | Assert ((t, v1, v2)) -> let v1 = expr v1 and v2 = option expr v2 in
      [G.Assert (t, v1, v2)]

  | ImportAs (t, v1, v2) -> 
      let mname = module_name v1 and nopt = option name v2 in
      [G.DirectiveStmt (G.ImportAs (t, mname, nopt))]
  | ImportAll (t, v1, v2) -> 
      let mname = module_name v1 and v2 = info v2 in
      [G.DirectiveStmt (G.ImportAll (t, mname, v2))]

  | ImportFrom (t, v1, v2) ->
      let v1 = module_name v1
      and v2 = list alias v2
      in
      [G.DirectiveStmt (G.ImportFrom (t, v1, v2))]

  | Global (t, v1) | NonLocal (t, v1)
    -> let v1 = list name v1 in
      v1 |> List.map (fun x -> 
          let ent = G.basic_entity x [] in
          G.DefStmt (ent, G.UseOuterDecl t))

  | ExprStmt v1 -> let v1 = expr v1 in 
      [G.ExprStmt v1]

  | Async (t, x) ->
      let x = stmt x in
      (match x with
      | G.DefStmt (ent, func) ->
          [G.DefStmt ({ ent with G.attrs = (G.attr G.Async t)
                                          ::ent.G.attrs}, func)]
      | _ -> [G.OtherStmt (G.OS_Async, [G.S x])]
      )

  | Pass t -> [G.OtherStmt (G.OS_Pass, [G.Tk t])]
  | Break t -> [G.Break (t, G.LNone)]
  | Continue t -> [G.Continue (t, G.LNone)]

  (* python2: *)
  | Print (tok, _dest, vals, _nl) -> 
      let id = Name (("print", tok), Load, ref NotResolved) in
      stmt_aux (ExprStmt (Call (id, vals |> List.map (fun e -> Arg e))))

  | Exec (tok, e, _eopt, _eopt2) -> 
      let id = Name (("exec", tok), Load, ref NotResolved) in
      stmt_aux (ExprStmt (Call (id, [Arg e])))

and stmt x = 
  G.stmt1 (stmt_aux x)

and pattern e = 
  let e = expr e in
  G.expr_to_pattern e

and excepthandler =
  function
  | ExceptHandler ((t, v1, v2, v3)) ->
      let v1 = option pattern v1 (* a type actually, even tuple of types *)
      and v2 = option name v2
      and v3 = list_stmt1 v3
      in t,
      (match v1, v2 with
      | Some e, None ->
         e
      | None, None -> 
         G.PatUnderscore (fake "_")
      | None, Some _ -> raise Impossible (* see the grammar *)
      | Some pat, Some n ->
         G.PatAs (pat, (n, G.empty_id_info ()))
      ), v3

and expr_to_attribute v  = 
  match v with
  | G.Call (G.Id (id, _), args) -> 
      G.NamedAttr (id, args)
  | _ -> G.OtherAttribute (G.OA_Expr, [G.E v])

and decorator v = 
  let v = expr v in
  expr_to_attribute v

and alias (v1, v2) = 
  let v1 = name v1 and v2 = option name v2 in 
  v1, v2

let program v = 
  let v = list stmt v in
  v

let any =
  function
  | Expr v1 -> let v1 = expr v1 in G.E v1
  | Stmt v1 -> let v1 = stmt v1 in G.S v1
  (* TODO? should use list stmt_aux here? Some intermediate Block
   * could be inserted preventing some sgrep matching?
   *)
  | Stmts v1 -> let v1 = list stmt v1 in G.Ss v1
  | Program v1 -> let v1 = program v1 in G.Pr v1
  | DictElem v1 -> let v1 = dictorset_elt v1 in G.E v1
      

