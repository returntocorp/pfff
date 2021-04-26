(* Yoann Padioleau
 *
 * Copyright (C) 2021 R2C
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
*)
open Common
module T = Parser_scala
module TH = Token_helpers_scala
open Parser_scala

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A recursive-descent Scala parser.
 *
 * This is mostly a port of Parsers.scala in the Scala 2 nsc compiler source
 * to OCaml.
 *
 * alt:
 *  - use Parser.scala of dotty?
 *  - use the one in scalameta?
*)

(* ok with partial match for all those accept *)
[@@@warning "-8"]

(* TODO: temporary *)
[@@@warning "-39-21-27-26"]

let debug_lexer = ref true

(*****************************************************************************)
(* Types  *)
(*****************************************************************************)

type env = {
  (* imitating the Scala implementation *)
  mutable token: T.token;

  (* not in Scala implementation *)
  mutable rest: T.token list;
}

let mk_env toks =
  match toks with
  | [] -> failwith "empty list of tokens, impossible, should at least get EOF"
  | x::xs ->
      { token = x;
        (* assume we will call first nextToken on it *)
        rest = (x::xs);
      }

let ab = Parse_info.abstract_info

(*****************************************************************************)
(* Error management  *)
(*****************************************************************************)
let error x in_ =
  pr2_gen in_.token;
  failwith x

(*****************************************************************************)
(* Helpers  *)
(*****************************************************************************)
(* todo: assert t1 is not a token with value (s, info) *)
let (=~=) t1 t2 =
  (TH.abstract_info_tok t1 =*= TH.abstract_info_tok t2)

let (++=) aref xs =
  ()

let (+=) aref x =
  ()

(*****************************************************************************)
(* Token Helpers  *)
(*****************************************************************************)
let rec nextToken in_ =
  match in_.rest with
  | [] -> failwith "nextToken: no more tokens"
  | x::xs ->
      if !debug_lexer
      then pr2_gen x;
      in_.rest <- xs;
      (match x with
       | Space _ | Comment _ ->
           nextToken in_
       (* TODO: lots of condition on when to do that *)
       | Nl x ->
           in_.token <- NEWLINE x

       | other ->
           in_.token <- other;
      )

(* was called in.next.token *)
let rec next_next_token in_ =
  match in_.rest with
  | [] -> failwith "in_next_token: no more tokens"
  (* TODO: also skip spacing and stuff? *)
  | x::xs -> x

(* supposed to return the offset too *)
let skipToken in_ =
  nextToken in_

let init in_ =
  nextToken in_

(* supposed to return the offset too *)
let accept t in_ =
  if not (in_.token =~= t)
  then error (spf "was expecting: %s" (Common.dump t)) in_;
  (match t with
   | EOF _ -> ()
   | _ -> nextToken in_
  )

let inBraces t in_ =
  failwith "inBraces"

(** {{{ { `sep` part } }}}. *)
let separatedToken sep part in_ =
  let ts = ref [] in
  while in_.token =~= sep do
    nextToken in_;
    let x = part in_ in
    ts += x;
  done;
  !ts

(*****************************************************************************)
(* Newline management  *)
(*****************************************************************************)

(** {{{
 *  semi = nl {nl} | `;`
 *  nl  = `\n` // where allowed
 *  }}}
*)
let acceptStatSep in_ =
  match in_.token with
  | NEWLINE _ | NEWLINES _ -> nextToken in_
  | _ -> accept (SEMI ab) in_

let acceptStatSepOpt in_ =
  if not (TH.isStatSeqEnd in_.token)
  then acceptStatSep in_


let newLineOpt in_ =
  match in_.token with
  | NEWLINE _ -> nextToken in_
  | _ -> ()

let newLineOptWhenFollowedBy token in_ =
  match in_.token, next_next_token in_ with
  | NEWLINE _, x when x =~= token -> newLineOpt in_
  | _ -> ()

(*****************************************************************************)
(* Parsing names  *)
(*****************************************************************************)

let ident in_ =
  match TH.isIdent in_.token with
  | Some (s, info) ->
      nextToken in_;
      (s, info)
  | None ->
      error "expecting ident" in_

let identForType in_ =
  let x = ident in_ in
  x

let selectors ~typeOK id in_ =
  failwith "selectors"

(** {{{
 *   QualId ::= Id {`.` Id}
 *   }}}
*)
let qualId in_ =
  let id = ident in_ in
  match in_.token with
  | DOT _ ->
      skipToken in_;
      selectors id ~typeOK:false in_
  | _ -> id


let pkgQualId in_ =
  let pkg = qualId in_ in
  newLineOptWhenFollowedBy (LBRACE ab) in_;
  pkg

(*****************************************************************************)
(* Parsing types  *)
(*****************************************************************************)

(*****************************************************************************)
(* Parsing expressions  *)
(*****************************************************************************)

(*****************************************************************************)
(* Parsing patterns  *)
(*****************************************************************************)

(*****************************************************************************)
(* Parsing directives  *)
(*****************************************************************************)
let packageObjectDef in_ =
  failwith "packageObjectDef"

let packageOrPackageObject in_ =
  failwith "packageOrPackageObject"

let importClause in_ =
  failwith "importClause"

(*****************************************************************************)
(* Parsing annotations/modifiers  *)
(*****************************************************************************)

(** {{{
 *  AccessQualifier ::= `[` (Id | this) `]`
 *  }}}
*)
let accessQualifierOpt mods in_ =
  let result = mods in
  (match in_.token with
   | LBRACKET _ ->
       nextToken in_;
       (match in_.token with
        | Kthis _ ->
            nextToken in_;
            (* Flags.Local?? *)
        | _ ->
            let x = identForType in_ in
            ()
       );
       accept (RBRACKET ab) in_
  );
  result

let addMods mods t in_ =
  nextToken in_;
  t::mods

(** {{{
 *  Modifiers ::= {Modifier}
 *  Modifier  ::= LocalModifier
 *              |  AccessModifier
 *              |  override
 *  }}}
*)
let modifiers in_ =
  let rec loop mods =
    match in_.token with
    | Kprivate _ | Kprotected _ ->
        let mods = addMods mods in_.token in_ in
        let mods = accessQualifierOpt mods in_ in
        loop mods
    | Kabstract _ | Kfinal _ | Ksealed _ | Koverride _ | Kimplicit _ | Klazy _
      ->
        let mods = addMods mods in_.token in_ in
        loop mods
    | NEWLINE _ ->
        nextToken in_;
        loop mods
    | _ -> mods
  in
  loop []


let readAnnots part in_ =
  separatedToken (AT ab) part in_

(** {{{
 *  Annotations       ::= {`@` SimpleType {ArgumentExprs}}
 *  ConstrAnnotations ::= {`@` SimpleType ArgumentExprs}
 *  }}}
*)
let annotationExpr in_ =
  failwith "annotationExpr"

let annotations ~skipNewLines in_ =
  readAnnots (fun in_ ->
    let t = annotationExpr in_ in
    if skipNewLines then newLineOpt in_;
    t
  )

(*****************************************************************************)
(* Parsing definitions  *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Object *)
(* ------------------------------------------------------------------------- *)
let objectDef in_ =
  failwith "objectDef"

(* ------------------------------------------------------------------------- *)
(* Class/trait *)
(* ------------------------------------------------------------------------- *)
let classDef in_ =
  failwith "classDef"

(* ------------------------------------------------------------------------- *)
(* TmplDef *)
(* ------------------------------------------------------------------------- *)

(** {{{
 *  TmplDef ::= [case] class ClassDef
 *            |  [case] object ObjectDef
 *            |  [override] trait TraitDef
 *  }}}
*)
let tmplDef in_ =
  match in_.token with
  | Ktrait _ -> classDef in_
  | Kclass _ -> classDef in_
  (* Caseclass -> classDef in_ *)
  | Kobject _ -> objectDef in_
  (* Caseobject -> objectDef in_ *)
  | _ -> error "expected start of definition" in_


(** Hook for IDE, for top-level classes/objects. *)
let topLevelTmplDef in_ =
  let _annots = annotations ~skipNewLines:true in_ in
  let _mods = modifiers in_ in
  let x = tmplDef in_ in
  x

let statSeq ?(errorMsg="illegal start of definition") stat in_ =
  let stats = ref [] in
  while not (TH.isStatSeqEnd in_.token) do
    (match stat in_ with
     | Some xs ->
         stats ++= xs
     | None ->
         if TH.isStatSep in_.token
         then ()
         else error errorMsg in_
    );
    acceptStatSepOpt in_
  done;
  !stats

(** {{{
 *  TopStatSeq ::= TopStat {semi TopStat}
 *  TopStat ::= Annotations Modifiers TmplDef
 *            | Packaging
 *            | package object ObjectDef
 *            | Import
 *            |
 *  }}}
*)
let topStat in_ =
  match in_.token with
  | Kpackage _ ->
      skipToken in_;
      let x = packageOrPackageObject in_ in
      Some x
  | Kimport _ ->
      let x = importClause in_ in
      Some x
  | t when TH.isAnnotation t || TH.isTemplateIntro t || TH.isModifier t ->
      let x = topLevelTmplDef in_ in
      Some x
  | _ -> None

let topStatSeq in_ =
  statSeq ~errorMsg:"expected class or object definition" topStat in_

(*****************************************************************************)
(* Entry point  *)
(*****************************************************************************)

(** {{{
 *  CompilationUnit ::= {package QualId semi} TopStatSeq
 *  }}}
*)
let compilationUnit in_ =
  let rec topstats in_ =
    let ts = ref [] in
    while (match in_.token with SEMI _ -> true | _ -> false) do
      nextToken in_
    done;
    (match in_.token with
     | Kpackage _ ->
         nextToken in_;
         (match in_.token with
          | Kobject _ ->
              let xs = packageObjectDef in_ in
              ts ++= xs;
              if not (in_.token =~= (EOF ab)) then begin
                acceptStatSep in_;
                let xs = topStatSeq in_ in
                ts ++= xs
              end
          | _ ->
              let pkg = pkgQualId in_ in
              (match in_.token with
               | EOF _ ->
                   let pack = () in
                   ts += pack
               | x when TH.isStatSep x ->
                   nextToken in_;
                   let xs = topstats in_ in
                   let pack = () in
                   ts += pack
               | _ ->
                   let xs = inBraces topStatSeq in_ in
                   let pack = () in
                   ts += pack;
                   acceptStatSepOpt in_;
                   let xs = topStatSeq in_ in
                   ts ++= xs
              )
         )
     | _ ->
         let xs = topStatSeq in_ in
         ts ++= xs
    );
    !ts
  in
  let xs = topstats in_ in
  ()

let parse toks =
  let in_ = mk_env toks in
  init in_;
  let xs = compilationUnit in_ in
  accept (EOF ab) in_;
  xs
