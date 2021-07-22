(*
   Stack-safe re-implementation of the popular functions of the standard
   List module.
*)

(*
   This is safe and simple but a little slow.
*)
(*
let naive_safe_map f l =
  List.rev_map f l |> List.rev
*)

(*
   Custom list type used to store intermediate lists, while minimizing
   the number of allocated blocks.
*)
type 'a list5 =
  | Elt of 'a * 'a list5
  | Tuple of 'a * 'a * 'a * 'a * 'a * 'a list5
  | Empty

let rev5 l =
  let rec aux acc l =
    match l with
    | Tuple (e, d, c, b, a, l) ->
        (* common case *)
        aux (a :: b :: c :: d :: e :: acc) l
    | Elt (a, l) ->
        aux (a :: acc) l
    | Empty -> acc
  in
  aux [] l

let rec slow_map acc f l =
  match l with
  | [] -> rev5 acc
  | [a] -> rev5 (Elt (f a, acc))
  | [a; b] ->
      let a = f a in
      let b = f b in
      rev5 (Elt (b, Elt (a, acc)))
  | [a; b; c] ->
      let a = f a in
      let b = f b in
      let c = f c in
      rev5 (Elt (c, (Elt (b, (Elt (a, acc))))))
  | [a; b; c; d] ->
      let a = f a in
      let b = f b in
      let c = f c in
      let d = f d in
      rev5 (Elt (d, (Elt (c, (Elt (b, (Elt (a, acc))))))))
  | [a; b; c; d; e] ->
      let a = f a in
      let b = f b in
      let c = f c in
      let d = f d in
      let e = f e in
      rev5 (Elt (e, (Elt (d, (Elt (c, (Elt (b, (Elt (a, acc))))))))))
  | a :: b :: c :: d :: e :: l ->
      let a = f a in
      let b = f b in
      let c = f c in
      let d = f d in
      let e = f e in
      slow_map (Tuple (e, d, c, b, a, acc)) f l

let rec fast_map rec_calls_remaining f l =
  if rec_calls_remaining <= 0 then
    slow_map Empty f l
  else
    match l with
    | [] -> []
    | [a] -> [f a]
    | [a; b] ->
        let a = f a in
        let b = f b in
        [a; b]
    | [a; b; c] ->
        let a = f a in
        let b = f b in
        let c = f c in
        [a; b; c]
    | [a; b; c; d] ->
        let a = f a in
        let b = f b in
        let c = f c in
        let d = f d in
        [a; b; c; d]
    | [a; b; c; d; e] ->
        let a = f a in
        let b = f b in
        let c = f c in
        let d = f d in
        let e = f e in
        [a; b; c; d; e]
    | a :: b :: c :: d :: e :: l ->
        let a = f a in
        let b = f b in
        let c = f c in
        let d = f d in
        let e = f e in
        a :: b :: c :: d :: e :: fast_map (rec_calls_remaining - 1) f l

(*
   This implementation of List.map makes at most 1000 non-tailrec calls
   before switching to a slower tailrec implementation.

   Additionally, this implementation guarantees left-to-right evaluation.
*)
let map f l = fast_map 1000 f l


(* List.rev_mapi isn't available *)
let mapi f l =
  let rec aux acc i l =
    match l with
    | [] -> List.rev acc
    | x :: l ->
        let y = f i x in
        aux (y :: acc) (i + 1) l
  in
  aux [] 0 l

let map2 f a b =
  List.rev_map2 f a b |> List.rev

let append a b =
  List.rev_append (List.rev a) b

(*
let flatten ll =
  match ll with
  | [] -> []
  | ll ->
      List.fold_left (fun acc l -> List.rev_append l acc) [] ll
      |> List.rev
*)
(* temporary! *)
let flatten = List.flatten

let concat = flatten

(*
let fold_right f l acc =
  match l with
  | [] -> acc
  | l ->
      List.fold_left (fun acc l -> f l acc) acc (List.rev l)
*)
(* temporary! *)
let fold_right = List.fold_right

let fold_right2 f al bl acc =
  List.fold_left2 (fun acc a b -> f a b acc) acc (List.rev al) (List.rev bl)

let remove_assoc k l =
  let rec aux acc = function
    | [] -> List.rev acc
    | ((k2, _) as kv) :: l ->
        if k2 = k then
          List.rev_append acc l
        else
          aux (kv :: acc) l
  in
  aux [] l

(* Same as assoc but using '==' instead of '=' *)
let remove_assq k l =
  let rec aux acc = function
    | [] -> List.rev acc
    | ((k2, _) as kv) :: l ->
        if k2 == k then
          List.rev_append acc l
        else
          aux (kv :: acc) l
  in
  aux [] l

let rec rev_split al bl l =
  match l with
  | [] -> (al, bl)
  | (a, b) :: l -> rev_split (a :: al) (b :: bl) l

let split l =
  List.rev l
  |> rev_split [] []

let rec rev_combine acc al bl =
  match al, bl with
  | [], [] -> acc
  | a :: al, b :: bl -> rev_combine ((a, b) :: acc) al bl
  | [], _ | _, [] -> invalid_arg "Ls.combine"

let combine al bl =
  rev_combine [] al bl
  |> List.rev

let merge cmp l1 l2 =
  let rec aux acc l1 l2 =
    match l1, l2 with
    | [], l2 -> List.rev_append acc l2
    | l1, [] -> List.rev_append acc l1
    | h1 :: t1, h2 :: t2 ->
        if cmp h1 h2 <= 0 then
          aux (h1 :: acc) t1 l2
        else
          aux (h2 :: acc) l1 t2
  in
  aux [] l1 l2
