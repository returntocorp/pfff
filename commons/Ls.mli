(*
   Safe re-implementation of the unsafe parts of the standard List module.

   Several functions of OCaml's List module run in O(list length) stack
   space, occasionally resulting in crashes due to stack overflows.

   This module provides simple but safe implementations for those problematic
   functions.

   See also: suggestion for OCaml's standard library
             https://github.com/ocaml/ocaml/issues/10528

   At the time of writing, here's the popularity ranking of the List functions
   as used across pfff and semgrep-core:

   1800 List.map *
    715 List.iter
    471 List.rev
    220 List.length
    179 List.flatten *
    157 List.fold_left
    149 List.hd
    127 List.filter
     87 List.mem
     65 List.assoc
     49 List.tl
     46 List.exists
     35 List.fold_right *
     33 List.for_all
     22 List.sort
     21 List.partition
     21 List.concat * [alias for List.flatten]
     20 List.rev_map
     19 List.concat_map
     17 List.find
     16 List.nth
     11 List.rev_append
      9 List.filter_map
      8 List.split *
      8 List.assoc_opt
      7 List.mapi *
      7 List.append * [(@) is usually used instead]
      6 List.mem_assoc
      4 List.set
      4 List.remove_assoc *
      4 List.find_opt
      2 List.map2 *
      1 List.to_seq
      1 List.sort_uniq
      1 List.of_seq
      1 List.nth_opt
      1 List.init
      1 List.for_all2

   * = unsafe

   Other unsafe functions:
   - List.remove_assq
   - List.combine
   - List.merge
*)

val map : ('a -> 'b) -> 'a list -> 'b list
val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list

val append : 'a list -> 'a list -> 'a list
val flatten : 'a list list -> 'a list
val concat : 'a list list -> 'a list

val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
val fold_right2 : ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c

val remove_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list
val remove_assq : 'a -> ('a * 'b) list -> ('a * 'b) list

val split : ('a * 'b) list -> 'a list * 'b list
val combine : 'a list -> 'b list -> ('a * 'b) list

val merge : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
