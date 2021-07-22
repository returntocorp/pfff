
let require capa l =
  Ls.fold_right (&&)
    (Ls.map (require_capa capa) l)
    true
