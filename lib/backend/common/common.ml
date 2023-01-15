module IdVar = struct
  type t = string * KosuIrTyped.Asttyped.rktype

  let compare = compare
end

module IdVarMap = Map.Make (IdVar)


let align_16 size =
  let ( ** ) = Int64.mul in
  let ( ++ ) = Int64.add in
  let div = Int64.unsigned_div size 16L in
  let modulo = if Int64.unsigned_rem size 16L = 0L then 0L else 1L in
  16L ** (div ++ modulo)