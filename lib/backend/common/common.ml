module IdVar = struct
  type t = string * KosuIrTyped.Asttyped.rktype

  let compare = compare
end

module IdVarMap = Map.Make (IdVar)

(**
  This module specifies the difference in label name convenient
  Moslty between MacOs and Linux with the use or not of an underscore    
*)
module type AsmSpecification = sig
  val label_prefix: string

  val label_of_external_fn: string -> string
  val label_of_constant: ?module_path:string -> string -> string

  val label_of_external_function: KosuIrTyped.Asttyped.rexternal_func_decl -> string

  val label_of_kosu_function: module_path:string -> KosuIrTyped.Asttyped.rfunction_decl -> string
  val main: string
end


let align_16 size =
  let ( ** ) = Int64.mul in
  let ( ++ ) = Int64.add in
  let div = Int64.unsigned_div size 16L in
  let modulo = if Int64.unsigned_rem size 16L = 0L then 0L else 1L in
  16L ** (div ++ modulo)