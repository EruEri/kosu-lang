(**
  This module specifies the difference in label name convention
  Moslty between MacOs and Linux with the use or not of an underscore    
*)
module type X86_64AsmSpecification = sig

  val function_directives: string -> string list

  val constant_directives: string -> [ `IntVal of KosuFrontend.Ast.isize * int64 | `StrVal of string ] -> string list

  val comment_prefix: string

  val string_litteral_section_start: string

  val string_litteral_section_end: string

  val string_litteral_directive: string

  val size_directive_of_size: KosuFrontend.Ast.isize -> string
  val label_prefix: string

  val label_of_constant: ?module_path:string -> string -> string

  val label_of_external_function: KosuIrTyped.Asttyped.rexternal_func_decl -> string

  (** This function should produce the same label name taht [label_of_tac_function] if the kosu function is the conterpart as the tac one  *)
  val label_of_kosu_function: module_path:string -> KosuIrTyped.Asttyped.rfunction_decl -> string

  (** This function should produce the same label name taht [label_of_kosu_function] if the tac function is the conterpart as the kosu one  *)
  val label_of_tac_function: module_path:string -> KosuIrTAC.Asttac.tac_function_decl -> string

  val label_of_kosu_operator: module_path:string -> KosuIrTyped.Asttyped.roperator_decl -> string
  val label_of_tac_operator: module_path:string -> KosuIrTAC.Asttac.tac_operator_decl -> string
  val main: string
end