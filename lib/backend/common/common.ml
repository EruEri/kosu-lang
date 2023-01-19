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

module type InstructionLine = sig
  type raw_line
end

module type AsmProgram = functor (InstructionLine : InstructionLine)
    -> sig
    type raw_line = InstructionLine.raw_line

    type asm_function_decl = {
      asm_name : string;
      asm_body : raw_line list;
    }

    type asm_const_decl = {
      asm_const_name : string;
      value :
        [ `IntVal of KosuFrontend.Ast.isize * int64
        | `StrVal of string ];
    }

    type asm_module_node =
      | Afunction of asm_function_decl
      | AConst of asm_const_decl

    type asm_module = AsmModule of asm_module_node list

    type asm_module_path = {
      apath : string;
      asm_module : asm_module;
    }

    type named_asm_module_path = {
      filename : string;
      asm_module_path : asm_module_path;
      rprogram : KosuIrTyped.Asttyped.rprogram;
      str_lit_map : (string, Util.stringlit_label) Hashtbl.t;
    }

    type asm_program = named_asm_module_path list
end

module AsmProgram(InstructionLine: InstructionLine) = struct
  type raw_line = InstructionLine.raw_line

  type asm_function_decl = {
    asm_name : string;
    asm_body : raw_line list;
  }

  type asm_const_decl = {
    asm_const_name : string;
    value : [ `IntVal of KosuFrontend.Ast.isize * int64 | `StrVal of string ];
  }

  type asm_module_node =
    | Afunction of asm_function_decl
    | AConst of asm_const_decl

  type asm_module = AsmModule of asm_module_node list
  type asm_module_path = { apath : string; asm_module : asm_module }

  type named_asm_module_path = {
    filename : string;
    asm_module_path : asm_module_path;
    rprogram : KosuIrTyped.Asttyped.rprogram;
    str_lit_map : (string, Util.stringlit_label) Hashtbl.t;
  }

  type asm_program = named_asm_module_path list


end


