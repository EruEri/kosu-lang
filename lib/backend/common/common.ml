(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of Kosu                                                                  *)
(* Copyright (C) 2022-2023 Yves Ndiaye                                                        *)
(*                                                                                            *)
(* Kosu is free software: you can redistribute it and/or modify it under the terms            *)
(* of the GNU General Public License as published by the Free Software Foundation,            *)
(* either version 3 of the License, or (at your option) any later version.                    *)
(*                                                                                            *)
(* Kosu is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;          *)
(* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR           *)
(* PURPOSE.  See the GNU General Public License for more details.                             *)
(* You should have received a copy of the GNU General Public License along with Kosu.         *)
(* If not, see <http://www.gnu.org/licenses/>.                                                *)
(*                                                                                            *)
(**********************************************************************************************)

module IdVar = struct
  type t = string * KosuIrTyped.Asttyped.rktype

  let compare = compare
end

module IdVarMap = Map.Make (IdVar)

module OffsetHelper = struct
  let align_16 size =
    let ( ** ) = Int64.mul in
    let ( ++ ) = Int64.add in
    let div = Int64.unsigned_div size 16L in
    let modulo = if Int64.unsigned_rem size 16L = 0L then 0L else 1L in
    16L ** (div ++ modulo)

  let offset_of_field_access first_ktype ~fields rprogram =
    let open KosuIrTyped.Asttyhelper in
    fields
    |> List.fold_left
         (fun (acc_offset, acc_type) field ->
           (* let () = Printf.printf "field : %s :: %s\n%!" field (KosuIrTyped.Asttypprint.string_of_rktype acc_type) in *)
           let struct_decl =
             match
               KosuIrTyped.Asttyhelper.RProgram.find_type_decl_from_rktye
                 acc_type rprogram
             with
             | Some (RDecl_Struct s) -> s
             | Some (RDecl_Enum _) ->
                 failwith "Expected to find a struct get an enum"
             | None -> failwith "Non type decl ??? my validation is very weak"
           in

           let extracted = RType.extract_parametrics_rktype acc_type in
           let mapped_generics = List.combine struct_decl.generics extracted in
           let hashmap_generis =
             mapped_generics |> List.to_seq |> Hashtbl.of_seq
           in
           let struct_decl =
             RStruct.instanciate_struct_decl mapped_generics struct_decl
           in
           let offset =
             KosuIrTyped.Asttyconvert.Sizeof.offset_of_field
               ~generics:hashmap_generis field struct_decl rprogram
           in
           let acc_offset = Int64.add acc_offset offset in
           let field_type =
             Option.get @@ RStruct.rktype_of_field_opt field struct_decl
           in
           (acc_offset, field_type))
         (0L, first_ktype)
    |> fst
end

module type InstructionLine = sig
  type raw_line
end

module AsmProgram (InstructionLine : InstructionLine) = struct
  type litterals = {
    str_lit_map: (string, Util.stringlit_label) Hashtbl.t;
    float_lit_map: (float, Util.floatlit_label) Hashtbl.t
  }
  type raw_line = InstructionLine.raw_line
  type asm_function_decl = { asm_name : string; asm_body : raw_line list }

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
    litterals: litterals
  }

  type asm_program = named_asm_module_path list
end

module NamingConvention = NamingConvention
