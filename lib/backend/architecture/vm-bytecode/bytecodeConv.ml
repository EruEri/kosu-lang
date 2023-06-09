(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of Kosu                                                                  *)
(* Copyright (C) 2023 Yves Ndiaye                                                             *)
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


open Util
open BytecodeCore
open BytecodeCore.LineInstruction
open BytecodeCore.Register
open BytecodeCore.BytecodeProgram
open BytecodeCore.Location
open KosuIrTAC.Asttachelper.StringLitteral
open KosuIrTAC.Asttac

let translate_tac_expression ~litterals ~target_reg fd tte = 
  match tte.tac_expression with
  | TEString s -> 
    let (SLit str_labl) = Hashtbl.find litterals.str_lit_map s in
    lea_label target_reg str_labl 
  | TEFalse 
  | TECmpLesser 
  | TEmpty 
  | TENullptr  -> mv_integer target_reg 0L 
  | TETrue | TECmpEqual -> mv_integer target_reg 1L
  | TECmpGreater -> mv_integer target_reg 2L
  | TEInt (_, _, int64) ->
    mv_integer target_reg int64
  | TEChar c ->
    let int_repr = Int64.of_int @@ Char.code c in
    mv_integer target_reg int_repr
  | TEFloat float ->
    let (FLit float_label) = Hashtbl.find litterals.float_lit_map float in
    let bits_repr = Int64.bits_of_float @@ snd float in
    mv_integer target_reg bits_repr
  | TEIdentifier id -> 
    let loc = 
      match FrameManager.location_of (id, tte.expr_rktype) fd with
      | None -> failwith "tte identifier setup null address"
      | Some loc -> loc
    in
    begin match loc with
    | LocReg reg -> 
      if reg = target_reg then []
      else smv target_reg @@ Operande.iregister reg
    | LocAddr address -> 
      if Register.does_return_hold_in_register_kt tte.expr_rktype then
        let ds = ConditionCode.data_size_of_kt tte.expr_rktype in
        sldr ds target_reg address
      else
        sadd target_reg address.base address.offset
    end
  | TESizeof kt -> 
    let size = KosuIrTyped.Sizeof.sizeof_kt kt in
    mv_integer target_reg size
  | TEConst {module_path; name} when tte.expr_rktype = RTString_lit -> 
    lea_label target_reg ~module_path name 
  | TEConst _ -> failwith "Other constant todo"


let translate_and_store ~where  ~litterals ~target_reg fd tte = 
 match where with
  | Some LocReg reg ->
    translate_tac_expression ~litterals ~target_reg:reg fd tte
  | Some LocAddr address -> 
    let target_reg = Register.r13 in
    let insts = translate_tac_expression ~litterals ~target_reg fd tte in
    let cp_insts = scopy target_reg address tte.expr_rktype in
    insts @ cp_insts
    | None -> 
      let target_reg = Register.r13 in
      let insts = translate_tac_expression ~litterals ~target_reg fd tte in
      insts

let translate_tac_rvalue ?is_deref ~litterals ~(where: location option) current_module rprogram 
  (fd : FrameManager.description) rvalue = 
  match rvalue.rvalue with
  | RVExpression tte -> translate_and_store ~where ~litterals ~target_reg:Register.r13 fd tte
  | RVStruct {fields; module_path = _; struct_name} -> 
    let struct_decl =
      match
        KosuIrTyped.Asttyhelper.RProgram.find_type_decl_from_rktye
          rvalue.rval_rktype rprogram
      with
      | Some (RDecl_Struct s) -> s
      | Some (RDecl_Enum _) ->
          failwith "Expected to find a struct get an enum"
      | None -> failwith "Non type decl ??? my validation is very weak"
    in
    
    let generics =
      rvalue.rval_rktype
      |> KosuIrTyped.Asttyhelper.RType.extract_parametrics_rktype
      |> List.combine struct_decl.generics
      |> List.to_seq |> Hashtbl.of_seq
    in
    let offset_list =
      fields
      |> List.map (fun (field, _) ->
             KosuIrTyped.Sizeof.offset_of_field ~generics field struct_decl rprogram)
    in

    fields
    |> List.mapi Util.couple
    |> List.fold_left (fun acc (index, (_field, tte)) -> 
      let where = where |> Option.map (function
        | LocAddr address -> LocAddr ( increment_adress (List.nth offset_list index) address )
        | LocReg _ as loc -> loc   
      ) in
      acc @ translate_and_store ~where ~litterals ~target_reg:Register.r13 fd tte
    ) []
  | RVFunction { module_path; fn_name; generics_resolver = _; tac_parameters } -> begin
    let typed_parameters =
      tac_parameters |> List.map (fun { expr_rktype; _ } -> expr_rktype)
    in
    let fn_module =
      if module_path = "" then current_module else module_path
    in
    let fn_decl =
      Option.get @@ KosuIrTyped.Asttyhelper.RProgram.find_function_decl_of_name fn_module
        fn_name rprogram
      
    in
    match fn_decl with
    | RExternal_Decl external_func_decl ->
      let fn_label =
        NamingConvention.label_of_external_function external_func_decl
      in
    failwith ""
  end 
  | _ -> failwith "TODO"