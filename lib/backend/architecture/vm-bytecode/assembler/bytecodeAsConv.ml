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

open BytecodeAsCore
open BytecodeAsCore.PcInfo
module ByteInstruction = BytecodeCompiler.Instruction
module AsInstruction = BytecodeAsCore.AsInstruction
module ConditionCode = BytecodeCompiler.ConditionCode
module ByteProgram = BytecodeCompiler.BytecodeProgram
module ByteLine = BytecodeCompiler.Line

let ( ++ ) = Int64.add
let ( -- ) = Int64.sub
let ( !+ ) = ( ++ ) 1L
let ( // ) = Int64.unsigned_div

let find_symbol symbole (pc : pc_info) =
  let absolue_pc =
    match PcRelatifMap.find_opt symbole pc.global_map with
    | Some pc ->
        pc
    | None ->
        PcRelatifMap.find symbole pc.local_map
  in
  (* let () = Out_channel.with_open_bin "debug.txt" (fun oc ->
       let () = Printf.fprintf oc "%s\n\n" @@ string_of_pc_info pc in
       Printf.fprintf oc "symbole = %s, abspc = %Ld, pc = %Ld\n" symbole absolue_pc pc.pc
     )
     in *)
  absolue_pc -- pc.pc

let as_instruction_of_bytecode_instructions info =
  let open ByteInstruction in
  let open AsInstruction in
  let open ConditionCode in
  function
  | Halt ->
      AsHalt
  | Ret ->
      AsRet
  | Syscall ->
      AsSyscall
  | CCall src ->
      AsCCall src
  | Mvnt single_operande ->
      AsMvnt single_operande
  | Mvng single_operande ->
      AsMvng single_operande
  | Mv single_operande ->
      AsMv single_operande
  | Mva { operandes; shift } ->
      AsMva { operandes; shift }
  | Jump jump_src ->
      let src =
        match jump_src with
        | `Register _ as e ->
            e
        | `Label s ->
            `PcRel (find_symbol s info)
      in
      AsJump src
  | Br jump_src ->
      let src =
        match jump_src with
        | `Register _ as e ->
            e
        | `Label s ->
            `PcRel (find_symbol s info)
      in
      AsBr src
  | Lea { destination; operande } ->
      let src =
        match operande with
        | LeaPcRel s ->
            BaLeaPcRel (find_symbol s info)
        | LeaRegAbs address ->
            BaLeaRegAbs address
      in
      AsLea { destination; operande = src }
  | Add bin_op_operande ->
      AsAdd bin_op_operande
  | Sub bin_op_operande ->
      AsSub bin_op_operande
  | Mult bin_op_operande ->
      AsMult bin_op_operande
  | Div { operandes : bin_op_operande; signed : bool } ->
      AsDiv { operandes; signed }
  | Mod { operandes; signed } ->
      AsMod { operandes : bin_op_operande; signed : bool }
  | And bin_op_operande ->
      AsAnd bin_op_operande
  | Or bin_op_operande ->
      AsOr bin_op_operande
  | Xor bin_op_operande ->
      AsXor bin_op_operande
  | Lsl bin_op_operande ->
      AsLsl bin_op_operande
  | Asr bin_op_operande ->
      AsAsr bin_op_operande
  | Lsr bin_op_operande ->
      AsLsr bin_op_operande
  | Cmp
      { cc : condition_code; lhs : Register.register; rhs : Register.register }
    ->
      AsCmp { cc; lhs; rhs }
  | Cset
      {
        cc : condition_code;
        destination : Register.register;
        lhs : Register.register;
        rhs : Register.register;
        update_last_cmp : bool;
      } ->
      AsCset { cc; destination; lhs; rhs; update_last_cmp }
  | Ldr
      {
        data_size : data_size;
        destination : Register.register;
        address : address;
      } ->
      AsLdr { data_size; destination; address }
  | Str
      {
        data_size : data_size;
        destination : Register.register;
        address : address;
      } ->
      AsStr { data_size; destination; address }
  | Itof
      {
        data_size : data_size;
        destination : Register.register;
        source : Register.register;
        signed : bool;
      } ->
      AsItof { data_size; destination; source; signed }
  | Ftoi
      {
        data_size : data_size;
        destination : Register.register;
        source : Register.register;
        signed : bool;
      } ->
      AsFtoi { data_size; destination; source; signed }

let next_pc info = { info with pc = !+(info.pc) }
let incr_pc n info = { info with pc = info.pc ++ n }

let add_local_map elt info =
  { info with local_map = PcRelatifMap.add elt info.pc info.local_map }

let add_global_map elt info =
  { info with global_map = PcRelatifMap.add elt info.pc info.global_map }

let pc_relatif_line info (ByteLine.AsmLine (raw_line, _)) =
  match raw_line with
  | Instruction _ ->
      next_pc info
  | Comment _ ->
      info
  | Label s ->
      add_local_map s info

let pc_relatif_map info = function
  | ByteProgram.AFloat_Litteral { fvalue = _; fname } ->
      info |> add_local_map fname |> incr_pc 2L
  | AStringLitteral { name; value } ->
      let stringlen = String.length value in
      let next = (KosuIrTyped.Sizeof.align_4 @@ Int64.of_int stringlen) // 4L in
      info |> add_local_map name |> incr_pc next
  | AConst { asm_const_name; value } -> (
      match value with
      | `IntVal (_, _) ->
          (* vm will store all number as 64 bits *)
          info |> add_global_map asm_const_name |> incr_pc 2L
      | `StrVal string ->
          let stringlen = String.length string in
          let next =
            (KosuIrTyped.Sizeof.align_4 @@ Int64.of_int stringlen) // 4L
          in
          info |> add_global_map asm_const_name |> incr_pc next
    )
  | Afunction { asm_name; asm_body } ->
      let info = add_global_map asm_name info in
      List.fold_left pc_relatif_line info asm_body

let pc_relatif pc (asm_module_node_list : ByteProgram.asm_module_node list) =
  let local_map = PcRelatifMap.empty in
  let global_map = PcRelatifMap.empty in
  let info = { pc; local_map; global_map } in
  asm_module_node_list |> List.fold_left pc_relatif_map info

(** [global_map asm_program] merge all the pc absolue value og the global symbol in [asm_program]  *)
let global_map asm_program =
  let open ByteProgram in
  asm_program
  |> List.fold_left
       (fun (pc, map)
            { asm_module_path = { asm_module = AsmModule nodes; _ }; _ } ->
         let { global_map; pc = new_pc; _ } = pc_relatif pc nodes in
         let map_union _key _lmvalue _rmvalue =
           failwith "Should'nt have key confllict"
         in
         let map = PcRelatifMap.union map_union global_map map in
         (new_pc, map)
       )
       (0L, PcRelatifMap.empty)

let asline_of_line info (ByteLine.AsmLine (raw_line, _)) =
  let open AsLine in
  match raw_line with
  | Instruction i ->
      ( next_pc info,
        AsInstruction (as_instruction_of_bytecode_instructions info i)
      )
  | Comment c ->
      (info, AsComment c)
  | Label s ->
      (info, AsLabel s)

let as_node_of_asm_node info =
  let open AsNode in
  function
  | ByteProgram.AFloat_Litteral { fvalue; fname } ->
      (incr_pc 2L info, AsFloatLitteral { fname; fvalue })
  | AStringLitteral { name; value } ->
      let stringlen = String.length value in
      let next = (KosuIrTyped.Sizeof.align_4 @@ Int64.of_int stringlen) // 4L in
      (incr_pc next info, AsStringLitteral { name; value })
  | AConst ({ asm_const_name = _; value } as c) -> (
      match value with
      | `IntVal (_, _) ->
          (* vm will store all number as 64 bits *)
          (incr_pc 2L info, AsConst c)
      | `StrVal string ->
          let stringlen = String.length string in
          let next =
            (KosuIrTyped.Sizeof.align_4 @@ Int64.of_int stringlen) // 4L
          in
          (incr_pc next info, AsConst c)
    )
  | Afunction { asm_name; asm_body } ->
      let info, lines =
        asm_body
        |> List.fold_left
             (fun (info, acc_line) asm_line ->
               let next_info, line = asline_of_line info asm_line in
               (next_info, line :: acc_line)
             )
             (info, [])
      in
      let as_function =
        AsFunction { as_name = asm_name; as_body = List.rev lines }
      in
      (info, as_function)

let nodes_of_asm_program asm_program =
  let open ByteProgram in
  let abs_global_map = snd @@ global_map asm_program in
  let _, prog =
    asm_program
    |> List.fold_left
         (fun (pc, previous_nodes)
              { asm_module_path = { asm_module = AsmModule nodes; _ }; _ } ->
           let info = pc_relatif pc nodes in
           let info = { info with global_map = abs_global_map; pc } in
           let info, lines =
             nodes
             |> List.fold_left
                  (fun (info, acc_line) node ->
                    let next_info, lines = as_node_of_asm_node info node in
                    (next_info, lines :: acc_line)
                  )
                  (info, [])
           in
           let nodes = List.rev lines in
           (info.pc, previous_nodes @ nodes)
         )
         (0L, [])
  in
  let pc_main =
    PcRelatifMap.find BytecodeCompiler.NamingConvention.main abs_global_map
  in
  (pc_main, prog)
