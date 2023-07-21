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

open BytecodeAsCore.AsInstruction
open BytecodeAsCore.AsLine
module BcPp = BytecodeCompiler.Pprint
open Printf

let string_of_address = BcPp.string_of_address
let string_of_register = BcPp.string_of_register
let string_of_src = BcPp.string_of_src
let string_of_condition_code = BcPp.string_of_condition_code
let string_of_single_operande = BcPp.string_of_single_operande
let string_of_shift = BcPp.string_of_shift
let string_of_double_operande = BcPp.string_of_double_operande
let string_of_instruction_format = BcPp.string_of_instruction_format
let string_of_data_size = BcPp.string_of_data_size

let string_of_map map =
  map |> BytecodeAsCore.PcRelatifMap.bindings
  |> List.map (fun (label, pc) ->
         Printf.sprintf "symbole: %s -> pc = %Ld" label pc
     )
  |> String.concat ", "

let string_of_pc_info
    ({ pc; local_map; global_map } : BytecodeAsCore.PcInfo.pc_info) =
  Printf.sprintf "{\n    pc = %Ld;\n    local = [%s];\n    global = [%s]\n}" pc
    (string_of_map local_map) (string_of_map global_map)

let string_of_lea_instruction = function
  | BaLeaPcRel int64 ->
      sprintf "%Ld" int64
  | BaLeaRegAbs add ->
      string_of_address add

let string_of_jump_src = function
  | `PcRel int64 ->
      sprintf "%Ld" int64
  | `Register reg ->
      string_of_register reg

let string_of_as_instruction = function
  | AsHalt ->
      "halt"
  | AsRet ->
      "ret"
  | AsSyscall ->
      "syscall"
  | AsCCall src ->
      Printf.sprintf "ccall %s" (string_of_src src)
  | AsMvnt so ->
      sprintf "mvnt %s" (string_of_single_operande so)
  | AsMvng so ->
      sprintf "mvng %s" (string_of_single_operande so)
  | AsMv so ->
      sprintf "mv %s" (string_of_single_operande so)
  | AsMva so ->
      sprintf "mva %s, %s"
        (string_of_single_operande so.operandes)
        (string_of_shift so.shift)
  | AsJump jump ->
      string_of_instruction_format "jump" string_of_jump_src jump
  | AsBr br ->
      string_of_instruction_format "br" string_of_jump_src br
  | AsLea { destination; operande } ->
      sprintf "lea %s, %s"
        (string_of_register destination)
        (string_of_lea_instruction operande)
  | AsAdd bin_op_operande ->
      string_of_instruction_format "add" string_of_double_operande
        bin_op_operande
  | AsSub bin_op_operande ->
      string_of_instruction_format "sub" string_of_double_operande
        bin_op_operande
  | AsMult bin_op_operande ->
      string_of_instruction_format "mult" string_of_double_operande
        bin_op_operande
  | AsDiv { operandes; signed } ->
      let si =
        if signed then
          "div"
        else
          "udiv"
      in
      string_of_instruction_format si string_of_double_operande operandes
  | AsMod { operandes; signed } ->
      let si =
        if signed then
          "mod"
        else
          "umod"
      in
      string_of_instruction_format si string_of_double_operande operandes
  | AsAnd bin_op_operande ->
      string_of_instruction_format "and" string_of_double_operande
        bin_op_operande
  | AsOr bin_op_operande ->
      string_of_instruction_format "or" string_of_double_operande
        bin_op_operande
  | AsXor bin_op_operande ->
      string_of_instruction_format "xor" string_of_double_operande
        bin_op_operande
  | AsLsl bin_op_operande ->
      string_of_instruction_format "lsl" string_of_double_operande
        bin_op_operande
  | AsAsr bin_op_operande ->
      string_of_instruction_format "asr" string_of_double_operande
        bin_op_operande
  | AsLsr bin_op_operande ->
      string_of_instruction_format "lsr" string_of_double_operande
        bin_op_operande
  | AsCmp { cc; lhs; rhs } ->
      sprintf "cmp%s, %s, %s"
        (string_of_condition_code cc)
        (string_of_register lhs) (string_of_register rhs)
  | AsCset { cc; destination; lhs; rhs; update_last_cmp } ->
      sprintf "cset%s%s, %s, %s, %s"
        ( if update_last_cmp then
            "u"
          else
            ""
        )
        (string_of_condition_code cc)
        (string_of_register destination)
        (string_of_register lhs) (string_of_register rhs)
  | AsLdr { data_size; destination; address } ->
      sprintf "ldr%s %s, %s"
        (string_of_data_size data_size)
        (string_of_register destination)
        (string_of_address address)
  | AsStr { data_size; destination; address } ->
      sprintf "str%s %s, %s"
        (string_of_data_size data_size)
        (string_of_register destination)
        (string_of_address address)
  | AsItof { data_size; destination; source; signed } ->
      sprintf "%sitof.%s %s %s"
        ( if signed then
            ""
          else
            "u"
        )
        (string_of_data_size data_size)
        (string_of_register destination)
        (string_of_register source)
  | AsFtoi { data_size; destination; source; signed } ->
      sprintf "%sftoi.%s %s %s"
        ( if signed then
            ""
          else
            "u"
        )
        (string_of_data_size data_size)
        (string_of_register destination)
        (string_of_register source)

let string_of_as_line = function
  | AsInstruction i ->
      sprintf "\t%s" (string_of_as_instruction i)
  | AsComment s ->
      sprintf "\t// %s" s
  | AsLabel s ->
      sprintf "%s:" s

let string_of_as_function
    ({ as_name; as_body } : BytecodeAsCore.AsNode.as_function_decl) =
  sprintf "%s:\n%s" as_name
    (as_body |> List.map string_of_as_line |> String.concat "\n")

let string_of_asm_node = function
  | BytecodeAsCore.AsNode.AsFunction f ->
      string_of_as_function f
  | AsConst _c ->
      failwith "TODO: string_of_asm_node Const"
  | AsStringLitteral { name; value } ->
      Printf.sprintf "%s:\n\t \"%s\"\n\n" name value
  | AsFloatLitteral { fname; fvalue } ->
      Printf.sprintf "%s:\n\t %LX\n\n" fname (Int64.bits_of_float @@ snd fvalue)
