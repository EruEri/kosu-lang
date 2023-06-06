(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of San: A 3 address code language/compiler                               *)
(* Copyright (C) 2023 Yves Ndiaye                                                             *)
(*                                                                                            *)
(* San is free software: you can redistribute it and/or modify it under the terms             *)
(* of the GNU General Public License as published by the Free Software Foundation,            *)
(* either version 3 of the License, or (at your option) any later version.                    *)
(*                                                                                            *)
(* San is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;           *)
(* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR           *)
(* PURPOSE.  See the GNU General Public License for more details.                             *)
(* You should have received a copy of the GNU General Public License along with San.          *)
(* If not, see <http://www.gnu.org/licenses/>.                                                *)
(*                                                                                            *)
(**********************************************************************************************)

module AsmAst = AsmAst
module NamingConvention = NamingConvention


module SanRegisterAllocator = CfgBase.SanRegisterAllocator
module SanVariableMap = CfgBase.SanVariableMap
module Cfg_Sig = CfgBase.Cfg_Sig
module CfgPprint = CfgBase.CfgPprint
module CfgConv = CfgBase.Conv


module Sizeof = struct
  let align n b =
    let m = n mod b in
    if m = 0 then n else n + b - m

  let align_16 b = align b 16

  let sizeof = function
    | (Ssize : SanTyped.SanTyAst.san_type) | Stringl -> Nativeint.size / 8
    | Unit | Boolean -> 1

  let alignmentof = sizeof

  let sizeof_tuple san_types =
    let size, alignment =
      san_types
      |> List.fold_left
           (fun (acc_size, acc_align) st ->
             let comming_size = sizeof st in
             let comming_align = alignmentof st in

             let aligned = align acc_size comming_align in
             let new_align = max acc_align comming_align in
             (aligned + comming_size, new_align))
           (0, 1)
    in
    align size alignment

  let offset_of_tuple_index index san_types =
    if index = 0 then 0
    else
      san_types
      |> List.mapi (fun i v -> (i - 1, v))
      |> List.fold_left
           (fun ((acc_size, acc_align, found) as acc) (tindex, st) ->
             let comming_size = sizeof st in
             let comming_align = alignmentof st in

             let aligned = align acc_size comming_align in
             let new_align = max acc_align comming_align in

<<<<<<< HEAD
             if found then acc
             else if index = tindex then (aligned, new_align, true)
             else (aligned + comming_size, new_align, found))
           (0, 1, false)
      |> function
      | a, _, _ -> a
end
=======
          if found then acc
          else if index = tindex then (aligned, new_align, true)
          else (aligned + comming_size, new_align, found))
        (0, 1, false)
    |> function
    | a, _, _ -> a
end

module Args = struct
  type passed_kind = 
  | Other
  | Float

  type passing_style = 
  | Simple_Reg of passed_kind
  | Double_Reg of passed_kind * passed_kind

  type ('a, 'b) return_kind = 
  | Simple_return of 'a
  | Double_return of ('a * 'b)

  let simple_return a = Simple_return a

  let double_return a b = Double_return (a, b)

  let pop_opt = function
  | [] -> None, []
  | t::q -> Some t, q

  let double_pop_opt = function
  | ([] | _::[] as l) -> None, l
  | t::t2::q -> Some (t, t2), q

  let rec consume_args ~fregs ~iregs ~fargs ~iargs ~stacks_args ~fpstyle ttes = match ttes with
  | [] -> List.rev iargs, List.rev fargs, List.rev stacks_args
  | t::q -> 
    begin match fpstyle t with
    | Simple_Reg kind -> begin match kind with
      | Other -> 
        let head_reg, remain_reg = pop_opt iregs in
        begin match head_reg with
        | None -> consume_args ~fregs ~iregs ~fargs ~iargs ~fpstyle ~stacks_args:(t::stacks_args) q
        | Some reg -> consume_args ~fregs ~iregs:remain_reg ~fargs ~iargs:((t, simple_return reg)::iargs) ~stacks_args ~fpstyle q
        end
      | Float -> 
        let head_reg, freg_remain = pop_opt fregs in
        begin match head_reg with
        | None -> consume_args ~fregs ~iregs ~fargs ~iargs ~stacks_args:(t::stacks_args) ~fpstyle q
        | Some reg -> consume_args ~fregs:freg_remain ~iregs ~fargs:((t, simple_return reg)::fargs) ~iargs ~stacks_args ~fpstyle q
        end
    end
    | Double_Reg (lhs, rhs) -> begin match lhs, rhs with  
      | Float, Float -> begin match fregs with
      | [] | _::[] -> consume_args ~fregs ~iregs ~fargs ~iargs ~fpstyle ~stacks_args:(t::stacks_args) q
      | fr1::fr2::remains -> 
        let arg = t, double_return fr1 fr2 in
        consume_args ~fregs:remains ~iregs ~fargs:(arg::fargs) ~iargs ~fpstyle ~stacks_args:(t::stacks_args) q
      end 
      | _ -> begin match fregs with
        | [] | _::[] -> consume_args ~fregs ~iregs ~fargs ~iargs ~fpstyle ~stacks_args:(t::stacks_args) q
        | ir1::ir2::remains -> 
          let arg = t, double_return ir1 ir2 in
          consume_args ~fregs ~iregs:remains ~fargs ~iargs:(arg::iargs) ~fpstyle ~stacks_args:(t::stacks_args) q
    end
    end
  end

  let consume_args = consume_args ~fargs:[] ~iargs:[] ~stacks_args:[]
end
>>>>>>> 4dfa4e5 ([San]: fix compilation)
