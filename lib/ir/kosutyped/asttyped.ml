(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of Kosu                                                                  *)
(* Copyright (C) 2022 Yves Ndiaye                                                             *)
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

open KosuFrontend.Ast

type rswitch_case =
  | RSC_Enum_Identifier of { variant : string }
  | RSC_Enum_Identifier_Assoc of {
      variant : string;
      assoc_ids : string option list;
    }

type rktype =
  | RTParametric_identifier of {
      module_path : string;
      parametrics_type : rktype list;
      name : string;
    }
  | RTType_Identifier of { module_path : string; name : string }
  | RTInteger of (signedness * isize)
  | RTPointer of rktype
  | RTTuple of rktype list
  | RTFunction of rktype list * rktype
  | RTString_lit
  | RTUnknow
  | RTFloat
  | RTBool
  | RTUnit

type rkbody = rkstatement list * typed_expression
and typed_expression = { rktype : rktype; rexpression : rkexpression }

and rkstatement =
  | RSDeclaration of {
      is_const : bool;
      variable_name : string;
      typed_expression : typed_expression;
    }
  | RSAffection of string * typed_expression
  | RSDiscard of typed_expression
  | RSDerefAffectation of string * typed_expression

and rkexpression =
  | REmpty
  | RTrue
  | RFalse
  | RENullptr
  | REInteger of (signedness * isize * int64)
  | REFloat of float
  | RESizeof of rktype
  | REstring of string
  | REAdress of string
  | REDeference of int * string
  | REIdentifier of { modules_path : string; identifier : string }
  | REFieldAcces of { first_expr : typed_expression; field : string }
  | REConst_Identifier of { modules_path : string; identifier : string }
  | REStruct of {
      modules_path : string;
      struct_name : string;
      fields : (string * typed_expression) list;
    }
  | REEnum of {
      modules_path : string;
      enum_name : string option;
      variant : string;
      assoc_exprs : typed_expression list;
    }
  | RETuple of typed_expression list
  | REBuiltin_Function_call of {
      fn_name : string;
      parameters : typed_expression list;
    }
  | REFunction_call of {
      modules_path : string;
      generics_resolver : rktype list option;
      fn_name : string;
      parameters : typed_expression list;
    }
  | REBinOperator_Function_call of rkbin_op
  | REUnOperator_Function_call of rkunary_op
  | REIf of typed_expression * rkbody * rkbody
  | RECases of { cases : (typed_expression * rkbody) list; else_case : rkbody }
  | RESwitch of {
      rexpression : typed_expression;
      cases : (rswitch_case list * (int * string * rktype) list * rkbody) list;
      wildcard_case : rkbody option;
    }
  | REBin_op of rkbin_op
  | REUn_op of rkunary_op

and rkbin_op =
  | RBAdd of typed_expression * typed_expression
  | RBMinus of typed_expression * typed_expression
  | RBMult of typed_expression * typed_expression
  | RBDiv of typed_expression * typed_expression
  | RBMod of typed_expression * typed_expression
  | RBBitwiseOr of typed_expression * typed_expression
  | RBBitwiseAnd of typed_expression * typed_expression
  | RBBitwiseXor of typed_expression * typed_expression
  | RBShiftLeft of typed_expression * typed_expression
  | RBShiftRight of typed_expression * typed_expression
  | RBAnd of typed_expression * typed_expression
  | RBOr of typed_expression * typed_expression
  | RBSup of typed_expression * typed_expression
  | RBSupEq of typed_expression * typed_expression
  | RBInf of typed_expression * typed_expression
  | RBInfEq of typed_expression * typed_expression
  | RBEqual of typed_expression * typed_expression
  | RBDif of typed_expression * typed_expression

and rkunary_op = RUMinus of typed_expression | RUNot of typed_expression

type rstruct_decl = {
  rstruct_name : string;
  generics : string list;
  rfields : (string * rktype) list;
}

type renum_decl = {
  renum_name : string;
  generics : string list;
  rvariants : (string * rktype list) list;
}

type rfunction_decl = {
  rfn_name : string;
  generics : string list;
  rparameters : (string * rktype) list;
  return_type : rktype;
  rbody : rkbody;
}

type roperator_decl =
  | RUnary of {
      op : parser_unary_op;
      rfield : string * rktype;
      return_type : rktype;
      kbody : rkbody;
    }
  | RBinary of {
      op : parser_binary_op;
      rfields : (string * rktype) * (string * rktype);
      return_type : rktype;
      kbody : rkbody;
    }

type rsyscall_decl = {
  rsyscall_name : string;
  parameters : rktype list;
  return_type : rktype;
  opcode : int64;
}

type rexternal_func_decl = {
  rsig_name : string;
  fn_parameters : rktype list;
  return_type : rktype;
  is_variadic : bool;
  c_name : string option;
}

type rtrue_function_decl = {
  rfn_name: string;
  rparameters : (string * rktype) list;
  return_type : rktype;
  rbody : rkbody;
}

type rconst_decl = { rconst_name : string; value : typed_expression }

type rmodule_node =
  | RNExternFunc of rexternal_func_decl
  | RNFunction of rfunction_decl
  | RNOperator of roperator_decl
  | RNSyscall of rsyscall_decl
  | RNStruct of rstruct_decl
  | RNEnum of renum_decl
  | RNConst of rconst_decl

type rmodule = RModule of rmodule_node list
type rmodule_path = { path : string; rmodule : rmodule }
type named_rmodule_path = { filename : string; rmodule_path : rmodule_path }
type rprogram = named_rmodule_path list

module RType = struct
  let is_builtin_type = function
    | RTParametric_identifier _ | RTType_Identifier _ -> false
    | _ -> true

  let rec npointer n kt = if n <= 0 then kt else RTPointer (npointer (n - 1) kt)

  let rtpointee = function
    | RTPointer kt -> kt
    | _ -> failwith "Cannot access pointee type of a none pointer type"

  let module_path_opt = function
    | RTParametric_identifier { module_path; _ }
    | RTType_Identifier { module_path; _ } ->
        Some module_path
    | _ -> None

  let rtype_name_opt = function
    | RTParametric_identifier { name; _ } | RTType_Identifier { name; _ } ->
        Some name
    | _ -> None

  let is_builtin_rtype = function
    | RTParametric_identifier _ | RTType_Identifier _ -> false
    | _ -> true

  let rpointer p = RTPointer p
  let rtuple tuples = RTTuple tuples

  let extract_parametrics_rktype = function
    | RTParametric_identifier { parametrics_type; _ } -> parametrics_type
    | _ -> []

  let rec remap_generic_ktype generics_table rktype =
    match rktype with
    | RTType_Identifier { module_path = ""; name } -> (
        match Hashtbl.find_opt generics_table name with
        | None -> RTType_Identifier { module_path = ""; name }
        | Some rtyp -> rtyp)
    | RTType_Identifier { module_path = _; name } as t -> (
        match Hashtbl.find_opt generics_table name with
        | None -> t
        | Some typ -> typ)
    | RTParametric_identifier { module_path; parametrics_type; name } ->
        RTParametric_identifier
          {
            module_path;
            parametrics_type =
              parametrics_type |> List.map (remap_generic_ktype generics_table);
            name;
          }
    | RTTuple kts ->
        RTTuple (kts |> List.map (remap_generic_ktype generics_table))
    | RTPointer kt -> kt |> remap_generic_ktype generics_table |> rpointer
    | _ as kt -> kt
end

module RSwitch_Case = struct
  let variant = function
    | RSC_Enum_Identifier { variant } | RSC_Enum_Identifier_Assoc { variant; _ }
      ->
        variant
end

module Expression = struct
  let is_expresion_branch = function
    | RECases _ | RESwitch _ | REIf _ -> true
    | _ -> false

  let is_typed_expresion_branch { rexpression; _ } =
    is_expresion_branch rexpression
end

module Binop = struct
  let left_operande = function
    | RBAdd (lhs, _)
    | RBMinus (lhs, _)
    | RBMult (lhs, _)
    | RBDiv (lhs, _)
    | RBMod (lhs, _)
    | RBBitwiseOr (lhs, _)
    | RBBitwiseAnd (lhs, _)
    | RBBitwiseXor (lhs, _)
    | RBShiftLeft (lhs, _)
    | RBShiftRight (lhs, _)
    | RBAnd (lhs, _)
    | RBOr (lhs, _)
    | RBSup (lhs, _)
    | RBSupEq (lhs, _)
    | RBInf (lhs, _)
    | RBInfEq (lhs, _)
    | RBEqual (lhs, _)
    | RBDif (lhs, _) ->
        lhs

  let right_operand = function
    | RBAdd (_, rhs)
    | RBMinus (_, rhs)
    | RBMult (_, rhs)
    | RBDiv (_, rhs)
    | RBMod (_, rhs)
    | RBBitwiseOr (_, rhs)
    | RBBitwiseAnd (_, rhs)
    | RBBitwiseXor (_, rhs)
    | RBShiftLeft (_, rhs)
    | RBShiftRight (_, rhs)
    | RBAnd (_, rhs)
    | RBOr (_, rhs)
    | RBSup (_, rhs)
    | RBSupEq (_, rhs)
    | RBInf (_, rhs)
    | RBInfEq (_, rhs)
    | RBEqual (_, rhs)
    | RBDif (_, rhs) ->
        rhs

  let operands = function
    | RBAdd (lhs, rhs)
    | RBMinus (lhs, rhs)
    | RBMult (lhs, rhs)
    | RBDiv (lhs, rhs)
    | RBMod (lhs, rhs)
    | RBBitwiseOr (lhs, rhs)
    | RBBitwiseAnd (lhs, rhs)
    | RBBitwiseXor (lhs, rhs)
    | RBShiftLeft (lhs, rhs)
    | RBShiftRight (lhs, rhs)
    | RBAnd (lhs, rhs)
    | RBOr (lhs, rhs)
    | RBSup (lhs, rhs)
    | RBSupEq (lhs, rhs)
    | RBInf (lhs, rhs)
    | RBInfEq (lhs, rhs)
    | RBEqual (lhs, rhs)
    | RBDif (lhs, rhs) ->
        (lhs, rhs)
end

module Rtype_Decl = struct
  type type_decl = RDecl_Struct of rstruct_decl | RDecl_Enum of renum_decl

  let type_name = function
    | RDecl_Struct e -> e.rstruct_name
    | RDecl_Enum s -> s.renum_name
end

module Rmodule = struct
  open Rtype_Decl

  let retrieve_type_decl = function
    | RModule rmodule_nodes ->
        rmodule_nodes
        |> List.filter_map (fun node ->
               match node with
               | RNStruct struct_decl -> Some (RDecl_Struct struct_decl)
               | RNEnum enum_decl -> Some (RDecl_Enum enum_decl)
               | _ -> None)
end

module RProgram = struct
  let find_module_of_name module_name (rprogram : rprogram) =
    rprogram
    |> List.find_map (fun { filename = _; rmodule_path } ->
           if rmodule_path.path = module_name then Some rmodule_path.rmodule
           else None)

  let find_type_decl_from_rktye ktype rprogram =
    if RType.is_builtin_rtype ktype then None
    else
      let type_name = ktype |> RType.rtype_name_opt |> Option.get in
      let type_module_name = ktype |> RType.module_path_opt |> Option.get in
      let rmodule =
        rprogram |> find_module_of_name type_module_name |> Option.get
      in
      rmodule |> Rmodule.retrieve_type_decl
      |> List.find (fun rtype_decl ->
             type_name = (rtype_decl |> Rtype_Decl.type_name))
      |> Option.some
end

module Sizeof = struct
  let ( ++ ) = Int64.add
  let ( -- ) = Int64.sub

  let rec size calcul program rktype =
    match rktype with
    | RTUnit | RTBool | RTUnknow -> 1L
    | RTInteger (_, isize) -> Isize.size_of_isize isize / 8 |> Int64.of_int
    | RTFloat | RTPointer _ | RTString_lit | RTFunction _ -> 8L
    | RTTuple kts -> (
        kts |> function
        | list -> (
            let size, align, _packed_size =
              list
              |> List.fold_left
                   (fun (acc_size, acc_align, acc_packed_size) kt ->
                     let comming_size = kt |> size `size program in
                     let comming_align = kt |> size `align program in
                     let quotient = Int64.unsigned_div acc_size comming_align in
                     let reminder = Int64.unsigned_rem acc_size comming_align in
                     let new_pacced_size = comming_size ++ acc_packed_size in

                     let add_size =
                       if new_pacced_size < acc_size then 0L
                       else if comming_size < acc_align then acc_align
                       else comming_size
                     in

                     let padded_size =
                       if reminder = 0L || acc_size = 0L then acc_size
                       else Int64.mul comming_align (quotient ++ 1L)
                     in
                     ( padded_size ++ add_size,
                       max comming_align acc_align,
                       new_pacced_size ))
                   (0L, 0L, 0L)
            in
            match calcul with `size -> size | `align -> align))
    | kt -> (
        let type_decl =
          RProgram.find_type_decl_from_rktye kt program |> Option.get
        in

        match type_decl with
        | Rtype_Decl.RDecl_Enum enum_decl ->
            size_enum calcul program
              (kt |> RType.extract_parametrics_rktype
              |> List.combine enum_decl.generics
              |> List.to_seq |> Hashtbl.of_seq)
              enum_decl
        | Rtype_Decl.RDecl_Struct struct_decl ->
            size_struct calcul program
              (kt |> RType.extract_parametrics_rktype
              |> List.combine struct_decl.generics
              |> List.to_seq |> Hashtbl.of_seq)
              struct_decl)

  and size_struct calcul program generics struct_decl =
    struct_decl.rfields
    |> List.map (fun (_, kt) -> RType.remap_generic_ktype generics kt)
    |> function
    | list -> (
        let size, align, _packed_size =
          list
          |> List.fold_left
               (fun (acc_size, acc_align, acc_packed_size) kt ->
                 let comming_size = kt |> size `size program in
                 let comming_align = kt |> size `align program in
                 let quotient = Int64.unsigned_div acc_size comming_align in
                 let reminder = Int64.unsigned_rem acc_size comming_align in
                 let new_pacced_size = comming_size ++ acc_packed_size in

                 let add_size =
                   if new_pacced_size < acc_size then 0L
                   else if comming_size < acc_align then acc_align
                   else comming_size
                 in

                 let padded_size =
                   if reminder = 0L || acc_size = 0L then acc_size
                   else Int64.mul comming_align (quotient ++ 1L)
                 in
                 ( padded_size ++ add_size,
                   max comming_align acc_align,
                   new_pacced_size ))
               (0L, 0L, 0L)
        in
        match calcul with `size -> size | `align -> align)

  and size_enum calcul program generics enum_decl =
    enum_decl.rvariants
    |> List.map (fun (_, kts) ->
           kts
           |> List.map (RType.remap_generic_ktype generics)
           |> List.cons (RTInteger (Unsigned, I32))
           |> RType.rtuple |> size calcul program)
    |> List.fold_left max 0L

  let sizeof program ktype = size `size program ktype
  let alignmentof program ktype = size `align program ktype
end
