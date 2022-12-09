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

type raw_function = 
| RFFunction of rtrue_function_decl
| RFOperator of roperator_decl

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

module Generics = struct
  let rec instanciate_generics_type generics = function
| RTType_Identifier { module_path = ""; name } as t -> begin 
  match generics |> List.assoc_opt name with
  | None -> t
  | Some kt -> kt
end
| RTParametric_identifier {module_path; parametrics_type; name} -> 
  RTParametric_identifier {
    module_path; 
    parametrics_type = parametrics_type |> List.map (instanciate_generics_type generics);
    name
  }
| RTPointer ptr -> RTPointer (instanciate_generics_type generics ptr)
| RTTuple rtks -> RTTuple ( rtks |> List.map (instanciate_generics_type generics) )
| _ as t -> t

let rec instanciate_generics_kbody generics (rkstatements, return_te) = 
  ( rkstatements |> List.map (instanciate_generics_statement generics),
  instanciate_generics_typed_expression generics return_te
  )
and instanciate_generics_statement generics = function
| RSDeclaration {is_const; variable_name; typed_expression} -> RSDeclaration {is_const; variable_name; typed_expression = instanciate_generics_typed_expression generics typed_expression}
| RSAffection (s, te) -> RSAffection (s, instanciate_generics_typed_expression generics te)
| RSDiscard te -> RSDiscard (instanciate_generics_typed_expression generics te)
| RSDerefAffectation (s, te) -> RSDerefAffectation (s, instanciate_generics_typed_expression generics te)

and instanciate_generics_rexpression generics = function
| RESizeof ktype -> RESizeof (instanciate_generics_type generics ktype)
| REFieldAcces {first_expr; field} -> REFieldAcces {first_expr = instanciate_generics_typed_expression generics first_expr; field}
| REStruct {modules_path; struct_name; fields} -> REStruct {
  modules_path; 
  struct_name; 
  fields = fields 
    |> List.map (fun (s, te) -> 
      s, instanciate_generics_typed_expression generics te
    )
}
| REEnum { modules_path; enum_name; variant; assoc_exprs} -> REEnum {
  modules_path;
  enum_name;
  variant;
  assoc_exprs = assoc_exprs |> List.map (instanciate_generics_typed_expression generics)
}
| RETuple tes -> RETuple (tes |> List.map  (instanciate_generics_typed_expression generics))
| REBuiltin_Function_call {fn_name; parameters} -> REBuiltin_Function_call {fn_name; parameters = parameters |> List.map (instanciate_generics_typed_expression generics)}
| REFunction_call {modules_path; generics_resolver; fn_name; parameters} -> REFunction_call {
  modules_path;
  generics_resolver = generics_resolver |> Option.map (List.map (instanciate_generics_type generics));
  fn_name;
  parameters = parameters |> List.map (instanciate_generics_typed_expression generics)
}
| REBinOperator_Function_call bo -> REBinOperator_Function_call (instanciate_generics_binary_op generics bo)
| REUnOperator_Function_call up -> REUnOperator_Function_call (instanciate_generics_unary_op generics up)
| REBin_op bo -> REBin_op (instanciate_generics_binary_op generics bo)
| REUn_op up -> REUn_op (instanciate_generics_unary_op generics up)
| REIf (te, ifbody, elsebody) -> REIf (
    instanciate_generics_typed_expression generics te, 
    instanciate_generics_kbody generics ifbody,
    instanciate_generics_kbody generics elsebody
    )
| RESwitch {rexpression; cases; wildcard_case} -> 
  RESwitch {
    rexpression = instanciate_generics_typed_expression generics rexpression;
    cases = cases |> List.map (fun (rswichs_list, info_list, rbody) -> 
      rswichs_list,
      info_list |> List.map (fun (i, s, ktype) -> (i, s, instanciate_generics_type generics ktype)),
      instanciate_generics_kbody generics rbody
      );
    wildcard_case = wildcard_case |> Option.map (instanciate_generics_kbody generics)
  }
| _ as t -> t
and instanciate_generics_typed_expression generics typed_expr = {
  rktype = instanciate_generics_type generics typed_expr.rktype;
  rexpression = instanciate_generics_rexpression generics typed_expr.rexpression
}
and instanciate_generics_unary_op generics = function
| RUMinus typed_expression -> RUMinus (instanciate_generics_typed_expression generics typed_expression)
| RUNot te -> RUNot (instanciate_generics_typed_expression generics te)
and instanciate_generics_binary_op generics = function
| RBAdd (lhs, rhs) -> RBAdd (instanciate_generics_typed_expression generics lhs, instanciate_generics_typed_expression generics rhs)
| RBMinus (lhs, rhs) -> RBMinus (instanciate_generics_typed_expression generics lhs, instanciate_generics_typed_expression generics rhs)
| RBMult (lhs, rhs) -> RBMult (instanciate_generics_typed_expression generics lhs, instanciate_generics_typed_expression generics rhs)
| RBDiv (lhs, rhs) -> RBDiv (instanciate_generics_typed_expression generics lhs, instanciate_generics_typed_expression generics rhs)
| RBMod (lhs, rhs) -> RBMod (instanciate_generics_typed_expression generics lhs, instanciate_generics_typed_expression generics rhs)
| RBBitwiseOr (lhs, rhs) -> RBBitwiseOr (instanciate_generics_typed_expression generics lhs, instanciate_generics_typed_expression generics rhs)
| RBBitwiseAnd (lhs, rhs) -> RBBitwiseAnd (instanciate_generics_typed_expression generics lhs, instanciate_generics_typed_expression generics rhs)
| RBBitwiseXor (lhs, rhs) -> RBBitwiseXor (instanciate_generics_typed_expression generics lhs, instanciate_generics_typed_expression generics rhs)
| RBShiftLeft (lhs, rhs) -> RBShiftLeft (instanciate_generics_typed_expression generics lhs, instanciate_generics_typed_expression generics rhs)
| RBShiftRight (lhs, rhs) -> RBShiftRight (instanciate_generics_typed_expression generics lhs, instanciate_generics_typed_expression generics rhs)
| RBAnd (lhs, rhs) -> RBAdd (instanciate_generics_typed_expression generics lhs, instanciate_generics_typed_expression generics rhs)
| RBOr (lhs, rhs) -> RBOr (instanciate_generics_typed_expression generics lhs, instanciate_generics_typed_expression generics rhs)
| RBSup (lhs, rhs) -> RBSup (instanciate_generics_typed_expression generics lhs, instanciate_generics_typed_expression generics rhs)
| RBSupEq (lhs, rhs) -> RBSupEq (instanciate_generics_typed_expression generics lhs, instanciate_generics_typed_expression generics rhs)
| RBInf (lhs, rhs) -> RBInf (instanciate_generics_typed_expression generics lhs, instanciate_generics_typed_expression generics rhs)
| RBInfEq (lhs, rhs) -> RBInfEq (instanciate_generics_typed_expression generics lhs, instanciate_generics_typed_expression generics rhs)
| RBEqual (lhs, rhs) -> RBEqual (instanciate_generics_typed_expression generics lhs, instanciate_generics_typed_expression generics rhs)
| RBDif (lhs, rhs) -> RBDif (instanciate_generics_typed_expression generics lhs, instanciate_generics_typed_expression generics rhs)
end

module Function = struct
  
  let true_function_of_rfunction_decl generics (rfunction_decl: rfunction_decl): rtrue_function_decl = let open Generics in
  match rfunction_decl.generics = [] with
  | true -> {rfn_name = rfunction_decl.rfn_name; rparameters = rfunction_decl.rparameters; return_type = rfunction_decl.return_type; rbody = rfunction_decl.rbody}
  | false -> 
    let assoc_generics = List.combine rfunction_decl.generics generics in
    let rparameters = rfunction_decl.rparameters |> List.map (fun (field, rtype) -> field,  instanciate_generics_type assoc_generics rtype) in
    let return_type = instanciate_generics_type assoc_generics rfunction_decl.return_type in
    let rbody = instanciate_generics_kbody assoc_generics rfunction_decl.rbody in
    {
      rfn_name = rfunction_decl.rfn_name;
      rparameters;
      return_type;
      rbody
    }

    let rec is_type_compatible_hashgen generic_table (init_type : rktype)
    (expected_type : rktype) (function_decl : rfunction_decl) =
  match (init_type, expected_type) with
  | kt, RTType_Identifier { module_path = ""; name }
    when match Hashtbl.find_opt generic_table name with
         | None ->
             if
               function_decl.generics
               |> List.mem name
             then
               let () =
                 Hashtbl.replace generic_table name
                   ( function_decl.generics
                     |> Util.ListHelper.index_of (( = ) name ),
                     kt )
               in
               true
             else false
         | Some (_, find_kt) -> find_kt = kt ->
      true
  | ( RTType_Identifier { module_path = init_path; name = init_name },
      RTType_Identifier { module_path = exp_path; name = exp_name } ) ->
      function_decl.generics |> List.mem exp_name
      || (init_path = exp_path && init_name = exp_name)
  | ( RTParametric_identifier
        {
          module_path = init_path;
          parametrics_type = init_pt;
          name = init_name;
        },
      RTParametric_identifier
        { module_path = exp_path; parametrics_type = exp_pt; name = exp_name }
    ) ->
      if
        init_path <> exp_path || init_name <> exp_name
        || List.compare_lengths init_pt exp_pt <> 0
      then false
      else
        List.combine init_pt exp_pt
        |> List.for_all (fun (i, e) ->
               is_type_compatible_hashgen generic_table i e function_decl)
  | RTUnknow, _ -> true
  | RTPointer _, RTPointer RTUnknow-> true
  | RTPointer lhs, RTPointer rhs ->
      is_type_compatible_hashgen generic_table lhs rhs function_decl
  | RTTuple lhs, RTTuple rhs ->
      Util.are_same_lenght lhs rhs
      && List.for_all2
           (fun lkt rkt ->
             is_type_compatible_hashgen generic_table lkt rkt
               function_decl)
           lhs rhs
  | lhs, rhs -> lhs = rhs
  
end

module Rtype_Decl = struct
  type type_decl = RDecl_Struct of rstruct_decl | RDecl_Enum of renum_decl

  let type_name = function
    | RDecl_Struct e -> e.rstruct_name
    | RDecl_Enum s -> s.renum_name
end

module Rmodule = struct
  open Rtype_Decl

  let retrieve_non_generics_function = function
  | RModule rnodes -> rnodes |> List.filter_map (fun rnode -> 
      match rnode with
      | RNFunction rfunction_decl when rfunction_decl.generics = [] -> Some (
          RFFunction {
            rfn_name = rfunction_decl.rfn_name; 
            rparameters = rfunction_decl.rparameters; 
            return_type = rfunction_decl.return_type; 
            rbody = rfunction_decl.rbody 
            }
          )
      | RNOperator roperator_decl -> Some (RFOperator roperator_decl)
      | _ -> None
    ) 

  let find_function_decl fn_name = function
  | RModule rnodes -> rnodes |> List.find_map (fun rnodes -> 
    match rnodes with
    | RNFunction rfunction_decl when rfunction_decl.rfn_name = fn_name -> Some rfunction_decl
    | _ -> None 
    )

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

  let rec specialise_generics_function current_module rprogram = function
  | REFunction_call {modules_path; fn_name; generics_resolver; parameters} -> 
    let function_module = if modules_path = "" then current_module else modules_path in
    let function_decl = rprogram 
    |> find_module_of_name function_module 
    |> Option.get
    |> Rmodule.find_function_decl fn_name
    |> Option.get in
    if function_decl.generics = [] then []
    else begin match generics_resolver with
    | None -> 
      let maped_type = Hashtbl.create (parameters |> List.length) in
      let inner_specialise = parameters |> List.combine function_decl.rparameters |> List.map ( fun ((_, type_decl), ({rktype; rexpression = _} as te)) -> 
        let _ = Function.is_type_compatible_hashgen maped_type rktype type_decl function_decl in
        specialise_generics_function_typed_expression current_module rprogram te 
        ) |> List.flatten in
      let type_list = maped_type |> Hashtbl.to_seq |> List.of_seq |> List.sort (fun (_ls, (lindex, _ltype))  (_rs, (rindex, _rtype)) -> compare lindex rindex) |> List.map (fun (_, (_, kt)) -> kt) in
        (function_module, Function.true_function_of_rfunction_decl type_list function_decl)::inner_specialise
    | Some generics ->  (function_module, Function.true_function_of_rfunction_decl generics function_decl)::[]
    end
  | REFieldAcces {first_expr; _ } -> specialise_generics_function_typed_expression current_module rprogram first_expr
  | REStruct { fields; _} -> fields |> List.map (fun (_, te) -> specialise_generics_function_typed_expression current_module rprogram te) |> List.flatten
  | REEnum {assoc_exprs = tes; _} | RETuple (tes) | REBuiltin_Function_call {parameters = tes; _} -> 
    tes |> List.map (specialise_generics_function_typed_expression current_module rprogram) |> List.flatten
  | REIf(te_cond, if_body, else_body) ->
    te_cond 
    |> specialise_generics_function_typed_expression current_module rprogram
    |> List.append ( if_body |> specialise_generics_function_kbody current_module rprogram)
    |> List.append ( else_body |> specialise_generics_function_kbody current_module rprogram)  
  | RECases {cases; else_case} ->
    cases |> List.map (fun (te, body) -> 
      te
      |> specialise_generics_function_typed_expression current_module rprogram
      |> List.append (body |> specialise_generics_function_kbody current_module rprogram)
    )
    |> List.flatten
    |> List.append (else_case |> specialise_generics_function_kbody current_module rprogram)
  | RESwitch { rexpression; cases; wildcard_case } -> 
    rexpression |> specialise_generics_function_typed_expression current_module rprogram
    |> List.append (
      cases |> List.map (fun (_, _, body) -> 
        (body |> specialise_generics_function_kbody current_module rprogram)
      ) |> List.flatten
    )
    |> List.append (wildcard_case |> Option.map (specialise_generics_function_kbody current_module rprogram) |> Option.to_list |> List.flatten)
  | REBinOperator_Function_call bin | REBin_op bin -> 
    let (lhs, rhs) =  Binop.operands bin in
    lhs 
    |> specialise_generics_function_typed_expression current_module rprogram
    |> List.append (rhs |> specialise_generics_function_typed_expression current_module rprogram)
  | REUnOperator_Function_call un | REUn_op un -> begin 
    match un with
    | RUMinus te | RUNot te -> specialise_generics_function_typed_expression current_module rprogram te
  end 
  | _ -> []
  and specialise_generics_function_typed_expression current_module rprogram typed_expression = 
    specialise_generics_function current_module rprogram typed_expression.rexpression
  and specialise_generics_function_statement current_module rprogram = function
  | RSDeclaration {typed_expression; _} -> specialise_generics_function_typed_expression current_module rprogram typed_expression
  | RSAffection (_, typed_expression) -> specialise_generics_function_typed_expression current_module rprogram typed_expression
  | RSDiscard typed_expression -> specialise_generics_function_typed_expression current_module rprogram typed_expression
  | RSDerefAffectation (_, typed_expression) -> specialise_generics_function_typed_expression current_module rprogram typed_expression
  and specialise_generics_function_kbody current_module rprogram (rkstatements, return_exprs) =
  rkstatements 
    |> List.map (specialise_generics_function_statement current_module rprogram) 
    |> List.flatten
    |> List.append ( return_exprs |> (specialise_generics_function_typed_expression current_module rprogram))
    
  (* let rec specialise_generics_function current_module (rkstatement, return) = 
    match rkstatement with
    | rstmt::q ->  *)
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
