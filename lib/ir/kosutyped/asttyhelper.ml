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

open Asttyped

module RType = struct

  let is_unsigned_integer = function
  | RTInteger(sign, _) -> sign = Unsigned
  | _ -> false 
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

  let is_pointer = function
  | RTPointer _ -> true
  | _ -> false

  let extract_parametrics_rktype = function
    | RTParametric_identifier { parametrics_type; _ } -> parametrics_type
    | _ -> []

    (**
        
    *)
  let rec update_generics map init_type param_type () = 
    match init_type, param_type with
    | kt, RTType_Identifier {module_path = ""; name} -> begin
      match Hashtbl.find_opt map name with
      | Some t -> ( 
        match t with
        | RTUnknow -> 
          let () = Hashtbl.replace map name kt in
          () 
        | _ as _t -> ()
      )
      | None -> ()
    end
    | ( RTParametric_identifier
    { module_path = lmp; parametrics_type = lpt; name = lname },
  RTParametric_identifier
    { module_path = rmp; parametrics_type = rpt; name = rname } ) -> 
      if lmp <> rmp || lname <> rname || Util.are_diff_lenght lpt rpt then ()
      else List.iter2 (fun l r ->  update_generics map l r ()) lpt rpt
    | RTPointer lhs, RTPointer rhs -> 
       update_generics map lhs rhs ()
    | RTTuple lhs, RTTuple rhs -> 
      List.iter2 (fun l r ->  update_generics map l r ()) lhs rhs
    | _ -> ()

  let rec restrict_rktype to_restrict restrict =
    match (to_restrict, restrict) with
    | ( (RTParametric_identifier
            { module_path = lmp; parametrics_type = lpt; name = lname } as lhs),
        RTParametric_identifier
          { module_path = rmp; parametrics_type = rpt; name = rname } ) ->
        if lmp <> rmp || lname <> rname || Util.are_diff_lenght lpt rpt then lhs
        else
          RTParametric_identifier
            {
              module_path = lmp;
              parametrics_type = List.map2 restrict_rktype lpt rpt;
              name = rname;
            }
  
    | RTPointer lhs, RTPointer rhs -> RTPointer (restrict_rktype lhs rhs)
      (* 
      Dont remove the [| RTUnknow, rtk] branch otherwise it breaks the type inference, 
      if [rtk] is generics, we restrict [RTUnknow] by [rtw] and we will instanciate [rtw] with [RType.update_generics] function
      *)
    | RTUnknow, rtk | rtk, RTUnknow -> rtk
    | (RTTuple rkts as rkt), RTTuple lkts ->
        if Util.are_diff_lenght rkts lkts then rkt
        else RTTuple (List.map2 restrict_rktype rkts lkts)
    | _ -> to_restrict

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
  let rec instanciate_generics_type generics = 
    (* let () = List.iter (fun (s, kt) -> Printf.printf " %s => %s \n\n" s (Asttypprint.string_of_rktype kt)) generics in *)
function
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
| RECases {cases; else_case} -> RECases {
  cases = cases |> List.map ( fun (condition, body) -> 
    instanciate_generics_typed_expression generics condition,
  instanciate_generics_kbody generics body
    );
  else_case = else_case |> instanciate_generics_kbody generics
}
| _ as t -> t
and instanciate_generics_typed_expression generics typed_expr = 
(* let () = Printf.printf "Intanst te = %s \n\n" (Asttypprint.string_of_typed_expression typed_expr) in *)
{
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

  let label_of_fn_name current_module name typed_list = 
    Printf.sprintf "_%s.%s_%s" current_module name (typed_list |> List.map Asttypprint.string_of_label_rktype |> String.concat "_")

  let label_of_fn_name fn_module (rfunction_decl: rfunction_decl) = label_of_fn_name fn_module rfunction_decl.rfn_name (rfunction_decl.rparameters |> List.map snd)
  
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

    let function_decl_of_rtrue_function_decl (fn_decl: rtrue_function_decl): rfunction_decl = 
      {
        rfn_name = fn_decl.rfn_name;
        generics = [];
        rparameters = fn_decl.rparameters;
        return_type = fn_decl.return_type;
        rbody = fn_decl.rbody;
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

module Renum = struct
  let tag_of_variant variant (enum_decl: renum_decl) = 
    enum_decl.rvariants 
      |> List.mapi (fun i v -> Int32.of_int i,v )
      |> List.find_map (fun (i,v) -> if (fst v ) = variant then Some i else None)
      |> Option.get

  let assoc_types_of_variant variant (enum_decl: renum_decl) = 
    enum_decl.rvariants
    |> List.find_map (fun (evariant, assoc_type) -> 
      if evariant = variant then Some assoc_type else None
    )
    |> Option.get
end

module Rtype_Decl = struct
  type type_decl = RDecl_Struct of rstruct_decl | RDecl_Enum of renum_decl

  let type_name = function
    | RDecl_Struct e -> e.rstruct_name
    | RDecl_Enum s -> s.renum_name
end

module RFunction_Decl = struct
  type fn_decl = 
  | RSyscall_Decl of rsyscall_decl
  | RExternal_Decl of rexternal_func_decl
  | RKosufn_Decl of rfunction_decl

  let fn_name = function
  | RSyscall_Decl {rsyscall_name = n; _}
  | RKosufn_Decl {rfn_name = n; _} -> n
  | RExternal_Decl {rsig_name; c_name = _; _} -> 
    rsig_name
    (* c_name |> Option.value ~default:rsig_name *)
end

module Rmodule = struct
  open Rtype_Decl
  open RFunction_Decl

  let add_node node = function
  | RModule rnodes -> RModule (node::rnodes)

  let remove_generics_node = function
  | RModule rnodes -> RModule (rnodes |> List.filter ( fun rnode -> 
    match rnode with
    | RNFunction {generics; _} -> generics = []
    | _ -> true
    ))
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

  let retrive_functions_decl = function
  | RModule rnodes -> rnodes |> List.filter_map (function
  | RNFunction fn -> Some (RKosufn_Decl fn)
  | RNExternFunc ef -> Some (RExternal_Decl ef)
  | RNSyscall scf -> Some (RSyscall_Decl scf)
  | _ -> None
  )

  let find_function_decl fn_name = function
  | RModule rnodes -> rnodes |> List.find_map (fun rnodes -> 
    match rnodes with
    | RNFunction rfunction_decl when rfunction_decl.rfn_name = fn_name -> Some rfunction_decl
    | _ -> None 
    )

  let find_function_decl_param_ktype fn_name ktypes = function
  | RModule rnodes -> rnodes |> List.find_map (fun rnodes -> 
      match rnodes with
      | RNFunction rfunction_decl when rfunction_decl.rfn_name = fn_name && 
      (rfunction_decl.rparameters |> List.map snd |> ( = ) ktypes) -> Some rfunction_decl
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
  type fn_signature = {
    fn_name: string;
    params: rktype list;
    return_type: rktype
  }

  let signature_of_rfunction_decl (rfunction_decl: rfunction_decl) = {
    fn_name = rfunction_decl.rfn_name;
    params = rfunction_decl.rparameters |> List.map snd;
    return_type = rfunction_decl.return_type
  }

  let signature_of_rtrue_function_decl (rfunction_decl: rtrue_function_decl) = {
    fn_name = rfunction_decl.rfn_name;
    params = rfunction_decl.rparameters |> List.map snd;
    return_type = rfunction_decl.return_type
  } 

  module FnSpec = Set.Make(struct 
    type t = (string *rtrue_function_decl )
    let compare (lmodule, lhs) (rmodule, rhs) = 
      let module_cmp = compare lmodule rmodule in
      if module_cmp == 0 then
      compare (signature_of_rtrue_function_decl lhs) (signature_of_rtrue_function_decl rhs)
      else module_cmp
  end)

  let find_module_of_name module_name (rprogram : rprogram) =
    rprogram
    |> List.find_map (fun { filename = _; rmodule_path } ->
           if rmodule_path.path = module_name then Some rmodule_path.rmodule
           else None)

  let find_function_decls_in_module module_name rprogram =
  rprogram
  |> find_module_of_name module_name
  |> Option.map( Rmodule.retrive_functions_decl )

  let find_function_decl_of_name module_name fn_name rprogram = 
    let (>>=) = Option.bind in
    rprogram 
    |> find_function_decls_in_module module_name
    >>= List.find_opt (fun fn_decl -> fn_decl |> RFunction_Decl.fn_name |> ( = ) fn_name
    )


  let find_function_decl_exact_param_types ~module_name ~ktypes ~fn_name rprogram = 
    let ( >>= ) = Option.bind in
    rprogram
    |> find_module_of_name module_name
    >>= Rmodule.find_function_decl_param_ktype fn_name ktypes

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

      let register_params_count = 8

  let rec stack_parameters_in_expression current_module rprogram = function
    | REFunction_call {modules_path; fn_name; parameters; _} -> 
      let cmodule = if modules_path = "" then current_module else modules_path in
      let ktypes = parameters |> List.map (fun {rktype; _} -> rktype) in
      let fn_decl = find_function_decl_of_name cmodule fn_name rprogram |> Option.get in
      begin match fn_decl with
      | RSyscall_Decl _ -> 0
      | RExternal_Decl extenal_decl when not extenal_decl.is_variadic ->         
        let parmas_count = extenal_decl.fn_parameters |> List.length in
       let count = parmas_count - register_params_count in
        if count < 0 then 0 else count
      | RExternal_Decl external_decl -> 
        let non_variadic_count = List.length external_decl.fn_parameters in
        let call_params_count = List.length ktypes in
        let concrete_params_count = min non_variadic_count register_params_count in
        let count =  call_params_count - concrete_params_count in
        count
      | RKosufn_Decl fn_decl -> 
        let parmas_count = fn_decl.rparameters |> List.length in
        let count = parmas_count - register_params_count in
        if count < 0 then 0 else count
      end
    | REFieldAcces {first_expr; _} -> stack_parameters_in_typed_expression  current_module rprogram first_expr
    | REStruct {fields; _} -> fields |> List.map (fun (_, te) -> stack_parameters_in_typed_expression current_module rprogram te) |> List.fold_left max 0
    | REEnum {assoc_exprs = tes; _} | RETuple tes | REBuiltin_Function_call {parameters = tes; _} -> 
      tes |> List.map (stack_parameters_in_typed_expression current_module rprogram) |> List.fold_left max 0
    | REUnOperator_Function_call un | REUn_op un -> 
      begin match un with
      | RUMinus (te) | RUNot (te) -> stack_parameters_in_typed_expression current_module rprogram te
      end
    | REBinOperator_Function_call rkbin | REBin_op rkbin -> 
      let lhs, rhs = Binop.operands rkbin in
      max (stack_parameters_in_typed_expression current_module rprogram lhs) (stack_parameters_in_typed_expression current_module rprogram rhs)
    | REIf (if_condition, ifbody, else_body) -> 
      let if_count = stack_parameters_in_typed_expression current_module rprogram if_condition in
      let if_body_count = stack_parameters_in_body current_module rprogram ifbody in
      let else_body_count = stack_parameters_in_body current_module rprogram else_body in
      if_count |> max if_body_count |> max else_body_count  
    | RECases {cases; else_case} ->
      let cases_max = cases |> List.map (fun (te, kbody) -> 
        let te_count = stack_parameters_in_typed_expression current_module rprogram te in
        let kb_count = stack_parameters_in_body current_module rprogram kbody in
        max te_count kb_count
        ) |> List.fold_left max 0 in
      let else_count = stack_parameters_in_body current_module rprogram else_case in
      max cases_max else_count
    | RESwitch {rexpression; cases; wildcard_case} -> 
      let te_count = stack_parameters_in_typed_expression current_module rprogram rexpression in
      let cases_count = cases |> List.map (fun (_, _, kb) -> stack_parameters_in_body current_module rprogram kb) |> List.fold_left max 0 in
      let wildcard_count = wildcard_case |> Option.map (stack_parameters_in_body current_module rprogram) |> Option.value ~default:0 in
      te_count |> max cases_count |> max wildcard_count
    | _ -> 0
    and stack_parameters_in_typed_expression current_module rprogram {rexpression; _} = stack_parameters_in_expression current_module rprogram rexpression
    and stack_parameters_in_statement current_module rprogram = function
    | RSAffection (_ , te) | RSDiscard (te) 
    | RSDerefAffectation (_, te) | RSDeclaration {typed_expression = te; _}-> stack_parameters_in_typed_expression current_module rprogram te
    and stack_parameters_in_body current_module rprogram (stmts, te) = 
    let stmts_max = stmts |> List.map (stack_parameters_in_statement current_module rprogram) |> List.fold_left max 0 in
    let te_count = stack_parameters_in_typed_expression current_module rprogram te in
    max stmts_max te_count

  let rec specialise_generics_function current_module ~ignored rprogram = function
  | REFunction_call {modules_path; fn_name; generics_resolver; parameters} -> 
    let default_set = parameters |> List.map ( fun ({rktype = _; rexpression = _} as te) -> 
      (* let () = Printf.printf "te: %s\n\n" (Asttypprint.string_of_typed_expression te) in *)
      specialise_generics_function_typed_expression ~ignored current_module rprogram te 
      ) |> List.fold_left FnSpec.union FnSpec.empty in
    let function_module = if modules_path = "" then current_module else modules_path in
    rprogram 
    |> find_module_of_name function_module 
    |> Option.get
    |> Rmodule.find_function_decl fn_name
    |> fun (function_decl_opt) -> (
      match function_decl_opt with
      | None -> (* let () = Printf.printf "No decl for %s::%s\n" modules_path fn_name in *) default_set
      | Some function_decl ->
          if function_decl.generics = [] then default_set
    else  
    let maped_type = Hashtbl.create (parameters |> List.length) in
    let inner_specialise = parameters |> List.combine function_decl.rparameters |> List.map ( fun ((_, type_decl), ({rktype; rexpression = _} as te)) -> 
      (* let () = Printf.printf "te: %s\n\n" (Asttypprint.string_of_typed_expression te) in *)
      let _ = Function.is_type_compatible_hashgen maped_type rktype type_decl function_decl in
      specialise_generics_function_typed_expression ~ignored current_module rprogram te 
      ) |> List.fold_left FnSpec.union FnSpec.empty in
      begin match generics_resolver with
          | None -> 
            let type_list = maped_type |> Hashtbl.to_seq |> List.of_seq |> List.sort (fun (_ls, (lindex, _ltype))  (_rs, (rindex, _rtype)) -> compare lindex rindex) |> List.map (fun (_, (_, kt)) -> kt) in
            (* let () = List.iter (fun (s, kt) -> Printf.printf "In %s => %s \n\n" s (Asttypprint.string_of_rktype kt)) (List.combine function_decl.generics type_list) in *)
            let true_decl = Function.true_function_of_rfunction_decl type_list function_decl in
            let to_add = (function_module, true_decl) in 
            let recall = if FnSpec.mem to_add ignored then FnSpec.empty else specialise_generics_function_kbody ~ignored:(FnSpec.add to_add ignored) function_module rprogram true_decl.rbody in
            inner_specialise 
            |> FnSpec.add (function_module, true_decl)
            |> FnSpec.union recall
        (* specialise_generics_function_kbody function_module rprogram true_decl.rbody*)
        | Some generics -> 
          let true_decl = Function.true_function_of_rfunction_decl generics function_decl in
          let to_add = (function_module, true_decl) in 
          let recall = if FnSpec.mem to_add ignored then FnSpec.empty else specialise_generics_function_kbody ~ignored:(FnSpec.add to_add ignored) function_module rprogram true_decl.rbody in
          inner_specialise 
          |> FnSpec.add (function_module, true_decl)
          |> FnSpec.union recall
    end
    )
  | REFieldAcces {first_expr; _ } -> specialise_generics_function_typed_expression ~ignored current_module rprogram first_expr
  | REStruct { fields; _} -> 
    fields 
    |> List.map (fun (_, te) -> specialise_generics_function_typed_expression ~ignored current_module rprogram te) 
    |> List.fold_left FnSpec.union FnSpec.empty
  | REEnum {assoc_exprs = tes; _} | RETuple (tes) | REBuiltin_Function_call {parameters = tes; _} -> 
    tes 
    |> List.map (specialise_generics_function_typed_expression ~ignored current_module rprogram) 
    |> List.fold_left FnSpec.union FnSpec.empty
  | REIf(te_cond, if_body, else_body) ->
    te_cond 
    |> specialise_generics_function_typed_expression ~ignored current_module rprogram
    |> FnSpec.union ( if_body |> specialise_generics_function_kbody ~ignored current_module rprogram)
    |> FnSpec.union ( else_body |> specialise_generics_function_kbody ~ignored current_module rprogram)  
  | RECases {cases; else_case} ->
    cases |> List.map (fun (te, body) -> 
      te
      |> specialise_generics_function_typed_expression ~ignored current_module rprogram
      |> FnSpec.union (body |> specialise_generics_function_kbody ~ignored current_module rprogram)
    )
    |> List.fold_left (FnSpec.union) FnSpec.empty
    |> FnSpec.union (else_case |> specialise_generics_function_kbody ~ignored current_module rprogram)
  | RESwitch { rexpression; cases; wildcard_case } -> 
    rexpression 
    |> specialise_generics_function_typed_expression ~ignored current_module rprogram
    |> FnSpec.union 
      ( 
        cases 
        |> List.map (fun (_, _, body) -> 
          body |> specialise_generics_function_kbody ~ignored current_module rprogram)
          |> List.fold_left FnSpec.union FnSpec.empty
      )
    |> FnSpec.union (wildcard_case |> Option.map (specialise_generics_function_kbody ~ignored current_module rprogram) |> Option.value ~default:(FnSpec.empty))
  | REBinOperator_Function_call bin | REBin_op bin -> 
    let (lhs, rhs) =  Binop.operands bin in
    lhs 
    |> specialise_generics_function_typed_expression ~ignored current_module rprogram
    |> FnSpec.union (rhs |> specialise_generics_function_typed_expression ~ignored current_module rprogram)
  | REUnOperator_Function_call un | REUn_op un -> begin 
    match un with
    | RUMinus te | RUNot te -> specialise_generics_function_typed_expression ~ignored current_module rprogram te
  end 
  | _ -> FnSpec.empty
  and specialise_generics_function_typed_expression ~ignored current_module rprogram typed_expression : FnSpec.t = 
    specialise_generics_function ~ignored current_module rprogram typed_expression.rexpression
  and specialise_generics_function_statement ~ignored current_module rprogram = function
  | RSDeclaration {typed_expression; _} -> specialise_generics_function_typed_expression ~ignored current_module rprogram typed_expression
  | RSAffection (_, typed_expression) -> specialise_generics_function_typed_expression ~ignored current_module rprogram typed_expression
  | RSDiscard typed_expression -> specialise_generics_function_typed_expression ~ignored current_module rprogram typed_expression
  | RSDerefAffectation (_, typed_expression) -> specialise_generics_function_typed_expression ~ignored current_module rprogram typed_expression
  and specialise_generics_function_kbody ?(ignored = FnSpec.empty) current_module rprogram (rkstatements, return_exprs) =
  rkstatements 
    |> List.map (specialise_generics_function_statement ~ignored current_module rprogram) 
    |> List.fold_left FnSpec.union (FnSpec.empty)
    |> FnSpec.union ( return_exprs |> (specialise_generics_function_typed_expression ~ignored current_module rprogram))
    
  let specialise rprogram = 
    rprogram |> List.map ( fun { rmodule_path = {path; rmodule} ; _} -> 
      let root_functions = Rmodule.retrieve_non_generics_function rmodule in

      root_functions |> List.map (function
      | RFFunction rtrue_function_decl -> specialise_generics_function_kbody path rprogram rtrue_function_decl.rbody
      | RFOperator ( RUnary {kbody; _} | RBinary {kbody; _} ) ->  specialise_generics_function_kbody path rprogram kbody
      ) |>  List.fold_left FnSpec.union FnSpec.empty
      
    ) |> List.fold_left FnSpec.union FnSpec.empty
      
  let remove_generics rprogram = 
    rprogram |> List.map ( 
      fun {filename; rmodule_path = {path; rmodule}} -> 
      {
        filename;
        rmodule_path = {
          path;
          rmodule = Rmodule.remove_generics_node rmodule
        }
      })

  let append_function_decl (module_path, rtrue_function_decl) rprogram = 
    rprogram |> List.map ( fun {filename; rmodule_path = {path; rmodule}} -> 
      {
        filename;
        rmodule_path = {
          path;
          rmodule = 
            if path = module_path then 
              Rmodule.add_node (RNFunction (Function.function_decl_of_rtrue_function_decl rtrue_function_decl)) rmodule 
          else rmodule
        }
      }
    )

end