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
  (**
     @returns If the type represents an unsigned value (.ie pointer or unsigned integer) 
  *)
  let is_raw_unsigned = function
    | RTInteger _ | RTPointer _ -> true
    | _ -> false

  let is_any_integer = function RTInteger _ -> true | _ -> false
  let is_bool = function RTBool -> true | _ -> false

  let is_unsigned_integer = function
    | RTInteger (sign, _) -> sign = Unsigned
    | _ -> false

  let integer_info = function
    | RTInteger (sign, size) -> Some (sign, size)
    | _ -> None

  let is_64bits_float = function RTFloat -> true | _ -> false

  let is_builtin_type = function
    | RTParametric_identifier _ | RTType_Identifier _ -> false
    | _ -> true

  let rec npointer n kt = if n <= 0 then kt else RTPointer (npointer (n - 1) kt)

  let is_closure = function
  | RTClosure _ -> true
  | _ -> false

  let rtpointee = function
    | RTPointer kt -> kt
    | _ -> failwith "Cannot access pointee type of a none pointer type"

  let as_named_tuple = function
  | RTNamedTuple named_tuples -> named_tuples
  | _ -> failwith "Type is not a named tuples"

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
  let is_pointer = function RTPointer _ -> true | _ -> false

  let s32 = RTInteger (KosuFrontend.Ast.Signed, KosuFrontend.Ast.I32)
  

  let extract_parametrics_rktype = function
    | RTParametric_identifier { parametrics_type; _ } -> parametrics_type
    | _ -> []


    

  let rec kt_compare lhs rhs = 

    match lhs, rhs with
    | RTParametric_identifier rtlhs, RTParametric_identifier rtrhs -> 
      begin match compare (rtlhs.module_path, rtlhs.name) (rtrhs.module_path, rtrhs.name) with
        | 0 when Util.are_same_lenght rtlhs.parametrics_type rtrhs.parametrics_type -> 
          compare_list_type ~cmp:kt_compare rtlhs.parametrics_type rtrhs.parametrics_type
        | 0 -> List.compare_lengths rtlhs.parametrics_type rtrhs.parametrics_type
        | n -> n
      end
    | RTPointer l, RTPointer r -> kt_compare l r
    | RTTuple ls, RTTuple rs when Util.are_same_lenght ls rs ->           
      List.combine ls rs |> List.fold_left (fun acc (l, r) -> 
      if acc <> 0 then acc else kt_compare l r
    ) 0
    | RTNamedTuple ls, RTNamedTuple rs when Util.are_same_lenght ls rs ->
      compare_list_type ~cmp:kt_compare (List.map snd ls) (List.map snd rs)
    | RTFunction (pls, rl), RTFunction (prs, rr) 
    | RTClosure {params = pls; return_type = rl; _}, RTClosure {params = prs; return_type = rr; _} when Util.are_same_lenght pls prs -> 
      begin match kt_compare rl rr with
      | 0 -> compare_list_type ~cmp:kt_compare pls prs
      | n -> n 
      end
    | _ -> compare lhs rhs
    and compare_list_type ~cmp lhs rhs = 
    List.combine lhs rhs |> List.fold_left (fun acc (l, r) -> 
      if acc <> 0 then acc else cmp l r
    ) 0

let rec kt_compare_subtype lhs rhs = 
  match lhs, rhs with
  | RTParametric_identifier rtlhs, RTParametric_identifier rtrhs -> 
    begin match compare (rtlhs.module_path, rtlhs.name) (rtrhs.module_path, rtrhs.name) with
      | 0 when Util.are_same_lenght rtlhs.parametrics_type rtrhs.parametrics_type -> 
        compare_list_type ~cmp:kt_compare_subtype rtlhs.parametrics_type rtrhs.parametrics_type
      | 0 -> List.compare_lengths rtlhs.parametrics_type rtrhs.parametrics_type
      | n -> n
    end
  | RTPointer l, RTPointer r -> kt_compare_subtype l r
  | RTTuple ls, RTTuple rs when Util.are_same_lenght ls rs ->           
    List.combine ls rs |> List.fold_left (fun acc (l, r) -> 
    if acc <> 0 then acc else kt_compare_subtype l r
  ) 0
  | RTNamedTuple ls, RTNamedTuple rs when Util.are_same_lenght ls rs ->
    compare_list_type ~cmp:kt_compare_subtype (List.map snd ls) (List.map snd rs)
  | RTFunction (pls, rl), RTFunction (prs, rr) 
  | RTClosure {params = pls; return_type = rl; _}, RTClosure {params = prs; return_type = rr; _} 
  | RTFunction (pls, rl),  RTClosure {params = prs; return_type = rr; _} 
  | RTClosure {params = pls; return_type = rl; _}, RTFunction (prs, rr) 
  when Util.are_same_lenght pls prs -> 
    begin match kt_compare rl rr with
    | 0 -> compare_list_type ~cmp:kt_compare_subtype pls prs
    | n -> n 
    end
  | _ -> compare lhs rhs

  (* let kt_compare lhs rhs = 
    let res = kt_compare lhs rhs in
    let () = Printf.printf "\n\nCompare %s :: %s = %d\n\n" (Asttypprint.string_of_rktype lhs) (Asttypprint.string_of_rktype rhs) res in
    res *)

    let compare_test = 
      let no_env = RTClosure { params = [s32; s32]; return_type = s32; captured_env = [] } in
      let env = RTClosure { params = [s32; s32]; return_type = s32; captured_env = ["dummy", s32] } in
      kt_compare no_env env 

  (**
        
    *)
  let rec update_generics map init_type param_type () =
    match (init_type, param_type) with
    | kt, RTType_Identifier { module_path = ""; name } -> (
        match Hashtbl.find_opt map name with
        | Some t -> (
            match t with
            | RTUnknow ->
                let () = Hashtbl.replace map name kt in
                ()
            | _ as _t -> ())
        | None -> ())
    | ( RTParametric_identifier
          { module_path = lmp; parametrics_type = lpt; name = lname },
        RTParametric_identifier
          { module_path = rmp; parametrics_type = rpt; name = rname } ) ->
        if lmp <> rmp || lname <> rname || Util.are_diff_lenght lpt rpt then ()
        else List.iter2 (fun l r -> update_generics map l r ()) lpt rpt
    | RTPointer lhs, RTPointer rhs -> update_generics map lhs rhs ()
    | RTTuple lhs, RTTuple rhs ->
        List.iter2 (fun l r -> update_generics map l r ()) lhs rhs
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
    | RTPointer lhs, RTPointer rhs ->
        RTPointer (restrict_rktype lhs rhs)
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

module OperatorDeclaration = struct
  let op_return_type = function
    | RUnary { return_type; _ } | RBinary { return_type; _ } -> return_type

  let label_of_operator = function
    | RUnary { op; rfield = _, rktype; return_type; _ } ->
        Printf.sprintf "_%s.%s__%s"
          (KosuFrontend.Asthelper.ParserOperator.string_name_of_parser_unary op)
          (Asttypprint.string_of_label_rktype rktype)
          (Asttypprint.string_of_label_rktype return_type)
    | RBinary { op; rfields = (_, ltype), (_, rtype); return_type; _ } ->
        Printf.sprintf "_%s.%s_%s__%s"
          (KosuFrontend.Asthelper.ParserOperator.string_name_of_parser_binary op)
          (Asttypprint.string_of_label_rktype ltype)
          (Asttypprint.string_of_label_rktype rtype)
          (Asttypprint.string_of_label_rktype return_type)

    let body = function
    | RUnary {kbody; _ } | RBinary {kbody; _} -> kbody
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
    | RTType_Identifier { module_path = ""; name } as t -> (
        match generics |> List.assoc_opt name with None -> t | Some kt -> kt)
    | RTParametric_identifier { module_path; parametrics_type; name } ->
        RTParametric_identifier
          {
            module_path;
            parametrics_type =
              parametrics_type |> List.map (instanciate_generics_type generics);
            name;
          }
    | RTPointer ptr -> RTPointer (instanciate_generics_type generics ptr)
    | RTTuple rtks ->
        RTTuple (rtks |> List.map (instanciate_generics_type generics))
    | _ as t -> t

  let rec instanciate_generics_kbody generics (rkstatements, return_te) =
    ( rkstatements |> List.map (instanciate_generics_statement generics),
      instanciate_generics_typed_expression generics return_te )

  and instanciate_generics_statement generics = function
    | RSDeclaration { is_const; variable_name; typed_expression } ->
        RSDeclaration
          {
            is_const;
            variable_name;
            typed_expression =
              instanciate_generics_typed_expression generics typed_expression;
          }
    | RSAffection (s, te) ->
        RSAffection (s, instanciate_generics_typed_expression generics te)
    | RSDiscard te ->
        RSDiscard (instanciate_generics_typed_expression generics te)
    | RSDerefAffectation (s, te) ->
        RSDerefAffectation (s, instanciate_generics_typed_expression generics te)

  and instanciate_generics_rexpression generics = function
    | RESizeof ktype -> RESizeof (instanciate_generics_type generics ktype)
    | REFieldAcces { first_expr; field } ->
        REFieldAcces
          {
            first_expr =
              instanciate_generics_typed_expression generics first_expr;
            field;
          }
    | REStruct { modules_path; struct_name; fields } ->
        REStruct
          {
            modules_path;
            struct_name;
            fields =
              fields
              |> List.map (fun (s, te) ->
                     (s, instanciate_generics_typed_expression generics te));
          }
    | REEnum { modules_path; enum_name; variant; assoc_exprs } ->
        REEnum
          {
            modules_path;
            enum_name;
            variant;
            assoc_exprs =
              assoc_exprs
              |> List.map (instanciate_generics_typed_expression generics);
          }
    | RETuple tes ->
        RETuple
          (tes |> List.map (instanciate_generics_typed_expression generics))
    | REBuiltin_Function_call { fn_name; parameters } ->
        REBuiltin_Function_call
          {
            fn_name;
            parameters =
              parameters
              |> List.map (instanciate_generics_typed_expression generics);
          }
    | REFunction_call { modules_path; generics_resolver; fn_name; parameters }
      ->
        REFunction_call
          {
            modules_path;
            generics_resolver =
              generics_resolver
              |> Option.map (List.map (instanciate_generics_type generics));
            fn_name;
            parameters =
              parameters
              |> List.map (instanciate_generics_typed_expression generics);
          }
    | REBinOperator_Function_call bo ->
        REBinOperator_Function_call (instanciate_generics_binary_op generics bo)
    | REUnOperator_Function_call up ->
        REUnOperator_Function_call (instanciate_generics_unary_op generics up)
    | REBin_op bo -> REBin_op (instanciate_generics_binary_op generics bo)
    | REUn_op up -> REUn_op (instanciate_generics_unary_op generics up)
    | REIf (te, ifbody, elsebody) ->
        REIf
          ( instanciate_generics_typed_expression generics te,
            instanciate_generics_kbody generics ifbody,
            instanciate_generics_kbody generics elsebody )
    | RESwitch { rexpression; cases; wildcard_case } ->
        RESwitch
          {
            rexpression =
              instanciate_generics_typed_expression generics rexpression;
            cases =
              cases
              |> List.map (fun (rswichs_list, info_list, rbody) ->
                     ( rswichs_list,
                       info_list
                       |> List.map (fun (i, s, ktype) ->
                              (i, s, instanciate_generics_type generics ktype)),
                       instanciate_generics_kbody generics rbody ));
            wildcard_case =
              wildcard_case |> Option.map (instanciate_generics_kbody generics);
          }
    | RECases { cases; else_case } ->
        RECases
          {
            cases =
              cases
              |> List.map (fun (condition, body) ->
                     ( instanciate_generics_typed_expression generics condition,
                       instanciate_generics_kbody generics body ));
            else_case = else_case |> instanciate_generics_kbody generics;
          }
    | _ as t -> t

  and instanciate_generics_typed_expression generics typed_expr =
    (* let () = Printf.printf "Intanst te = %s \n\n" (Asttypprint.string_of_typed_expression typed_expr) in *)
    {
      rktype = instanciate_generics_type generics typed_expr.rktype;
      rexpression =
        instanciate_generics_rexpression generics typed_expr.rexpression;
    }

  and instanciate_generics_unary_op generics = function
    | RUMinus typed_expression ->
        RUMinus
          (instanciate_generics_typed_expression generics typed_expression)
    | RUNot te -> RUNot (instanciate_generics_typed_expression generics te)

  and instanciate_generics_binary_op generics = function
    | RBAdd (lhs, rhs) ->
        RBAdd
          ( instanciate_generics_typed_expression generics lhs,
            instanciate_generics_typed_expression generics rhs )
    | RBMinus (lhs, rhs) ->
        RBMinus
          ( instanciate_generics_typed_expression generics lhs,
            instanciate_generics_typed_expression generics rhs )
    | RBMult (lhs, rhs) ->
        RBMult
          ( instanciate_generics_typed_expression generics lhs,
            instanciate_generics_typed_expression generics rhs )
    | RBDiv (lhs, rhs) ->
        RBDiv
          ( instanciate_generics_typed_expression generics lhs,
            instanciate_generics_typed_expression generics rhs )
    | RBMod (lhs, rhs) ->
        RBMod
          ( instanciate_generics_typed_expression generics lhs,
            instanciate_generics_typed_expression generics rhs )
    | RBBitwiseOr (lhs, rhs) ->
        RBBitwiseOr
          ( instanciate_generics_typed_expression generics lhs,
            instanciate_generics_typed_expression generics rhs )
    | RBBitwiseAnd (lhs, rhs) ->
        RBBitwiseAnd
          ( instanciate_generics_typed_expression generics lhs,
            instanciate_generics_typed_expression generics rhs )
    | RBBitwiseXor (lhs, rhs) ->
        RBBitwiseXor
          ( instanciate_generics_typed_expression generics lhs,
            instanciate_generics_typed_expression generics rhs )
    | RBShiftLeft (lhs, rhs) ->
        RBShiftLeft
          ( instanciate_generics_typed_expression generics lhs,
            instanciate_generics_typed_expression generics rhs )
    | RBShiftRight (lhs, rhs) ->
        RBShiftRight
          ( instanciate_generics_typed_expression generics lhs,
            instanciate_generics_typed_expression generics rhs )
    | RBAnd (lhs, rhs) ->
        RBAdd
          ( instanciate_generics_typed_expression generics lhs,
            instanciate_generics_typed_expression generics rhs )
    | RBOr (lhs, rhs) ->
        RBOr
          ( instanciate_generics_typed_expression generics lhs,
            instanciate_generics_typed_expression generics rhs )
    | RBSup (lhs, rhs) ->
        RBSup
          ( instanciate_generics_typed_expression generics lhs,
            instanciate_generics_typed_expression generics rhs )
    | RBSupEq (lhs, rhs) ->
        RBSupEq
          ( instanciate_generics_typed_expression generics lhs,
            instanciate_generics_typed_expression generics rhs )
    | RBInf (lhs, rhs) ->
        RBInf
          ( instanciate_generics_typed_expression generics lhs,
            instanciate_generics_typed_expression generics rhs )
    | RBInfEq (lhs, rhs) ->
        RBInfEq
          ( instanciate_generics_typed_expression generics lhs,
            instanciate_generics_typed_expression generics rhs )
    | RBEqual (lhs, rhs) ->
        RBEqual
          ( instanciate_generics_typed_expression generics lhs,
            instanciate_generics_typed_expression generics rhs )
    | RBDif (lhs, rhs) ->
        RBDif
          ( instanciate_generics_typed_expression generics lhs,
            instanciate_generics_typed_expression generics rhs )
end

module Function = struct
  let label_of_fn_name current_module name typed_list =
    Printf.sprintf "_%s.%s_%s" current_module name
      (typed_list
      |> List.map Asttypprint.string_of_label_rktype
      |> String.concat "_")

  let label_of_fn_name fn_module (rfunction_decl : rfunction_decl) =
    label_of_fn_name fn_module rfunction_decl.rfn_name
      (rfunction_decl.rparameters |> List.map snd)

  let true_function_of_rfunction_decl generics (rfunction_decl : rfunction_decl)
      : rtrue_function_decl =
    let open Generics in
    match rfunction_decl.generics = [] with
    | true ->
        {
          rfn_name = rfunction_decl.rfn_name;
          rmaped_generics = [];
          rparameters = rfunction_decl.rparameters;
          return_type = rfunction_decl.return_type;
          rbody = rfunction_decl.rbody;
        }
    | false ->
        let assoc_generics = List.combine rfunction_decl.generics generics in
        let rparameters =
          rfunction_decl.rparameters
          |> List.map (fun (field, rtype) ->
                 (field, instanciate_generics_type assoc_generics rtype))
        in
        let return_type =
          instanciate_generics_type assoc_generics rfunction_decl.return_type
        in
        let rbody =
          instanciate_generics_kbody assoc_generics rfunction_decl.rbody
        in
        {
          rfn_name = rfunction_decl.rfn_name;
          rmaped_generics = generics;
          rparameters;
          return_type;
          rbody;
        }

  let function_decl_of_rtrue_function_decl (fn_decl : rtrue_function_decl) :
      rfunction_decl =
    {
      rfn_name = fn_decl.rfn_name;
      generics =
        fn_decl.rmaped_generics |> List.map Asttypprint.string_of_label_rktype;
      true_generics = false;
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
               if function_decl.generics |> List.mem name then
                 let () =
                   Hashtbl.replace generic_table name
                     ( function_decl.generics
                       |> Util.ListHelper.index_of (( = ) name),
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
    | RTPointer _, RTPointer RTUnknow -> true
    | RTPointer lhs, RTPointer rhs ->
        is_type_compatible_hashgen generic_table lhs rhs function_decl
    | RTTuple lhs, RTTuple rhs ->
        Util.are_same_lenght lhs rhs
        && List.for_all2
             (fun lkt rkt ->
               is_type_compatible_hashgen generic_table lkt rkt function_decl)
             lhs rhs
    | lhs, rhs -> lhs = rhs
end

module Renum = struct
  let instanciate_enum_decl generics enum_decl =
    let generics = generics |> List.to_seq |> Hashtbl.of_seq in
    {
      enum_decl with
      rvariants =
        enum_decl.rvariants
        |> List.map (fun (v, kts) ->
               (v, kts |> List.map (RType.remap_generic_ktype generics)));
    }

  let tag_of_variant variant (enum_decl : renum_decl) =
    enum_decl.rvariants
    |> List.mapi (fun i v -> (Int32.of_int i, v))
    |> List.find_map (fun (i, v) -> if fst v = variant then Some i else None)
    |> Option.get

  let assoc_types_of_variant ?(tagged = false) variant (enum_decl : renum_decl)
      =
    let tagtype =
      if tagged then
        RTInteger (KosuFrontend.Ast.Signed, KosuFrontend.Ast.I32) :: []
      else []
    in
    tagtype
    @ (enum_decl.rvariants
      |> List.find_map (fun (evariant, assoc_type) ->
             if evariant = variant then Some assoc_type else None)
      |> Option.get)
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
    | RSyscall_Decl { rsyscall_name = n; _ } | RKosufn_Decl { rfn_name = n; _ }
      ->
        n
    | RExternal_Decl { rsig_name; c_name = _; _ } -> rsig_name
  (* c_name |> Option.value ~default:rsig_name *)
end

module Rmodule = struct
  open Rtype_Decl
  open RFunction_Decl

  let add_node node = function RModule rnodes -> RModule (node :: rnodes)

  let remove_generics_node = function
    | RModule rnodes ->
        RModule
          (rnodes
          |> List.filter (fun rnode ->
                 match rnode with
                 | RNFunction { true_generics; _ } -> not true_generics
                 | _ -> true))

  let retrieve_non_generics_function = function
    | RModule rnodes ->
        rnodes
        |> List.filter_map (fun rnode ->
               match rnode with
               | RNFunction rfunction_decl when rfunction_decl.generics = [] ->
                   Some
                     (RFFunction
                        {
                          rfn_name = rfunction_decl.rfn_name;
                          rmaped_generics = [];
                          rparameters = rfunction_decl.rparameters;
                          return_type = rfunction_decl.return_type;
                          rbody = rfunction_decl.rbody;
                        })
               | RNOperator roperator_decl -> Some (RFOperator roperator_decl)
               | _ -> None)

  let retrive_functions_decl = function
    | RModule rnodes ->
        rnodes
        |> List.filter_map (function
             | RNFunction fn -> Some (RKosufn_Decl fn)
             | RNExternFunc ef -> Some (RExternal_Decl ef)
             | RNSyscall scf -> Some (RSyscall_Decl scf)
             | _ -> None)

  let retrieve_const_decl = function
    | RModule rnodes ->
        rnodes |> List.filter_map (function RNConst s -> Some s | _ -> None)

  let retrive_operator_decl = function
    | RModule rnodes ->
        rnodes
        |> List.filter_map (function RNOperator e -> Some e | _ -> None)

  let find_function_decl fn_name = function
    | RModule rnodes ->
        rnodes
        |> List.find_map (fun rnodes ->
               match rnodes with
               | RNFunction rfunction_decl
                 when rfunction_decl.rfn_name = fn_name ->
                   Some rfunction_decl
               | _ -> None)

  let find_function_decl_param_ktype fn_name ktypes = function
    | RModule rnodes ->
        rnodes
        |> List.find_map (fun rnodes ->
               match rnodes with
               | RNFunction rfunction_decl
                 when rfunction_decl.rfn_name = fn_name
                      && rfunction_decl.rparameters |> List.map snd
                         |> RType.compare_list_type ~cmp:RType.kt_compare_subtype ktypes |>  ( = ) 0 ->
                   Some rfunction_decl
               | _ -> None)

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
    fn_name : string;
    generics : string list;
    params : rktype list;
    return_type : rktype;
  }

  let signature_of_rfunction_decl (rfunction_decl : rfunction_decl) =
    {
      fn_name = rfunction_decl.rfn_name;
      generics = rfunction_decl.generics;
      params = rfunction_decl.rparameters |> List.map snd;
      return_type = rfunction_decl.return_type;
    }

  let signature_of_rtrue_function_decl (rfunction_decl : rtrue_function_decl) =
    {
      fn_name = rfunction_decl.rfn_name;
      generics =
        rfunction_decl.rmaped_generics
        |> List.map Asttypprint.string_of_label_rktype;
      params = rfunction_decl.rparameters |> List.map snd;
      return_type = rfunction_decl.return_type;
    }

  module FnSpec = Set.Make (struct
    type t = string * rtrue_function_decl

    let compare (lmodule, lhs) (rmodule, rhs) =
      let module_cmp = compare lmodule rmodule in
      if module_cmp = 0 then
        compare
          (signature_of_rtrue_function_decl lhs)
          (signature_of_rtrue_function_decl rhs)
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
    |> Option.map Rmodule.retrive_functions_decl

  let find_function_decl_of_name module_name fn_name rprogram =
    let ( >>= ) = Option.bind in
    rprogram
    |> find_function_decls_in_module module_name
    >>= List.find_opt (fun fn_decl ->
            fn_decl |> RFunction_Decl.fn_name |> ( = ) fn_name)

  let find_function_decl_exact_param_types ~module_name ~ktypes ~fn_name
      rprogram =
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

  let find_const_decl ~name ~module_path rprogram =
    let ( >>= ) = Option.bind in
    rprogram
    |> find_module_of_name module_path
    |> Option.map (fun md -> md |> Rmodule.retrieve_const_decl)
    >>= List.find_opt (fun const_decl -> const_decl.rconst_name = name)

  let register_params_count = 8

  let find_binary_operator_decl (op : KosuFrontend.Ast.parser_binary_op)
      (lhs, rhs) ~r_type rprogram =
    rprogram
    |> List.map (fun { filename = _; rmodule_path } ->
           rmodule_path.rmodule |> Rmodule.retrive_operator_decl
           |> List.filter (function
                | RUnary _ -> false
                | RBinary record ->
                    let (_, kt1), (_, kt2) = record.rfields in
                    op = record.op
                    && record.return_type = r_type
                    && lhs = kt1 && rhs = kt2))
    |> List.flatten

  let find_unary_operator_decl (op : KosuFrontend.Ast.parser_unary_op) lhs
      ~r_type rprogram =
    rprogram
    |> List.map (fun { filename = _; rmodule_path } ->
           rmodule_path.rmodule |> Rmodule.retrive_operator_decl
           |> List.filter (function
                | RBinary _ -> false
                | RUnary record ->
                    let _, kt1 = record.rfield in
                    op = record.op && record.return_type = r_type && lhs = kt1))
    |> List.flatten

  let rec stack_parameters_in_expression current_module rprogram = function
    | REFunction_call
        { modules_path; fn_name; parameters; generics_resolver = _ } -> (
        let cmodule =
          if modules_path = "" then current_module else modules_path
        in
        let ktypes = parameters |> List.map (fun { rktype; _ } -> rktype) in
        let fn_decl =
          rprogram |> find_function_decl_of_name cmodule fn_name |> function
          | Some s -> s
          | None -> failwith "814"
        in
        match fn_decl with
        | RSyscall_Decl _ -> 0
        | RExternal_Decl extenal_decl when not extenal_decl.is_variadic ->
            let parmas_count = extenal_decl.fn_parameters |> List.length in
            let count = parmas_count - register_params_count in
            if count < 0 then 0 else count
        | RExternal_Decl external_decl ->
            let non_variadic_count = List.length external_decl.fn_parameters in
            let call_params_count = List.length ktypes in
            let concrete_params_count =
              min non_variadic_count register_params_count
            in
            let count = call_params_count - concrete_params_count in
            count
        | RKosufn_Decl fn_decl ->
            let parmas_count = fn_decl.rparameters |> List.length in
            let count = parmas_count - register_params_count in
            if count < 0 then 0 else count)
    | REFieldAcces { first_expr; _ } ->
        stack_parameters_in_typed_expression current_module rprogram first_expr
    | REStruct { fields; _ } ->
        fields
        |> List.map (fun (_, te) ->
               stack_parameters_in_typed_expression current_module rprogram te)
        |> List.fold_left max 0
    | REEnum { assoc_exprs = tes; _ }
    | RETuple tes
    | REBuiltin_Function_call { parameters = tes; _ } ->
        tes
        |> List.map
             (stack_parameters_in_typed_expression current_module rprogram)
        |> List.fold_left max 0
    | REUnOperator_Function_call un | REUn_op un -> (
        match un with
        | RUMinus te | RUNot te ->
            stack_parameters_in_typed_expression current_module rprogram te)
    | REBinOperator_Function_call rkbin | REBin_op rkbin ->
        let lhs, rhs = Binop.operands rkbin in
        max
          (stack_parameters_in_typed_expression current_module rprogram lhs)
          (stack_parameters_in_typed_expression current_module rprogram rhs)
    | REIf (if_condition, ifbody, else_body) ->
        let if_count =
          stack_parameters_in_typed_expression current_module rprogram
            if_condition
        in
        let if_body_count =
          stack_parameters_in_body current_module rprogram ifbody
        in
        let else_body_count =
          stack_parameters_in_body current_module rprogram else_body
        in
        if_count |> max if_body_count |> max else_body_count
    | RECases { cases; else_case } ->
        let cases_max =
          cases
          |> List.map (fun (te, kbody) ->
                 let te_count =
                   stack_parameters_in_typed_expression current_module rprogram
                     te
                 in
                 let kb_count =
                   stack_parameters_in_body current_module rprogram kbody
                 in
                 max te_count kb_count)
          |> List.fold_left max 0
        in
        let else_count =
          stack_parameters_in_body current_module rprogram else_case
        in
        max cases_max else_count
    | RESwitch { rexpression; cases; wildcard_case } ->
        let te_count =
          stack_parameters_in_typed_expression current_module rprogram
            rexpression
        in
        let cases_count =
          cases
          |> List.map (fun (_, _, kb) ->
                 stack_parameters_in_body current_module rprogram kb)
          |> List.fold_left max 0
        in
        let wildcard_count =
          wildcard_case
          |> Option.map (stack_parameters_in_body current_module rprogram)
          |> Option.value ~default:0
        in
        te_count |> max cases_count |> max wildcard_count
    | _ -> 0

  and stack_parameters_in_typed_expression current_module rprogram
      { rexpression; _ } =
    stack_parameters_in_expression current_module rprogram rexpression

  and stack_parameters_in_statement current_module rprogram = function
    | RSAffection (_, te)
    | RSDiscard te
    | RSDerefAffectation (_, te)
    | RSDeclaration { typed_expression = te; _ } ->
        stack_parameters_in_typed_expression current_module rprogram te

  and stack_parameters_in_body current_module rprogram (stmts, te) =
    let stmts_max =
      stmts
      |> List.map (stack_parameters_in_statement current_module rprogram)
      |> List.fold_left max 0
    in
    let te_count =
      stack_parameters_in_typed_expression current_module rprogram te
    in
    max stmts_max te_count

  let rec specialise_generics_function current_module ~ignored rprogram =
    function
    | REFunction_call { modules_path; fn_name; generics_resolver; parameters }
      -> (
        let default_set =
          parameters
          |> List.map (fun ({ rktype = _; rexpression = _ } as te) ->
                 (* let () = Printf.printf "te: %s\n\n" (Asttypprint.string_of_typed_expression te) in *)
                 specialise_generics_function_typed_expression ~ignored
                   current_module rprogram te)
          |> List.fold_left FnSpec.union FnSpec.empty
        in
        let function_module =
          if modules_path = "" then current_module else modules_path
        in
        rprogram
        |> find_module_of_name function_module
        |> (function Some s -> s | None -> failwith "Ici ?")
        |> Rmodule.find_function_decl fn_name
        |> fun function_decl_opt ->
        match function_decl_opt with
        | None ->
            (* let () = Printf.printf "No decl for %s::%s\n" modules_path fn_name in *)
            default_set
        | Some function_decl -> (
            if function_decl.generics = [] then default_set
            else
              let maped_type = Hashtbl.create (parameters |> List.length) in
              let inner_specialise =
                parameters
                |> List.combine function_decl.rparameters
                |> List.map
                     (fun ((_, type_decl), ({ rktype; rexpression = _ } as te))
                     ->
                       (* let () = Printf.printf "te: %s\n\n" (Asttypprint.string_of_typed_expression te) in *)
                       let _ =
                         Function.is_type_compatible_hashgen maped_type rktype
                           type_decl function_decl
                       in
                       specialise_generics_function_typed_expression ~ignored
                         current_module rprogram te)
                |> List.fold_left FnSpec.union FnSpec.empty
              in
              match generics_resolver with
              | None ->
                  let type_list =
                    maped_type |> Hashtbl.to_seq |> List.of_seq
                    |> List.sort
                         (fun (_ls, (lindex, _ltype)) (_rs, (rindex, _rtype)) ->
                           compare lindex rindex)
                    |> List.map (fun (_, (_, kt)) -> kt)
                  in
                  (* let () = List.iter (fun (s, kt) -> Printf.printf "In %s => %s \n\n" s (Asttypprint.string_of_rktype kt)) (List.combine function_decl.generics type_list) in *)
                  let true_decl =
                    Function.true_function_of_rfunction_decl type_list
                      function_decl
                  in
                  let to_add = (function_module, true_decl) in
                  let recall =
                    if FnSpec.mem to_add ignored then FnSpec.empty
                    else
                      specialise_generics_function_kbody
                        ~ignored:(FnSpec.add to_add ignored)
                        function_module rprogram true_decl.rbody
                  in
                  inner_specialise
                  |> FnSpec.add (function_module, true_decl)
                  |> FnSpec.union recall
              (* specialise_generics_function_kbody function_module rprogram true_decl.rbody*)
              | Some generics ->
                  let true_decl =
                    Function.true_function_of_rfunction_decl generics
                      function_decl
                  in
                  let to_add = (function_module, true_decl) in
                  let recall =
                    if FnSpec.mem to_add ignored then FnSpec.empty
                    else
                      specialise_generics_function_kbody
                        ~ignored:(FnSpec.add to_add ignored)
                        function_module rprogram true_decl.rbody
                  in
                  inner_specialise
                  |> FnSpec.add (function_module, true_decl)
                  |> FnSpec.union recall))
    | REFieldAcces { first_expr; _ } ->
        specialise_generics_function_typed_expression ~ignored current_module
          rprogram first_expr
    | REStruct { fields; _ } ->
        fields
        |> List.map (fun (_, te) ->
               specialise_generics_function_typed_expression ~ignored
                 current_module rprogram te)
        |> List.fold_left FnSpec.union FnSpec.empty
    | REEnum { assoc_exprs = tes; _ }
    | RETuple tes
    | REBuiltin_Function_call { parameters = tes; _ } ->
        tes
        |> List.map
             (specialise_generics_function_typed_expression ~ignored
                current_module rprogram)
        |> List.fold_left FnSpec.union FnSpec.empty
    | REIf (te_cond, if_body, else_body) ->
        te_cond
        |> specialise_generics_function_typed_expression ~ignored current_module
             rprogram
        |> FnSpec.union
             (if_body
             |> specialise_generics_function_kbody ~ignored current_module
                  rprogram)
        |> FnSpec.union
             (else_body
             |> specialise_generics_function_kbody ~ignored current_module
                  rprogram)
    | RECases { cases; else_case } ->
        cases
        |> List.map (fun (te, body) ->
               te
               |> specialise_generics_function_typed_expression ~ignored
                    current_module rprogram
               |> FnSpec.union
                    (body
                    |> specialise_generics_function_kbody ~ignored
                         current_module rprogram))
        |> List.fold_left FnSpec.union FnSpec.empty
        |> FnSpec.union
             (else_case
             |> specialise_generics_function_kbody ~ignored current_module
                  rprogram)
    | RESwitch { rexpression; cases; wildcard_case } ->
        rexpression
        |> specialise_generics_function_typed_expression ~ignored current_module
             rprogram
        |> FnSpec.union
             (cases
             |> List.map (fun (_, _, body) ->
                    body
                    |> specialise_generics_function_kbody ~ignored
                         current_module rprogram)
             |> List.fold_left FnSpec.union FnSpec.empty)
        |> FnSpec.union
             (wildcard_case
             |> Option.map
                  (specialise_generics_function_kbody ~ignored current_module
                     rprogram)
             |> Option.value ~default:FnSpec.empty)
    | REBinOperator_Function_call bin | REBin_op bin ->
        let lhs, rhs = Binop.operands bin in
        lhs
        |> specialise_generics_function_typed_expression ~ignored current_module
             rprogram
        |> FnSpec.union
             (rhs
             |> specialise_generics_function_typed_expression ~ignored
                  current_module rprogram)
    | REUnOperator_Function_call un | REUn_op un -> (
        match un with
        | RUMinus te | RUNot te ->
            specialise_generics_function_typed_expression ~ignored
              current_module rprogram te)
    | _ -> FnSpec.empty

  and specialise_generics_function_typed_expression ~ignored current_module
      rprogram typed_expression : FnSpec.t =
    specialise_generics_function ~ignored current_module rprogram
      typed_expression.rexpression

  and specialise_generics_function_statement ~ignored current_module rprogram =
    function
    | RSDeclaration { typed_expression; _ } ->
        specialise_generics_function_typed_expression ~ignored current_module
          rprogram typed_expression
    | RSAffection (_, typed_expression) ->
        specialise_generics_function_typed_expression ~ignored current_module
          rprogram typed_expression
    | RSDiscard typed_expression ->
        specialise_generics_function_typed_expression ~ignored current_module
          rprogram typed_expression
    | RSDerefAffectation (_, typed_expression) ->
        specialise_generics_function_typed_expression ~ignored current_module
          rprogram typed_expression

  and specialise_generics_function_kbody ?(ignored = FnSpec.empty)
      current_module rprogram (rkstatements, return_exprs) =
    rkstatements
    |> List.map
         (specialise_generics_function_statement ~ignored current_module
            rprogram)
    |> List.fold_left FnSpec.union FnSpec.empty
    |> FnSpec.union
         (return_exprs
         |> specialise_generics_function_typed_expression ~ignored
              current_module rprogram)

  let specialise rprogram =
    rprogram
    |> List.map (fun { rmodule_path = { path; rmodule }; _ } ->
           let root_functions =
             Rmodule.retrieve_non_generics_function rmodule
           in

           root_functions
           |> List.map (function
                | RFFunction rtrue_function_decl ->
                    specialise_generics_function_kbody path rprogram
                      rtrue_function_decl.rbody
                | RFOperator (RUnary { kbody; _ } | RBinary { kbody; _ }) ->
                    specialise_generics_function_kbody path rprogram kbody)
           |> List.fold_left FnSpec.union FnSpec.empty)
    |> List.fold_left FnSpec.union FnSpec.empty

  let remove_generics rprogram =
    rprogram
    |> List.map (fun { filename; rmodule_path = { path; rmodule } } ->
           {
             filename;
             rmodule_path =
               { path; rmodule = Rmodule.remove_generics_node rmodule };
           })

  let append_function_decl (module_path, rtrue_function_decl) rprogram =
    rprogram
    |> List.map (fun { filename; rmodule_path = { path; rmodule } } ->
           {
             filename;
             rmodule_path =
               {
                 path;
                 rmodule =
                   (if path = module_path then
                    Rmodule.add_node
                      (RNFunction
                         (Function.function_decl_of_rtrue_function_decl
                            rtrue_function_decl))
                      rmodule
                   else rmodule);
               };
           })
end

module Sizeof = struct
  let ( ++ ) = Int64.add
  let ( -- ) = Int64.sub

  let align n b =
    let m = Int64.unsigned_rem n b in
    if m = 0L then n else n ++ b -- m


  module KtypeHashTbl = Hashtbl.Make(struct
    type t = rktype
    let equal lhs rhs = rhs |> RType.kt_compare_subtype lhs |> ( = ) 0
    let hash _ = 0 (* Temporary *)
  end)

  let map_size : int64 KtypeHashTbl.t = KtypeHashTbl.create 16

  let map_align : int64 KtypeHashTbl.t = KtypeHashTbl.create 16

  let rec size calcul program rktype =
    match rktype with
    | RTUnit | RTBool | RTUnknow -> 1L
    | RTInteger (_, isize) -> KosuFrontend.Ast.Isize.size_of_isize isize / 8 |> Int64.of_int
    | RTFloat | RTPointer _ | RTString_lit | RTFunction _ -> 8L
    | RTTuple kts -> size_tuple calcul program kts
    | RTNamedTuple kts -> kts |> List.map snd |> size_tuple calcul program
    | RTClosure {params = _; return_type = _; captured_env} ->
       (("", RTPointer RTUnknow)::captured_env |> List.map snd |> size_tuple calcul program)
    | (RTParametric_identifier _ | RTType_Identifier _) as kt -> (
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

  and size_tuple calcul program = function
    | list -> (
        let size, alignment, _packed_size =
          list
          |> List.fold_left
                (fun (acc_size, acc_align, _acc_packed_size) kt ->

                  let comming_size = sizeof program kt in
                  let comming_align = alignmentof program kt in

                  if comming_align = 0L then (acc_size, acc_align, _acc_packed_size)
                  else

                  let aligned = align acc_size comming_align in
                  let new_align = max acc_align comming_align in
                  ( aligned ++ comming_size,
                    new_align,
                    _acc_packed_size ++ comming_size ))
                (0L, 0L, 0L)
        in
        match calcul with
        | `size -> if alignment = 0L || size = 0L then 0L else align size alignment
        | `align -> alignment)

  and size_struct calcul program generics struct_decl =
    struct_decl.rfields
    |> List.map (fun (_, kt) -> RType.remap_generic_ktype generics kt)
    |> size_tuple calcul program

  and size_enum calcul program generics enum_decl =
    enum_decl.rvariants
    |> List.map (fun (_, kts) ->
            kts
            |> List.map (RType.remap_generic_ktype generics)
            |> List.cons (RTInteger (Unsigned, I32))
            |> RType.rtuple |> size calcul program)
    |> List.fold_left max 0L
  and sizeof program ktype = 
    match ktype with
    | RTClosure _ as kt -> 
      
      let clo_size = size `size program kt in
      let max_clo_size = match KtypeHashTbl.find_opt map_size kt with
        | None -> 
          (* let () = Printf.printf "None ::: input closure type = %s\n%!" (Asttypprint.string_of_rktype kt) in *)
          let () = KtypeHashTbl.add map_size kt clo_size in clo_size
        | Some found_size -> (* let () = Printf.printf "Some ::: input closure type = %s\n%!" (Asttypprint.string_of_rktype kt) in *)
        let max_size = max found_size clo_size in
        let () = KtypeHashTbl.replace map_size kt max_size in
        max_size
    in

      (* let map = map_size |> KtypeHashTbl.to_seq |> List.of_seq in
      let () = map |> List.map (fun (kt, size) -> 
        Printf.sprintf "%s => %Lu" (KosuIrTyped.Asttypprint.string_of_rktype kt) size
      ) |> String.concat "\n" |> Printf.printf "\n\n[%s]\n" in *)

      max_clo_size
    | ktype -> 
      begin match KtypeHashTbl.find_opt map_size ktype with
      | Some size -> size
      | None -> 
          let ktsize = size `size program ktype in
          let () = KtypeHashTbl.replace map_size ktype ktsize in
          ktsize
      end

  and alignmentof program ktype = 
    match KtypeHashTbl.find_opt map_align ktype with
    | Some align -> align
    | None -> 
      let kt_align = size `align program ktype in
      let () = KtypeHashTbl.replace map_align ktype kt_align in
      kt_align

  let offset_of_tuple_index ?(generics = Hashtbl.create 0) index rktypes
      rprogram =
    let ( ++ ) = Int64.add in

    rktypes
    |> List.mapi (fun i v -> (i, v))
    |> List.fold_left
          (fun ((acc_size, acc_align, found) as acc) (tindex, rktype) ->
            let comming_size =
              rktype |> RType.remap_generic_ktype generics |> size `size rprogram
            in
            let comming_align =
              rktype
              |> RType.remap_generic_ktype generics
              |> size `align rprogram
            in

            let aligned, new_align = 
            try
              align acc_size comming_align, max acc_align comming_align
            with Division_by_zero -> 0L, 0L
            in
            

            if found then acc
            else if index = tindex then 
              (aligned, new_align, true)
            else 
              (aligned ++ comming_size, new_align, found)
          ) (0L, 0L, false)
    |> function
    | a, _, _ -> a

  let offset_of_field ?(generics = Hashtbl.create 0) field rstruct_decl rprogram
      =
    let field_index =
      rstruct_decl.rfields
      |> List.mapi (fun index value -> (index, value))
      |> List.find_map (fun (index, (sfield, _)) ->
              if field = sfield then Some index else None)
      |> Option.get
    in

    let rktypes = rstruct_decl.rfields |> List.map snd in
    offset_of_tuple_index ~generics field_index rktypes rprogram

  (* To refacto later in functor*)
  (* on X86 and Arm64: if the retuned_value can be held in R0 and R1 (RAX, RCX)*)
  (* If so, there is no need to pass the address of the destination to the function*)
  (* Therefore : the retunred values dont need to be on the stack since there are discarded*)
  let discardable_size = function
    | 1L | 2L | 4L | 8L | 9L | 10L | 12L | 16L -> true
    | _ -> false

    let update_closure_size rprogram = function
  | RELambda {parameters; body = _; captured_env; clofn_name = _; return_ktype} 
    ->
      let name_type = match captured_env with 
      | [] -> RTFunction (parameters |> List.map snd, return_ktype)
      | _ ->  RTClosure {
          params = parameters |> List.map snd;
          return_type = return_ktype;
          captured_env
        }
    in
        let _ = sizeof rprogram name_type in
        ()

  | _ -> ()

end

module Closure = struct
  let make_closure_name ~closure_count s = 
    let n = !closure_count in
    let () = closure_count := n + 1 in
    Printf.sprintf "closure.%s.%u" s n

  module ClosureSet = Set.Make(struct
    type t = Asttyped.rclosure_function_decl
    let compare = compare
  end)

  let convert_expr ~expr ~ty_expr = { rexpression = expr; rktype = ty_expr.rktype }

  let rec create_clo_function_of_typed_expr ~closure_count current_module rprogram typed_expr = 
    let new_expr, closure = create_clo_function_of_expr ~ktype:typed_expr.rktype ~closure_count current_module rprogram typed_expr.rexpression in
    convert_expr ~expr:new_expr ~ty_expr:typed_expr, closure
  and create_clo_function_of_expr ~ktype ~closure_count current_module rprogram expr = 

  let () = Sizeof.update_closure_size rprogram expr in

  (* let () = Printf.printf "\n*** %s : Comptue ktype  %s: %Lu ***\n" (Asttypprint.string_of_rkexpression expr) (Asttypprint.string_of_rktype ktype) size in *)
  

  let closures_from_list list = 
      list |> List.fold_left (fun (acc_type, acc_clo) typed_expr -> 
        let new_ty_expr, closures = create_clo_function_of_typed_expr ~closure_count current_module rprogram typed_expr in
        new_ty_expr::acc_type, ClosureSet.union (closures) acc_clo
    ) ([], ClosureSet.empty) |> (fun (l, set) -> List.rev l, set)
  in

  let convert_binop_expr ~lhs ~rhs = 
    let lty, lclosure = create_clo_function_of_typed_expr ~closure_count current_module rprogram lhs in
    let rty, rclosure = create_clo_function_of_typed_expr ~closure_count current_module rprogram rhs in
    let closure = ClosureSet.union lclosure rclosure in
    (lty, rty), closure
  in

  let convert_binop = function
  | RBAdd (lhs, rhs) -> 
    let (lty, rty), closure = convert_binop_expr ~lhs ~rhs in
    RBAdd(lty,rty), closure
  | RBMinus (lhs, rhs) ->       
    let (lty, rty), closure = convert_binop_expr ~lhs ~rhs in
    RBMinus(lty,rty), closure
  | RBMult (lhs, rhs) ->       
    let (lty, rty), closure = convert_binop_expr ~lhs ~rhs in
    RBMult(lty,rty), closure
  | RBDiv (lhs, rhs) ->       
    let (lty, rty), closure = convert_binop_expr ~lhs ~rhs in
    RBDiv(lty,rty), closure
  | RBMod (lhs, rhs)  ->       
    let (lty, rty), closure = convert_binop_expr ~lhs ~rhs in
    RBMod(lty,rty), closure
  | RBBitwiseOr (lhs, rhs) ->       
    let (lty, rty), closure = convert_binop_expr ~lhs ~rhs in
    RBBitwiseOr(lty,rty), closure
  | RBBitwiseAnd (lhs, rhs) ->       
    let (lty, rty), closure = convert_binop_expr ~lhs ~rhs in
    RBMinus(lty,rty), closure
  | RBBitwiseXor (lhs, rhs) ->       
    let (lty, rty), closure = convert_binop_expr ~lhs ~rhs in
    RBBitwiseXor(lty,rty), closure
  | RBShiftLeft (lhs, rhs) ->       
    let (lty, rty), closure = convert_binop_expr ~lhs ~rhs in
    RBShiftLeft(lty,rty), closure
  | RBShiftRight (lhs, rhs) ->       
    let (lty, rty), closure = convert_binop_expr ~lhs ~rhs in
    RBShiftRight(lty,rty), closure
  | RBAnd (lhs, rhs) ->       
    let (lty, rty), closure = convert_binop_expr ~lhs ~rhs in
    RBAnd(lty,rty), closure
  | RBOr (lhs, rhs) ->       
    let (lty, rty), closure = convert_binop_expr ~lhs ~rhs in
    RBOr(lty,rty), closure
  | RBSup (lhs, rhs) ->       
    let (lty, rty), closure = convert_binop_expr ~lhs ~rhs in
    RBSup(lty,rty), closure
  | RBSupEq (lhs, rhs) ->       
    let (lty, rty), closure = convert_binop_expr ~lhs ~rhs in
    RBSupEq(lty,rty), closure
  | RBInf (lhs, rhs) ->       
    let (lty, rty), closure = convert_binop_expr ~lhs ~rhs in
    RBInf(lty,rty), closure
  | RBInfEq (lhs, rhs) ->       
    let (lty, rty), closure = convert_binop_expr ~lhs ~rhs in
    RBInfEq(lty,rty), closure
  | RBEqual (lhs, rhs) ->       
    let (lty, rty), closure = convert_binop_expr ~lhs ~rhs in
    RBEqual(lty,rty), closure
  | RBDif (lhs, rhs) ->       
    let (lty, rty), closure = convert_binop_expr ~lhs ~rhs in
    RBEqual(lty,rty), closure
  in

  match expr with
  | REFieldAcces {first_expr; field} -> 
    let new_expr, closures =  create_clo_function_of_typed_expr ~closure_count current_module rprogram first_expr in
    REFieldAcces {first_expr = new_expr; field}, closures
  | RELambda record -> 
    let rec iter_until_no_generate_body ~acc body =
      let new_body, closures = create_clo_function_of_kbody ~closure_count current_module rprogram body in
      if closures = ClosureSet.empty then (new_body, acc)
      else iter_until_no_generate_body ~acc:(ClosureSet.union acc closures) new_body 
    in

    (* To update closure size *)
    (* let _ = Sizeof.sizeof rprogram ktype in *)
    ignore ktype;


    let clofn_name = make_closure_name ~closure_count current_module in
    let rbody, closures = iter_until_no_generate_body ~acc:ClosureSet.empty record.body in

    let closure_fn_decl = {
      clo_name = clofn_name;
      rparameters = record.parameters;
      captured_env = record.captured_env;
      rbody = rbody;
      return_type = record.return_ktype
    } in
    let closures = ClosureSet.add closure_fn_decl closures in

    RELambda {
      clofn_name = Some clofn_name;
      parameters = record.parameters;
      return_ktype = record.return_ktype;
      body = rbody;
      captured_env = record.captured_env
    }, closures

  | REStruct {modules_path; struct_name; fields; } -> 
    let fields_name, fields_exprs = List.split fields in
    let new_typed_exprs, closures = fields_exprs |> closures_from_list in
    REStruct {modules_path; struct_name; fields = List.combine fields_name new_typed_exprs}, closures

  | REEnum {modules_path; enum_name; variant; assoc_exprs} ->
    let new_assoc_exprs, closures = closures_from_list assoc_exprs in
    REEnum {modules_path; enum_name; variant; assoc_exprs = new_assoc_exprs}, closures
  | RETuple exprs -> 
    let new_assoc_exprs, closures = closures_from_list exprs in
    RETuple new_assoc_exprs, closures
  | REBuiltin_Function_call { fn_name; parameters } -> 
    let new_assoc_exprs, closures = closures_from_list parameters in
    REBuiltin_Function_call {fn_name; parameters = new_assoc_exprs}, closures

  | REFunction_call { modules_path; generics_resolver; fn_name; parameters} -> 
    let new_assoc_exprs, closures = closures_from_list parameters in
    REFunction_call {modules_path; generics_resolver; fn_name; parameters = new_assoc_exprs}, closures
  | REUn_op (RUMinus typed_expression) -> 
    let new_typed_expr, closures = create_clo_function_of_typed_expr ~closure_count current_module rprogram typed_expression in
    REUn_op (RUMinus new_typed_expr), closures
  | REUn_op (RUNot typed_expression) -> 
    let new_typed_expr, closures = create_clo_function_of_typed_expr ~closure_count current_module rprogram typed_expression in
    REUn_op (RUNot new_typed_expr), closures
  | REUnOperator_Function_call (RUMinus typed_expression) -> 
    let new_typed_expr, closures = create_clo_function_of_typed_expr ~closure_count current_module rprogram typed_expression in
    REUnOperator_Function_call (RUMinus new_typed_expr), closures
  | REUnOperator_Function_call (RUNot typed_expression) -> 
    let new_typed_expr, closures = create_clo_function_of_typed_expr ~closure_count current_module rprogram typed_expression in
    REUnOperator_Function_call (RUNot new_typed_expr), closures
  | REBin_op rebin -> 
    let new_rebin, closure = convert_binop rebin in
    REBin_op new_rebin, closure
  | REBinOperator_Function_call rebin -> 
    let new_rebin, closures = convert_binop rebin in
    REBinOperator_Function_call new_rebin, closures
  | REWhile (condition, body) -> 
    let new_condition, condition_closure = create_clo_function_of_typed_expr ~closure_count current_module rprogram condition in
    let new_body, body_closures = create_clo_function_of_kbody ~closure_count current_module rprogram body in
    let closure = ClosureSet.union condition_closure body_closures in
    REWhile (new_condition, new_body), closure
  | REIf (condition, if_body, else_body) ->
    let new_condition, condition_closure = create_clo_function_of_typed_expr ~closure_count current_module rprogram condition in
    let new_ifbody, ifbody_closures = create_clo_function_of_kbody ~closure_count current_module rprogram if_body in
    let new_elsebody, elsebody_closures = create_clo_function_of_kbody ~closure_count current_module rprogram else_body in
    let closures = condition_closure
    |> ClosureSet.union ifbody_closures
    |> ClosureSet.union elsebody_closures
  in
  REIf (new_condition, new_ifbody, new_elsebody), closures
  | RECases {cases; else_case} ->
    let new_else_body, else_closures = create_clo_function_of_kbody ~closure_count current_module rprogram else_case in
    let new_cases, cases_closures = cases |> List.fold_left (fun (acc_case, acc_closure) (typed_expr, body) -> 
      let new_condition_expr, condition_closure = create_clo_function_of_typed_expr ~closure_count current_module rprogram typed_expr in
      let new_body, body_closures = create_clo_function_of_kbody ~closure_count current_module rprogram body in
      let closures = acc_closure |> ClosureSet.union condition_closure |> ClosureSet.union body_closures in
      (new_condition_expr, new_body)::acc_case, closures
    ) ([], ClosureSet.empty) in
    RECases {cases = new_cases; else_case = new_else_body}, ClosureSet.union cases_closures else_closures
  | RESwitch { rexpression; cases; wildcard_case} ->
    let new_condition_expr, condition_closure = create_clo_function_of_typed_expr ~closure_count current_module rprogram rexpression in
    let new_wildcard_body, wildcard_closure = 
        wildcard_case 
          |> Option.map (create_clo_function_of_kbody ~closure_count current_module rprogram) 
          |> Option.fold ~none:(None, ClosureSet.empty) ~some:(fun (body, clo) -> (Some body), clo)
      in
    let base_closure = ClosureSet.union condition_closure wildcard_closure in
    let new_cases, closure = cases |> List.fold_left (fun (acc_case, acc_closure) (rsc, list, body) -> 
      let new_body, body_closure = create_clo_function_of_kbody ~closure_count current_module rprogram body in
      (rsc, list, new_body)::acc_case, ClosureSet.union body_closure acc_closure
    ) ([], base_closure) in
    RESwitch {rexpression = new_condition_expr; cases = new_cases; wildcard_case = new_wildcard_body}, closure
  | expr -> expr, ClosureSet.empty

  and create_clo_function_of_kbody_acc ~rev_stmts ~acc ~closure_count current_module rprogram kbody =
  
  
    let smts, final_expr = kbody in
    match smts with
    | stmt::q -> begin match stmt with
      | RSDeclaration {is_const; variable_name; typed_expression} -> 
        let new_typed_expr, closures = create_clo_function_of_typed_expr ~closure_count current_module rprogram typed_expression in
        let declaration = RSDeclaration {is_const; variable_name; typed_expression = new_typed_expr} in
        let new_acc = ClosureSet.union closures acc in
        create_clo_function_of_kbody_acc ~acc:new_acc ~rev_stmts:(declaration::rev_stmts) ~closure_count current_module rprogram (q, final_expr)
      | RSAffection (id, typed_expression) -> 
        let new_typed_expr, closures = create_clo_function_of_typed_expr ~closure_count current_module rprogram typed_expression in
        let affectation = RSAffection (id, new_typed_expr) in
        let new_acc = ClosureSet.union closures acc in
        create_clo_function_of_kbody_acc ~acc:new_acc ~rev_stmts:(affectation::rev_stmts) ~closure_count current_module rprogram (q, final_expr)
      | RSDiscard (typed_expression) -> 
        let new_typed_expr, closures = create_clo_function_of_typed_expr ~closure_count current_module rprogram typed_expression in
        let discard = RSDiscard (new_typed_expr) in
        let new_acc = ClosureSet.union closures acc in
        create_clo_function_of_kbody_acc ~acc:new_acc ~rev_stmts:(discard::rev_stmts) ~closure_count current_module rprogram (q, final_expr)
      | RSDerefAffectation (id, typed_expression) ->
        let new_typed_expr, closures = create_clo_function_of_typed_expr ~closure_count current_module rprogram typed_expression in
        let deref = RSDerefAffectation (id, new_typed_expr) in
        let new_acc = ClosureSet.union closures acc in
        create_clo_function_of_kbody_acc ~acc:new_acc ~rev_stmts:(deref::rev_stmts) ~closure_count current_module rprogram (q, final_expr)
    end
    | [] -> 
      let new_final_expr, generated = create_clo_function_of_typed_expr ~closure_count current_module rprogram final_expr in
      let closures = ClosureSet.union acc generated in
      let reordered_stmt = List.rev rev_stmts in
      (reordered_stmt, new_final_expr), closures
  and create_clo_function_of_kbody ~closure_count current_module rprogram kbody = 
      create_clo_function_of_kbody_acc ~acc:ClosureSet.empty ~rev_stmts:[] ~closure_count current_module rprogram kbody

  let create_closure_node ~closure_count current_module rprogram = function
  | RModule nodes -> 
    let new_nodes =  nodes |> List.map (fun node -> 
      match node with
      | RNFunction record -> 
        let new_body, closures = create_clo_function_of_kbody ~closure_count current_module rprogram record.rbody in
        (RNFunction { record with rbody = new_body})::(closures |> ClosureSet.elements |> List.map (fun clo -> RNClosureFunc clo))
      | RNOperator (RBinary binary) -> 
        let new_body, closures = create_clo_function_of_kbody ~closure_count current_module rprogram binary.kbody in
        (RNOperator (RBinary {binary with kbody = new_body} ) )::( closures |> ClosureSet.elements |> List.map (fun clo -> RNClosureFunc clo) )
      | RNOperator (RUnary unary) -> 
        let new_body, closures = create_clo_function_of_kbody ~closure_count current_module rprogram unary.kbody in
        (RNOperator (RUnary {unary with kbody = new_body} ) )::( closures |> ClosureSet.elements |> List.map (fun clo -> RNClosureFunc clo) )
      | node  -> [node] 
    ) |> List.flatten
    in
    RModule new_nodes

  let generate_closure_from_lambda rprogram = 
    rprogram |> List.map (fun { filename; rmodule_path = { path; rmodule } } -> 
      {
        filename;
        rmodule_path = 
        {
          path;
          rmodule = create_closure_node ~closure_count:(ref 0) path rprogram rmodule
        }
      }
    )

end