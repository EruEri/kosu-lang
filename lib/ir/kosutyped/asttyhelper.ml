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

  let float_info = function RTFloat size -> Some size | _ -> None
  let is_64bits_float = function RTFloat F64 -> true | _ -> false
  let is_32bits_float = function RTFloat F32 -> true | _ -> false
  let is_float = function RTFloat _ -> true | _ -> false

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
  let is_pointer = function RTPointer _ -> true | _ -> false

  let extract_parametrics_rktype = function
    | RTParametric_identifier { parametrics_type; _ } -> parametrics_type
    | _ -> []

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
    | RBinary { op; rbfields = (_, ltype), (_, rtype); return_type; _ } ->
        Printf.sprintf "_%s.%s_%s__%s"
          (Asttypprint.string_name_of_extended_parser_binary op)
          (Asttypprint.string_of_label_rktype ltype)
          (Asttypprint.string_of_label_rktype rtype)
          (Asttypprint.string_of_label_rktype return_type)
end

module Binop = struct
  let pbadd = ParBinOp KosuFrontend.Ast.Add
  let pbminus = ParBinOp KosuFrontend.Ast.Minus
  let pbmult = ParBinOp KosuFrontend.Ast.Mult
  let pbmodulo = ParBinOp KosuFrontend.Ast.Modulo
  let pbdiv = ParBinOp KosuFrontend.Ast.Div
  let pbbitwiseor = ParBinOp KosuFrontend.Ast.BitwiseOr
  let pbbitwisexor = ParBinOp KosuFrontend.Ast.BitwiseXor
  let pbbitwiseoand = ParBinOp KosuFrontend.Ast.BitwiseAnd
  let pbshiftleft = ParBinOp KosuFrontend.Ast.ShiftLeft
  let pbshiftright = ParBinOp KosuFrontend.Ast.ShiftRight
  let pbequal = ParBinOp KosuFrontend.Ast.Equal
  let pbordered = ParBinOp KosuFrontend.Ast.Spaceship
  let pbsup = ParserSup
  let pbinf = ParserInf
  let pbsupeq = ParserSupEq
  let pbinfeq = ParserInfEq
  let pbdiff = ParserDiff

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
    | RBDif (lhs, _)
    | RBCmp (lhs, _) ->
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
    | RBDif (_, rhs)
    | RBCmp (_, rhs) ->
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
    | RBDif (lhs, rhs)
    | RBCmp (lhs, rhs) ->
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
    | RSDiscard te ->
        RSDiscard (instanciate_generics_typed_expression generics te)
    | RSAffection (RAFVariable (name, kt), te) ->
        RSAffection
          ( RAFVariable (name, instanciate_generics_type generics kt),
            instanciate_generics_typed_expression generics te )
    | RSAffection (RAFField { variable = name, kt; fields }, te) ->
        RSAffection
          ( RAFField
              {
                fields;
                variable = (name, instanciate_generics_type generics kt);
              },
            instanciate_generics_typed_expression generics te )
    | RSDerefAffectation (RAFVariable (name, kt), te) ->
        RSDerefAffectation
          ( RAFVariable (name, instanciate_generics_type generics kt),
            instanciate_generics_typed_expression generics te )
    | RSDerefAffectation (RAFField { variable = name, kt; fields }, te) ->
        RSDerefAffectation
          ( RAFField
              {
                fields;
                variable = (name, instanciate_generics_type generics kt);
              },
            instanciate_generics_typed_expression generics te )

  and instanciate_generics_rexpression generics = function
    | RESizeof ktype -> RESizeof (instanciate_generics_type generics ktype)
    | REFieldAcces { first_expr; field } ->
        REFieldAcces
          {
            first_expr =
              instanciate_generics_typed_expression generics first_expr;
            field;
          }
    | RETupleAccess { first_expr; index } ->
        RETupleAccess
          {
            first_expr =
              instanciate_generics_typed_expression generics first_expr;
            index;
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
    | REWhile (te, body) ->
        REWhile
          ( instanciate_generics_typed_expression generics te,
            instanciate_generics_kbody generics body )
    | ( REmpty | RTrue | RFalse | RENullptr | RECmpLess | RECmpEqual
      | RECmpGreater | REInteger _ | REFloat _ | REChar _ | REstring _
      | REAdress _
      | REDeference (_, _)
      | REIdentifier _ | REConst_Identifier _ ) as t ->
        t

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
    | RBCmp (lhs, rhs) ->
        RBCmp
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
        (* let () = Printf.printf "fn name = %s\n" rfunction_decl.rfn_name in *)
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

    let assoc_types_of_variant_tag ?(tagged = false) index (enum_decl : renum_decl)
      =
    let tagtype =
      if tagged then
        RTInteger (KosuFrontend.Ast.Signed, KosuFrontend.Ast.I32) :: []
      else []
    in
    tagtype
    @ (index |> List.nth enum_decl.rvariants |> snd)
end

module RStruct = struct
  let instanciate_struct_decl generics (struct_decl : rstruct_decl) =
    let generics = generics |> List.to_seq |> Hashtbl.of_seq in
    {
      struct_decl with
      rfields =
        struct_decl.rfields
        |> List.map (fun (s, kt) -> (s, RType.remap_generic_ktype generics kt));
    }

  let rktype_of_field_opt field (struct_decl : rstruct_decl) =
    struct_decl.rfields |> List.assoc_opt field
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

  let retrieve_binary_function_decl op = function
    | RModule rnodes ->
        rnodes
        |> List.filter_map (function
             | RNOperator (RBinary bod) when bod.op = op -> Some bod
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
                         |> ( = ) ktypes ->
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

module FnCallInfo = Set.Make (struct
  type t = function_call_info

  let compare = Stdlib.compare
end)

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
      if module_cmp == 0 then
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

  let find_binary_operator_decl (op : extended_parser_operator) (lhs, rhs)
      ~r_type rprogram =
    rprogram
    |> List.map (fun { filename = _; rmodule_path } ->
           rmodule_path.rmodule |> Rmodule.retrive_operator_decl
           |> List.filter (function
                | RUnary _ -> false
                | RBinary record ->
                    let (_, kt1), (_, kt2) = record.rbfields in
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
        let fn_decl =
          rprogram |> find_function_decl_of_name cmodule fn_name |> function
          | Some s -> s
          | None -> failwith "814"
        in
        match fn_decl with
        | RSyscall_Decl rsyscall_decl ->
            FnCallInfo.singleton
              {
                varia_index = None;
                parameters = rsyscall_decl.parameters;
                return_type = rsyscall_decl.return_type;
              }
        | RExternal_Decl extenal_decl when not extenal_decl.is_variadic ->
            FnCallInfo.singleton
              {
                varia_index = None;
                parameters = extenal_decl.fn_parameters;
                return_type = extenal_decl.return_type;
              }
        | RExternal_Decl external_decl ->
            FnCallInfo.singleton
              {
                varia_index =
                  Option.some @@ List.length external_decl.fn_parameters;
                parameters =
                  parameters
                  |> List.map (fun { rktype; rexpression = _ } -> rktype);
                return_type = external_decl.return_type;
              }
        | RKosufn_Decl fn_decl ->
            let parameters = List.map snd fn_decl.rparameters in
            FnCallInfo.singleton
              {
                varia_index = None;
                parameters;
                return_type = fn_decl.return_type;
              })
    | REFieldAcces { first_expr; _ } ->
        stack_parameters_in_typed_expression current_module rprogram first_expr
    | REStruct { fields; _ } ->
        fields
        |> List.map (fun (_, te) ->
               stack_parameters_in_typed_expression current_module rprogram te)
        |> List.fold_left FnCallInfo.union FnCallInfo.empty
    | REEnum { assoc_exprs = tes; _ }
    | RETuple tes
    | REBuiltin_Function_call { parameters = tes; _ } ->
        tes
        |> List.map
             (stack_parameters_in_typed_expression current_module rprogram)
        |> List.fold_left FnCallInfo.union FnCallInfo.empty
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
          |> List.fold_left FnCallInfo.union FnCallInfo.empty
        in
        let else_count =
          stack_parameters_in_body current_module rprogram else_case
        in
        FnCallInfo.union cases_max else_count
    | RESwitch { rexpression; cases; wildcard_case } ->
        let te_count =
          stack_parameters_in_typed_expression current_module rprogram
            rexpression
        in
        let cases_count =
          cases
          |> List.map (fun (_, _, kb) ->
                 stack_parameters_in_body current_module rprogram kb)
          |> List.fold_left FnCallInfo.union FnCallInfo.empty
        in
        let wildcard_count =
          wildcard_case
          |> Option.map (stack_parameters_in_body current_module rprogram)
          |> Option.value ~default:FnCallInfo.empty
        in
        te_count
        |> FnCallInfo.union cases_count
        |> FnCallInfo.union wildcard_count
    | _ -> FnCallInfo.empty

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
      |> List.fold_left FnCallInfo.union FnCallInfo.empty
    in
    let te_count =
      stack_parameters_in_typed_expression current_module rprogram te
    in
    FnCallInfo.union stmts_max te_count

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
    | REFieldAcces { first_expr; _ } | RETupleAccess { first_expr; _ } ->
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
    | REWhile (te, body) ->
        let fn_expr =
          specialise_generics_function_typed_expression ~ignored current_module
            rprogram te
        in
        body
        |> specialise_generics_function_kbody ~ignored current_module rprogram
        |> FnSpec.union fn_expr
    | REmpty | RTrue | RFalse | RECmpEqual | RECmpLess | RECmpGreater
    | RENullptr | REInteger _ | REFloat _ | REChar _ | RESizeof _ | REstring _
    | REAdress _
    | REDeference (_, _)
    | REIdentifier _ | REConst_Identifier _ ->
        FnSpec.empty

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

  let generate_function_from_spaceship_decl ~generate_equal
      (binary_operator_decl : binary_operator_decl) =
    let open Binop in
    let typed_expr_of_parameter p =
      {
        rktype = snd p;
        rexpression = REIdentifier { modules_path = ""; identifier = fst p };
      }
    in
    let lhs = typed_expr_of_parameter @@ fst binary_operator_decl.rbfields in
    let rhs = typed_expr_of_parameter @@ snd binary_operator_decl.rbfields in
    let cmp_rkbin_op_expr = REBinOperator_Function_call (RBCmp (lhs, rhs)) in
    let typed_cmp = { rktype = RTOrdered; rexpression = cmp_rkbin_op_expr } in

    let create_node op return_type params kbody =
      RNOperator (RBinary { op; return_type; rbfields = params; kbody })
    in

    let or_op lhs rhs =
      let expr = REBin_op (RBOr (lhs, rhs)) in
      { rktype = RTBool; rexpression = expr }
    in

    let equal_op lhs rhs =
      let expr = REBin_op (RBEqual (lhs, rhs)) in
      { rktype = RTBool; rexpression = expr }
    in

    let not_op lhs =
      let expr = REUn_op (RUNot lhs) in
      { rktype = lhs.rktype; rexpression = expr }
    in

    let inline_var_name = "#compare" in
    let inline_var_expr =
      typed_expr_of_parameter (inline_var_name, RTOrdered)
    in

    let gt = { rktype = RTOrdered; rexpression = RECmpGreater } in

    let eq = { rktype = RTOrdered; rexpression = RECmpEqual } in

    let lt = { rktype = RTOrdered; rexpression = RECmpLess } in

    let spaceshift_call_typed_expression =
      RSDeclaration
        {
          is_const = true;
          variable_name = inline_var_name;
          typed_expression = typed_cmp;
        }
    in

    let sup_expr = equal_op inline_var_expr gt in
    let supeq_expr =
      or_op (equal_op inline_var_expr eq) (equal_op inline_var_expr gt)
    in
    let inf_expr = equal_op inline_var_expr lt in
    let infeq_expr =
      or_op (equal_op inline_var_expr eq) (equal_op inline_var_expr lt)
    in

    let rtbool = RTBool in

    let rfields = binary_operator_decl.rbfields in

    let equal_expr = equal_op inline_var_expr eq in
    let dif_expr = not_op equal_expr in

    let spaceshift_call_stmt = [ spaceshift_call_typed_expression ] in

    let supbody : rkbody = (spaceshift_call_stmt, sup_expr) in
    let supeqbody : rkbody = (spaceshift_call_stmt, supeq_expr) in

    let infbody : rkbody = (spaceshift_call_stmt, inf_expr) in
    let infeqbody : rkbody = (spaceshift_call_stmt, infeq_expr) in
    let diffbody : rkbody = (spaceshift_call_stmt, dif_expr) in

    let supnode = create_node pbsup rtbool rfields supbody in
    let supeqnode = create_node pbsupeq rtbool rfields supeqbody in
    let infnode = create_node pbinf rtbool rfields infbody in
    let infeqnode = create_node pbinfeq rtbool rfields infeqbody in
    let diffnode = create_node pbdiff rtbool rfields diffbody in

    (* let () = Printf.printf "expr = %s\n%!" (Asttypprint.string_of_rkbody diffbody) in *)
    let always_generated =
      [ supnode; supeqnode; infnode; infeqnode; diffnode ]
    in
    match generate_equal with
    | false -> always_generated
    | true ->
        let equal_body : rkbody = (spaceshift_call_stmt, equal_expr) in
        let eq_node = create_node pbequal rtbool rfields equal_body in
        eq_node :: always_generated

  (**
    Gerete compare function from spaceshift [<=>] operator    
  *)
  let create_compare_function rprogram =
    rprogram
    |> List.map (fun { filename; rmodule_path = { path; rmodule } } ->
           let spaceshift_operator_decl =
             Rmodule.retrieve_binary_function_decl
               (ParBinOp KosuFrontend.Ast.Spaceship) rmodule
           in
           let generated =
             spaceshift_operator_decl
             |> List.map (fun decl ->
                    let (_, ltype), (_, rtype) = decl.rbfields in
                    let find_eq_decl =
                      find_binary_operator_decl Binop.pbequal (ltype, rtype)
                        ~r_type:RTBool rprogram
                    in
                    generate_function_from_spaceship_decl
                      ~generate_equal:(find_eq_decl = []) decl)
             |> List.flatten
           in

           let rmodule =
             generated
             |> List.fold_left
                  (fun acc_module node -> Rmodule.add_node node acc_module)
                  rmodule
           in
           { filename; rmodule_path = { path; rmodule } })

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
