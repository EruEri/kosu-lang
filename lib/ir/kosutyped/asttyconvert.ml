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
open Asttyhelper
open KosuFrontend.Ast
open KosuFrontend.Ast.Env
open KosuFrontend

module Make (TypeCheckerRule : KosuFrontend.TypeCheckerRule) = struct
  module KTypeChecker = KosuFrontend.Typecheck.Make (TypeCheckerRule)
  open KTypeChecker

  let restrict_typed_expression restrict typed_expression =
    {
      typed_expression with
      rktype = RType.restrict_rktype typed_expression.rktype restrict;
    }

  let rec from_ktype = function
    | TParametric_identifier { module_path; parametrics_type; name } ->
        RTParametric_identifier
          {
            module_path = module_path.v;
            parametrics_type =
              parametrics_type
              |> List.map (fun pkt -> pkt |> Position.value |> from_ktype);
            name = name.v;
          }
    | TType_Identifier { module_path; name } ->
        RTType_Identifier { module_path = module_path.v; name = name.v }
    | TInteger (Some (sign, size)) ->
        RTInteger (sign, size)
    | TInteger None ->
        RTInteger (Signed, I32)
    | TOpaque { module_path; name } ->
        RTOpaque { module_path = Position.value module_path; name = name.v }
    | TPointer kt ->
        RTPointer (from_ktype kt.v)
    | TTuple kts ->
        RTTuple (kts |> List.map (fun kt -> kt |> Position.value |> from_ktype))
    | TFunction (parameters, return_type) ->
        RTFunction
          ( parameters |> List.map (fun kt -> kt |> Position.value |> from_ktype),
            return_type |> Position.value |> from_ktype
          )
    | TString_lit ->
        RTString_lit
    | TFloat fsize ->
        let fsize = Option.value ~default:F64 fsize in
        RTFloat fsize
    | TBool ->
        RTBool
    | TOredered ->
        RTOrdered
    | TUnit ->
        RTUnit
    | TChar ->
        RTChar
    | TUnknow ->
        RTUnknow
    | TArray info ->
        RTArray { size = info.size.v; rktype = from_ktype info.ktype.v }

  let rec to_ktype =
    let open Position in
    function
    | RTUnknow ->
        TUnknow
    | RTFloat fsize ->
        TFloat (Some fsize)
    | RTBool ->
        TBool
    | RTUnit ->
        TUnit
    | RTOrdered ->
        TOredered
    | RTChar ->
        TChar
    | RTString_lit ->
        TString_lit
    | RTArray { size; rktype } ->
        TArray { size = val_dummy size; ktype = val_dummy @@ to_ktype rktype }
    | RTInteger (sign, size) ->
        TInteger (Some (sign, size))
    | RTPointer rkt ->
        TPointer { v = to_ktype rkt; position = dummy }
    | RTOpaque { module_path; name } ->
        TOpaque
          {
            module_path = Position.val_dummy module_path;
            name = val_dummy name;
          }
    | RTTuple rkts ->
        TTuple
          (rkts |> List.map (fun rkt -> { v = to_ktype rkt; position = dummy }))
    | RTFunction (parameters, return_type) ->
        TFunction
          ( parameters |> List.map (fun kt -> val_dummy @@ to_ktype kt),
            val_dummy @@ to_ktype return_type
          )
    | RTType_Identifier { module_path; name } ->
        TType_Identifier
          { module_path = val_dummy module_path; name = val_dummy name }
    | RTParametric_identifier { module_path; parametrics_type; name } ->
        TParametric_identifier
          {
            module_path = val_dummy module_path;
            parametrics_type =
              List.map (fun kt -> val_dummy @@ to_ktype kt) parametrics_type;
            name = val_dummy name;
          }

  let rec from_switch_case = function
    | SC_Enum_Identifier { variant } ->
        RSC_Enum_Identifier { variant = variant.v }
    | SC_Enum_Identifier_Assoc { variant; assoc_ids } ->
        RSC_Enum_Identifier_Assoc
          {
            variant = variant.v;
            assoc_ids = assoc_ids |> List.map (Option.map Position.value);
          }

  and typed_expression_of_kexpression ~generics_resolver (env : Env.t)
      (current_mod_name : string) (prog : module_path list)
      ?(hint_type = RTUnknow) (expression : kexpression Position.location) =
    {
      rktype =
        RType.restrict_rktype
          (expression
          |> typeof
               ~constraint_type:(Option.some @@ to_ktype hint_type)
               ~generics_resolver env current_mod_name prog
          |> from_ktype
          )
          hint_type;
      rexpression =
        from_kexpression ~generics_resolver env current_mod_name prog ~hint_type
          expression.v;
    }

  and raffacted_value_of_affected_value (env : Env.t) = function
    | AFVariable variable ->
        let var_kt =
          env
          |> Env.find_identifier_opt variable.v
          |> Option.get |> Env.vi_ktype |> from_ktype
        in
        RAFVariable (variable.v, var_kt)
    | AFField { variable; fields } ->
        let first_expr_type =
          env
          |> Env.find_identifier_opt variable.v
          |> Option.get |> Env.vi_ktype
        in
        RAFField
          {
            variable = (variable.v, from_ktype first_expr_type);
            fields = List.map Position.value fields;
          }

  and rkbody_of_kbody ~generics_resolver (env : Env.t) current_module
      (program : module_path list) ~return_type
      ( (kstatements : Ast.kstatement Position.location list),
        (kexpression : kexpression Position.location)
      ) =
    let open Position in
    match kstatements with
    | kstatement :: q -> (
        match kstatement.v with
        | SDiscard expr ->
            let rktype =
              from_ktype
              @@ typeof ~constraint_type:None ~generics_resolver env
                   current_module program expr
            in
            let mapped =
              typed_expression_of_kexpression ~generics_resolver env
                current_module program ~hint_type:rktype expr
            in
            let stmts_remains, future_expr =
              rkbody_of_kbody ~generics_resolver env current_module program
                ~return_type (q, kexpression)
            in
            (RSDiscard mapped :: stmts_remains, future_expr)
        | SDeclaration { is_const; variable_name; explicit_type; expression } ->
            let type_of_expression =
              typeof
                ~constraint_type:(Option.map Position.value explicit_type)
                ~generics_resolver env current_module program expression
            in
            let variable_type =
              match explicit_type with
              | None ->
                  type_of_expression
              | Some explicit_type ->
                  explicit_type.v
            in
            let typed_expression =
              typed_expression_of_kexpression ~generics_resolver env
                current_module program ~hint_type:(from_ktype variable_type)
                expression
            in
            let updated_env =
              env
              |> Env.add_variable
                   (variable_name.v, { is_const; ktype = variable_type })
            in
            let stmts_remains, future_expr =
              rkbody_of_kbody ~generics_resolver updated_env current_module
                program ~return_type (q, kexpression)
            in
            ( RSDeclaration
                { is_const; variable_name = variable_name.v; typed_expression }
              :: stmts_remains,
              future_expr
            )
        | SAffection (AFVariable variable, expression) ->
            let var_kt =
              env
              |> Env.find_identifier_opt variable.v
              |> Option.get |> Env.vi_ktype |> from_ktype
            in
            let typed_expression =
              typed_expression_of_kexpression ~generics_resolver env
                current_module program ~hint_type:var_kt expression
            in
            let stmts_remains, future_expr =
              rkbody_of_kbody ~generics_resolver env current_module program
                ~return_type (q, kexpression)
            in
            ( RSAffection (RAFVariable (variable.v, var_kt), typed_expression)
              :: stmts_remains,
              future_expr
            )
        | SAffection (AFField { variable; fields }, expression) ->
            let first_expr_type =
              env
              |> Env.find_identifier_opt variable.v
              |> Option.get |> Env.vi_ktype
            in
            let field_rktype =
              from_ktype
              @@ Asthelper.Affected_Value.field_type ~variable first_expr_type
                   current_module program fields
            in
            let typed_expression =
              typed_expression_of_kexpression ~generics_resolver env
                current_module program ~hint_type:field_rktype expression
            in
            let stmts_remains, future_expr =
              rkbody_of_kbody ~generics_resolver env current_module program
                ~return_type (q, kexpression)
            in
            ( RSAffection
                ( RAFField
                    {
                      variable = (variable.v, from_ktype first_expr_type);
                      fields = fields |> List.map Position.value;
                    },
                  typed_expression
                )
              :: stmts_remains,
              future_expr
            )
        | SDerefAffectation (AFVariable id, expression) ->
            let { is_const = _; ktype } =
              env |> Env.find_identifier_opt id.v |> Option.get
            in
            let pointee = Type.pointee_fail ktype in
            let ktype =
              Type.restrict_type pointee
                (expression
                |> typeof ~constraint_type:(Some pointee) ~generics_resolver env
                     current_module program
                )
            in
            let rktype = from_ktype ktype in
            let stmts_remains, future_expr =
              rkbody_of_kbody ~generics_resolver env current_module program
                ~return_type (q, kexpression)
            in
            let typed_expression =
              typed_expression_of_kexpression ~generics_resolver env
                current_module program ~hint_type:rktype expression
            in
            ( RSDerefAffectation
                (RAFVariable (id.v, from_ktype ktype), typed_expression)
              :: stmts_remains,
              future_expr
            )
        | SDerefAffectation (AFField { variable; fields }, expression) ->
            let { is_const = _; ktype } =
              env |> Env.find_identifier_opt variable.v |> Option.get
            in
            let pointee = Type.pointee_fail ktype in
            let field_rktype =
              from_ktype
              @@ Asthelper.Affected_Value.field_type ~variable pointee
                   current_module program fields
            in
            let stmts_remains, future_expr =
              rkbody_of_kbody ~generics_resolver env current_module program
                ~return_type (q, kexpression)
            in
            let typed_expression =
              typed_expression_of_kexpression ~generics_resolver env
                current_module program ~hint_type:field_rktype expression
            in
            ( RSDerefAffectation
                ( RAFField
                    {
                      variable = (variable.v, from_ktype ktype);
                      fields = fields |> List.map Position.value;
                    },
                  typed_expression
                )
              :: stmts_remains,
              future_expr
            )
      )
    | [] ->
        let rktype =
          match return_type with
          | RTUnknow ->
              kexpression
              |> typeof ~constraint_type:None ~generics_resolver env
                   current_module program
              |> from_ktype
          | kt ->
              kt
        in
        let typed_ex =
          {
            rktype;
            rexpression =
              from_kexpression ~generics_resolver env current_module program
                ~hint_type:rktype kexpression.v;
          }
        in
        ([], typed_ex)

  and from_kexpression ~generics_resolver (env : Env.t) current_module program
      ?(hint_type = RTUnknow) =
    let open Position in
    function
    | Empty ->
        REmpty
    | True ->
        RTrue
    | False ->
        RFalse
    | ECmpEqual ->
        RECmpEqual
    | ECmpGreater ->
        RECmpGreater
    | ECmpLess ->
        RECmpLess
    | ENullptr ->
        RENullptr
    | EChar c ->
        REChar c
    | EInteger (sign_size, value) ->
        let default =
          match hint_type with
          | RTInteger e ->
              e
          | _ ->
              Ast.Type.default_integer_info
        in
        let sign, size = Option.value ~default sign_size in
        REInteger (sign, size, value)
    | EFloat (size, float) ->
        let default =
          match hint_type with
          | RTFloat e ->
              e
          | _ ->
              Ast.Type.default_float_info
        in
        let size = Option.value ~default size in
        REFloat (size, float)
    | ESizeof either ->
        let ktype =
          match either with
          | Either.Left ktype ->
              ktype.v
          | Either.Right kexpression ->
              typeof ~constraint_type:None ~generics_resolver env current_module
                program kexpression
        in
        let rktype = from_ktype ktype in
        RESizeof rktype
    | EString s ->
        REstring s
    | EAdress e ->
        REAdress e.v
    | EDeference (c, id) ->
        REDeference (c, id.v)
    | EIdentifier { modules_path; identifier } ->
        REIdentifier
          { modules_path = modules_path.v; identifier = identifier.v }
    | ETupleAccess { first_expr; index } ->
        let typed_expression =
          typed_expression_of_kexpression ~generics_resolver env current_module
            program first_expr
        in
        RETupleAccess { first_expr = typed_expression; index = index.v }
    | EArrayAccess { array_expr; index_expr } ->
        let rearray_expr =
          typed_expression_of_kexpression ~generics_resolver env current_module
            program array_expr
        in
        let rindex_expr =
          typed_expression_of_kexpression ~generics_resolver env current_module
            program index_expr
        in
        REArrayAccess { array_expr = rearray_expr; index_expr = rindex_expr }
    | EAdressof affected_value ->
        let ra = raffacted_value_of_affected_value env affected_value in
        REAdressof ra
    | EFieldAcces { first_expr; field } ->
        let typed_expression =
          typed_expression_of_kexpression ~generics_resolver env current_module
            program first_expr
        in
        REFieldAcces
          { first_expr = typed_expression; field = field |> Position.value }
    | EConst_Identifier { modules_path; identifier } ->
        REConst_Identifier
          { modules_path = modules_path.v; identifier = identifier.v }
    | EStruct { modules_path; struct_name; fields } ->
        let struct_decl =
          Result.get_ok
          @@ Asthelper.Program.find_struct_decl_opt current_module modules_path
               struct_name program
        in
        let fields =
          struct_decl.fields |> List.combine fields
          |> List.map (fun ((s, para), (_, ktype)) ->
                 let hint =
                   match
                     KosuFrontend.Asthelper.Struct.is_type_generic ktype.v
                       struct_decl
                   with
                   | true ->
                       RTUnknow
                   | false ->
                       from_ktype ktype.v
                 in
                 ( s.v,
                   typed_expression_of_kexpression ~generics_resolver env
                     current_module ~hint_type:hint program para
                 )
             )
        in
        REStruct
          { modules_path = modules_path.v; struct_name = struct_name.v; fields }
    | EEnum { modules_path; enum_name; variant; assoc_exprs } ->
        let enum_decl =
          match
            Asthelper.Program.find_enum_decl_opt current_module modules_path
              (Option.map Position.value enum_name)
              variant assoc_exprs program
          with
          | Error e ->
              raise @@ Ast.Error.ast_error e
          | Ok e ->
              e
        in

        let assoc_types =
          Option.get @@ Asthelper.Enum.associate_type variant enum_decl
        in
        let assoc_exprs =
          assoc_types |> List.combine assoc_exprs
          |> List.map (fun (expr, ktype) ->
                 let hint =
                   match
                     KosuFrontend.Asthelper.Enum.is_type_generic ktype.v
                       enum_decl
                   with
                   | true ->
                       RTUnknow
                   | false ->
                       from_ktype ktype.v
                 in
                 typed_expression_of_kexpression ~generics_resolver env
                   current_module ~hint_type:hint program expr
             )
        in
        REEnum
          {
            modules_path = modules_path.v;
            enum_name = enum_name |> Option.map Position.value;
            variant = variant.v;
            assoc_exprs;
          }
    | ETuple exprs ->
        RETuple
          (exprs
          |> List.map
               (typed_expression_of_kexpression ~generics_resolver env
                  current_module program
               )
          )
    | EArray exprs ->
        let hint =
          match hint_type with RTArray info -> info.rktype | _ -> RTUnknow
        in
        REArray
          (exprs
          |> List.map
             @@ typed_expression_of_kexpression ~hint_type:hint
                  ~generics_resolver env current_module program
          )
    | EBuiltin_Function_call { fn_name; parameters } ->
        REBuiltin_Function_call
          {
            fn_name =
              KosuFrontend.Asthelper.Builtin_Function.builtin_fn_of_fn_name
                fn_name
              |> Result.get_ok;
            parameters =
              parameters
              |> List.map
                   (typed_expression_of_kexpression ~generics_resolver env
                      current_module program
                   );
          }
    | EWhile (condition, body) ->
        let typed_condition =
          typed_expression_of_kexpression ~generics_resolver env current_module
            program condition
        in
        let rkbody =
          rkbody_of_kbody ~generics_resolver ~return_type:hint_type
            (Env.push_empty_context env)
            current_module program body
        in
        REWhile (typed_condition, rkbody)
    | EIf (condition, if_block, else_block) ->
        REIf
          ( condition
            |> typed_expression_of_kexpression ~generics_resolver env
                 current_module program,
            rkbody_of_kbody ~generics_resolver
              (env |> Env.push_context [])
              current_module program ~return_type:hint_type if_block,
            rkbody_of_kbody ~generics_resolver
              (env |> Env.push_context [])
              current_module program ~return_type:hint_type else_block
          )
    | ECases { cases; else_case } ->
        RECases
          {
            cases =
              cases
              |> List.map (fun (expr, kbody) ->
                     ( typed_expression_of_kexpression ~generics_resolver env
                         current_module program expr,
                       kbody
                       |> rkbody_of_kbody ~generics_resolver
                            (env |> Env.push_context [])
                            current_module ~return_type:hint_type program
                     )
                 );
            else_case =
              else_case
              |> rkbody_of_kbody ~generics_resolver
                   (env |> Env.push_context [])
                   current_module ~return_type:hint_type program;
          }
    | EMatch _ ->
        failwith "TODO: in ast convert"
    | ESwitch { expression; cases; wildcard_case } ->
        let open Asthelper.Enum in
        let open Asthelper.Switch_case in
        let open Ast.Error in
        let variant_cases =
          cases
          |> List.map (fun (v, _) ->
                 v
                 |> List.map (function
                      | SC_Enum_Identifier { variant } ->
                          RSC_Enum_Identifier { variant = variant.v }
                      | SC_Enum_Identifier_Assoc { variant; assoc_ids } ->
                          RSC_Enum_Identifier_Assoc
                            {
                              variant = variant.v;
                              assoc_ids =
                                assoc_ids |> List.map (Option.map Position.value);
                            }
                      )
             )
        in
        let expr_type =
          typeof ~constraint_type:None ~generics_resolver env current_module
            program expression
        in

        let module_path, name =
          expr_type |> Ast.Type.module_path_of_ktype_opt |> Option.get
        in
        let enum_decl =
          match
            Asthelper.Program.find_type_decl_from_ktype
              ~ktype_def_path:module_path ~ktype_name:name ~current_module
              program
          with
          | Ok (Type_Decl.Decl_Enum e) ->
              e
          | _ ->
              failwith "Wierd it supposed to be an enum"
        in
        let generics_mapped =
          expr_type |> Ast.Type.extract_parametrics_ktype
          |> List.combine enum_decl.generics
          |> List.map Position.assocs_value
          |> List.to_seq |> Hashtbl.of_seq
        in
        (* let () = List.iter (fun (gen, kt) -> Printf.printf "generic = %s ==> %s\n\n" (Pprint.string_of_ktype gen) (Pprint.string_of_ktype kt.v)) generics_mapped in  *)
        let bound_variables, rkbodys =
          cases
          |> List.map (fun (sc_list, kb) ->
                 let combine_binding_type =
                   sc_list
                   |> List.map (fun sc ->
                          let variant_name = sc |> variant_name in
                          let assoc_types =
                            extract_assoc_remap_type_variant generics_mapped
                              variant_name enum_decl
                            |> Option.get
                          in
                          (* let () = List.iter (fun s -> Printf.printf "assoc = %s\n\n" (Pprint.string_of_ktype s.v)) assoc_types in *)
                          let assoc_binding = assoc_binding sc in
                          ( variant_name,
                            assoc_types |> List.combine assoc_binding
                            |> List.mapi (fun index (v, l) -> (index, v, l))
                          )
                      )
                 in
                 match combine_binding_type with
                 | [] ->
                     failwith "Unreachable case: empty case"
                 | (first_variant, ass_bin) :: q ->
                     let new_conext =
                       q
                       |> List.fold_left
                            (fun acc (variant_name, value) ->
                              let reduced_binding =
                                reduce_binded_variable_combine value
                              in
                              match
                                Ast.Type.find_field_error acc reduced_binding
                              with
                              | None ->
                                  acc
                              | Some (`diff_binding_name (lhs, rhs)) ->
                                  Incompatible_Binding_Name
                                    {
                                      switch_expr = expression;
                                      base_variant = first_variant;
                                      base_bound_id = lhs |> fst;
                                      wrong_variant = variant_name;
                                      wrong_bound_id = rhs |> fst;
                                    }
                                  |> switch_error |> raise
                              | Some (`diff_binding_ktype (lhs, rhs)) ->
                                  Incompatible_Binding_Ktype
                                    {
                                      switch_expr = expression;
                                      base_variant = first_variant;
                                      base_bound_id = lhs |> fst;
                                      base_bound_ktype = lhs |> snd;
                                      wrong_variant = variant_name;
                                      wrong_bound_id = rhs |> fst;
                                      wrong_bound_ktype = rhs |> snd;
                                    }
                                  |> switch_error |> raise
                              | Some
                                  (`diff_binding_index
                                    ( (base_index, base_bound_id),
                                      (wrong_index, wrong_bound_id)
                                    )
                                    ) ->
                                  Incompatible_Binding_Position
                                    {
                                      base_index;
                                      base_variant = first_variant;
                                      base_bound_id;
                                      wrong_index;
                                      wrong_variant = variant_name;
                                      wrong_bound_id;
                                    }
                                  |> switch_error |> raise
                            )
                            (reduce_binded_variable_combine ass_bin)
                       |> List.map (fun (i, variable_name, ktype) ->
                              ( i,
                                ( variable_name,
                                  ( { is_const = true; ktype = ktype.v }
                                    : Env.variable_info
                                    )
                                )
                              )
                          )
                     in
                     let new_variable_info = new_conext |> List.split |> snd in
                     ( new_conext,
                       rkbody_of_kbody ~generics_resolver
                         (env
                         |> Env.push_context
                              (new_variable_info
                              |> List.map Position.assoc_value_left
                              )
                         )
                         current_module program ~return_type:hint_type kb
                     )
             )
          |> List.split
        in
        let bound_variables =
          bound_variables
          |> List.map
               (List.map (fun (index, (bound_varn, variable_info)) ->
                    ( index,
                      bound_varn.v,
                      variable_info |> Env.vi_ktype |> from_ktype
                    )
                )
               )
        in
        let cases =
          rkbodys
          |> List.combine bound_variables
          |> List.combine variant_cases
          |> List.map (fun (a, (b, c)) -> (a, b, c))
        in
        RESwitch
          {
            rexpression =
              typed_expression_of_kexpression ~generics_resolver env
                current_module program expression;
            cases;
            wildcard_case =
              wildcard_case
              |> Option.map
                   (rkbody_of_kbody ~generics_resolver
                      (Env.push_empty_context env)
                      current_module ~return_type:hint_type program
                   );
          }
    | EFunction_call
        { modules_path; generics_resolver = grc; fn_name; parameters } -> (
        let fn_decl =
          Result.get_ok
          @@ Asthelper.Program.find_function_decl_from_fn_name modules_path
               fn_name current_module program
        in

        match fn_decl with
        | Ast.Function_Decl.Decl_Syscall
            { syscall_name = _; parameters = sys_type_parameters; _ } ->
            let typed_parameters =
              sys_type_parameters |> List.combine parameters
              |> List.map (fun (para, ktype) ->
                     typed_expression_of_kexpression ~generics_resolver
                       ~hint_type:(from_ktype ktype.v) env current_module
                       program para
                 )
            in
            REFunction_call
              {
                modules_path = modules_path.v;
                generics_resolver = None;
                fn_name = fn_name.v;
                parameters = typed_parameters;
              }
        | Ast.Function_Decl.Decl_External
            { sig_name = _; fn_parameters; is_variadic; _ } -> (
            match is_variadic with
            | true ->
                let known_parameters_len = List.length fn_parameters in
                let expected_params, variadic_params =
                  parameters |> List.mapi Util.couple
                  |> List.partition_map (fun (index, para) ->
                         if index < known_parameters_len then
                           Either.left para
                         else
                           Either.right para
                     )
                in
                let expected_typed_parameters =
                  fn_parameters
                  |> List.combine expected_params
                  |> List.map (fun (para, ktype) ->
                         typed_expression_of_kexpression ~generics_resolver
                           ~hint_type:(from_ktype ktype.v) env current_module
                           program para
                     )
                in
                let variadic_typed_parameters =
                  variadic_params
                  |> List.map
                       (typed_expression_of_kexpression ~generics_resolver env
                          current_module program
                       )
                in
                let typed_parameters =
                  expected_typed_parameters @ variadic_typed_parameters
                in
                REFunction_call
                  {
                    modules_path = modules_path.v;
                    generics_resolver = None;
                    fn_name = fn_name.v;
                    parameters = typed_parameters;
                  }
            | false ->
                let typed_parameters =
                  fn_parameters |> List.combine parameters
                  |> List.map (fun (para, ktype) ->
                         typed_expression_of_kexpression ~generics_resolver
                           ~hint_type:(from_ktype ktype.v) env current_module
                           program para
                     )
                in
                REFunction_call
                  {
                    modules_path = modules_path.v;
                    generics_resolver = None;
                    fn_name = fn_name.v;
                    parameters = typed_parameters;
                  }
          )
        | Ast.Function_Decl.Decl_Kosu_Function kosu_function ->
            let new_map_generics =
              kosu_function.generics
              |> List.map (fun s -> (s, ()))
              |> List.to_seq |> Hashtbl.of_seq
            in
            let function_rktype_param =
              kosu_function.parameters
              |> List.map (fun (_, { v = kt; _ }) -> from_ktype kt)
            in
            let typed_parameters =
              kosu_function.parameters |> List.combine parameters
              |> List.map (fun (para, (_, ktype)) ->
                     let hint =
                       match
                         KosuFrontend.Asthelper.Function.is_ktype_generic
                           ktype.v kosu_function
                       with
                       | true ->
                           RTUnknow
                       | false ->
                           from_ktype ktype.v
                     in
                     typed_expression_of_kexpression
                       ~generics_resolver:new_map_generics env current_module
                       ~hint_type:hint program para
                 )
            in

            let hashmap =
              kosu_function.generics
              |> List.map (fun s -> (s.v, RTUnknow))
              |> List.to_seq |> Hashtbl.of_seq
            in
            let () =
              List.iter2
                (fun { rktype; _ } param ->
                  let _ = RType.update_generics hashmap rktype param () in
                  ()
                )
                typed_parameters function_rktype_param
            in
            let typed_parameters =
              kosu_function.parameters
              |> List.map (fun (_, { v = kt; _ }) ->
                     kt |> from_ktype |> RType.remap_generic_ktype hashmap
                 )
              |> List.map2
                   (fun typed_exp kt -> restrict_typed_expression kt typed_exp)
                   typed_parameters
            in
            REFunction_call
              {
                modules_path = modules_path.v;
                generics_resolver =
                  grc
                  |> Option.map
                       (List.map (fun kt -> from_ktype @@ Position.value kt));
                fn_name = fn_name.v;
                parameters = typed_parameters;
              }
      )
    | EUn_op (UMinus expression) ->
        let typed =
          typed_expression_of_kexpression ~generics_resolver env current_module
            program expression
        in

        if RType.is_builtin_type typed.rktype then
          REUn_op (RUMinus typed)
        else
          REUnOperator_Function_call (RUMinus typed)
    | EUn_op (UNot expression) ->
        let typed =
          typed_expression_of_kexpression ~generics_resolver env current_module
            program expression
        in
        let runot = RUNot typed in
        if RType.is_builtin_type typed.rktype then
          REUn_op runot
        else
          REUnOperator_Function_call runot
    | EBin_op binop ->
        let rkbin =
          match binop with
          | BAdd (lhs, rhs) ->
              let ltyped =
                typed_expression_of_kexpression ~generics_resolver env
                  current_module program lhs
              in
              let rtyped =
                typed_expression_of_kexpression ~generics_resolver env
                  current_module program rhs
              in
              RBAdd (ltyped, rtyped)
          | BMinus (lhs, rhs) ->
              let ltyped =
                typed_expression_of_kexpression ~generics_resolver env
                  current_module program lhs
              in
              let rtyped =
                typed_expression_of_kexpression ~generics_resolver env
                  current_module program rhs
              in
              RBMinus (ltyped, rtyped)
          | BMult (lhs, rhs) ->
              let ltyped =
                typed_expression_of_kexpression ~generics_resolver env
                  current_module program lhs
              in
              let rtyped =
                typed_expression_of_kexpression ~generics_resolver env
                  current_module program rhs
              in
              RBMult (ltyped, rtyped)
          | BDiv (lhs, rhs) ->
              let ltyped =
                typed_expression_of_kexpression ~generics_resolver env
                  current_module program lhs
              in
              let rtyped =
                typed_expression_of_kexpression ~generics_resolver env
                  current_module program rhs
              in
              RBDiv (ltyped, rtyped)
          | BMod (lhs, rhs) ->
              let ltyped =
                typed_expression_of_kexpression ~generics_resolver env
                  current_module program lhs
              in
              let rtyped =
                typed_expression_of_kexpression ~generics_resolver env
                  current_module program rhs
              in
              RBMod (ltyped, rtyped)
          | BBitwiseOr (lhs, rhs) ->
              let ltyped =
                typed_expression_of_kexpression ~generics_resolver env
                  current_module program lhs
              in
              let rtyped =
                typed_expression_of_kexpression ~generics_resolver env
                  current_module program rhs
              in
              RBBitwiseOr (ltyped, rtyped)
          | BBitwiseAnd (lhs, rhs) ->
              let ltyped =
                typed_expression_of_kexpression ~generics_resolver env
                  current_module program lhs
              in
              let rtyped =
                typed_expression_of_kexpression ~generics_resolver env
                  current_module program rhs
              in
              RBBitwiseAnd (ltyped, rtyped)
          | BBitwiseXor (lhs, rhs) ->
              let ltyped =
                typed_expression_of_kexpression ~generics_resolver env
                  current_module program lhs
              in
              let rtyped =
                typed_expression_of_kexpression ~generics_resolver env
                  current_module program rhs
              in
              RBBitwiseXor (ltyped, rtyped)
          | BShiftLeft (lhs, rhs) ->
              let ltyped =
                typed_expression_of_kexpression ~generics_resolver env
                  current_module program lhs
              in
              let rtyped =
                typed_expression_of_kexpression ~generics_resolver env
                  current_module program rhs
              in
              RBShiftLeft (ltyped, rtyped)
          | BShiftRight (lhs, rhs) ->
              let ltyped =
                typed_expression_of_kexpression ~generics_resolver env
                  current_module program lhs
              in
              let rtyped =
                typed_expression_of_kexpression ~generics_resolver env
                  current_module program rhs
              in
              RBShiftRight (ltyped, rtyped)
          | BAnd (lhs, rhs) ->
              let ltyped =
                typed_expression_of_kexpression ~generics_resolver env
                  current_module program lhs
              in
              let rtyped =
                typed_expression_of_kexpression ~generics_resolver env
                  current_module program rhs
              in
              RBAnd (ltyped, rtyped)
          | BOr (lhs, rhs) ->
              let ltyped =
                typed_expression_of_kexpression ~generics_resolver env
                  current_module program lhs
              in
              let rtyped =
                typed_expression_of_kexpression ~generics_resolver env
                  current_module program rhs
              in
              RBOr (ltyped, rtyped)
          | BSup (lhs, rhs) ->
              let ltyped =
                typed_expression_of_kexpression ~generics_resolver env
                  current_module program lhs
              in
              let rtyped =
                typed_expression_of_kexpression ~generics_resolver env
                  current_module program rhs
              in
              RBSup (ltyped, rtyped)
          | BSupEq (lhs, rhs) ->
              let ltyped =
                typed_expression_of_kexpression ~generics_resolver env
                  current_module program lhs
              in
              let rtyped =
                typed_expression_of_kexpression ~generics_resolver env
                  current_module program rhs
              in
              RBSupEq (ltyped, rtyped)
          | BInf (lhs, rhs) ->
              let ltyped =
                typed_expression_of_kexpression ~generics_resolver env
                  current_module program lhs
              in
              let rtyped =
                typed_expression_of_kexpression ~generics_resolver env
                  current_module program rhs
              in
              RBInf (ltyped, rtyped)
          | BInfEq (lhs, rhs) ->
              let ltyped =
                typed_expression_of_kexpression ~generics_resolver env
                  current_module program lhs
              in
              let rtyped =
                typed_expression_of_kexpression ~generics_resolver env
                  current_module program rhs
              in
              RBInfEq (ltyped, rtyped)
          | BEqual (lhs, rhs) ->
              let ltyped =
                typed_expression_of_kexpression ~generics_resolver env
                  current_module program lhs
              in
              let rtyped =
                typed_expression_of_kexpression ~generics_resolver env
                  current_module program rhs
              in
              RBEqual (ltyped, rtyped)
          | BDif (lhs, rhs) ->
              let ltyped =
                typed_expression_of_kexpression ~generics_resolver env
                  current_module program lhs
              in
              let rtyped =
                typed_expression_of_kexpression ~generics_resolver env
                  current_module program rhs
              in
              RBDif (ltyped, rtyped)
          | BCmp (lhs, rhs) ->
              let ltyped =
                typed_expression_of_kexpression ~generics_resolver env
                  current_module program lhs
              in
              let rtyped =
                typed_expression_of_kexpression ~generics_resolver env
                  current_module program rhs
              in
              RBCmp (ltyped, rtyped)
        in
        let lhs, rhs = Binop.operands rkbin in
        if
          lhs.rktype |> RType.is_builtin_type |> not
          || rhs.rktype |> RType.is_builtin_type |> not
        then
          REBinOperator_Function_call rkbin
        else
          REBin_op rkbin

  and from_module_node current_module (prog : module_path list) =
    let open Position in
    function
    | NOpaque s ->
        RNOpaque s.v
    | NStruct { struct_name; generics; fields } ->
        RNStruct
          {
            rstruct_name = struct_name.v;
            generics = generics |> List.map Position.value;
            rfields =
              fields
              |> List.map (fun (field, ktype) ->
                     (field.v, ktype |> Position.value |> from_ktype)
                 );
          }
    | NEnum { enum_name; generics; variants } ->
        RNEnum
          {
            renum_name = enum_name.v;
            generics = generics |> List.map Position.value;
            rvariants =
              variants
              |> List.map (fun (variant, assoc_ktype) ->
                     ( variant.v,
                       assoc_ktype
                       |> List.map (fun lkt ->
                              lkt |> Position.value |> from_ktype
                          )
                     )
                 );
          }
    | NOperator (Unary { op; field = field, ktype; return_type; kbody }) ->
        let empty_env = Env.create_empty_env in
        RNOperator
          (RUnary
             {
               op = op.v;
               rfield = (field.v, ktype |> Position.value |> from_ktype);
               return_type = return_type.v |> from_ktype;
               kbody =
                 rkbody_of_kbody ~generics_resolver:(Hashtbl.create 0)
                   (empty_env
                   |> Env.add_fn_parameters ~const:true (field.v, ktype.v)
                   )
                   current_module prog
                   ~return_type:(return_type.v |> from_ktype)
                   kbody;
             }
          )
    | NOperator
        (Binary
          {
            op;
            fields = (field1, ktype1), (field2, ktype2);
            return_type;
            kbody;
          }
          ) ->
        let env =
          Env.create_empty_env
          |> Env.add_fn_parameters ~const:true (field1.v, ktype1.v)
          |> Env.add_fn_parameters ~const:true (field2.v, ktype2.v)
        in
        RNOperator
          (RBinary
             {
               op = ParBinOp op.v;
               rbfields =
                 ( (field1.v, from_ktype ktype1.v),
                   (field2.v, from_ktype ktype2.v)
                 );
               return_type = return_type |> Position.value |> from_ktype;
               kbody =
                 rkbody_of_kbody ~generics_resolver:(Hashtbl.create 0) env
                   current_module prog
                   ~return_type:(return_type.v |> from_ktype)
                   kbody;
             }
          )
    | NConst const_decl ->
        let generics_resolver = Hashtbl.create 0 in
        RNConst
          {
            rconst_name = const_decl.const_name.v;
            value =
              typed_expression_of_kexpression ~generics_resolver
                Env.create_empty_env current_module prog const_decl.value;
          }
    | NExternFunc external_func_decl ->
        RNExternFunc
          {
            rsig_name = external_func_decl.sig_name.v;
            fn_parameters =
              external_func_decl.fn_parameters
              |> List.map (fun lkt -> lkt.v |> from_ktype);
            return_type =
              external_func_decl.r_type |> Position.value |> from_ktype;
            is_variadic = external_func_decl.is_variadic;
            c_name = external_func_decl.c_name;
          }
    | NSyscall syscall_decl ->
        RNSyscall
          {
            rsyscall_name = syscall_decl.syscall_name.v;
            parameters =
              syscall_decl.parameters
              |> List.map (fun ktl -> ktl.v |> from_ktype);
            return_type = syscall_decl.return_type.v |> from_ktype;
            opcode = syscall_decl.opcode.v;
          }
    | NFunction { fn_name; generics; parameters; return_type; body } ->
        let generics_resolver =
          generics
          |> List.map (fun g -> (g, ()))
          |> List.to_seq |> Hashtbl.of_seq
        in
        let env =
          parameters
          |> List.fold_left
               (fun acc (para_name, ktype) ->
                 acc |> Env.add_fn_parameters ~const:false (para_name.v, ktype.v)
               )
               Env.create_empty_env
        in
        let rfuntion =
          {
            rfn_name = fn_name.v;
            generics = generics |> List.map Position.value;
            true_generics = generics <> [];
            rparameters =
              parameters
              |> List.map (fun (lf, lkt) ->
                     (lf.v, lkt |> Position.value |> from_ktype)
                 );
            return_type = from_ktype return_type.v;
            rbody =
              rkbody_of_kbody ~generics_resolver env current_module prog
                ~return_type:(return_type.v |> from_ktype)
                body;
          }
        in
        (* let () =
             Printf.printf "%s\n\n" (Asttpprint.string_of_rfunc_decl rfuntion)
           in *)
        RNFunction rfuntion

  and from_module_path module_path_list { path; _module = Mod module_nodes } =
    {
      path;
      rmodule =
        RModule
          (module_nodes
          |> List.map (fun mn -> from_module_node path module_path_list mn)
          );
    }

  and from_program (program : Ast.program) : rprogram =
    let rprogram =
      program
      |> List.map (fun { filename; module_path } ->
             {
               filename;
               rmodule_path =
                 from_module_path
                   (program |> Asthelper.Program.to_module_path_list)
                   module_path;
             }
         )
    in
    let specialised_functions =
      rprogram |> Asttyhelper.RProgram.specialise
      |> Asttyhelper.RProgram.FnSpec.to_seq |> List.of_seq
    in

    (* let () = specialised_functions |> List.iter (fun (_, fnspect) ->
         Printf.printf "specialized function : %s generics = [%s] : params = [%s]\n"
         fnspect.rfn_name
         (fnspect.rmaped_generics |> List.map Asttypprint.string_of_rktype |> String.concat ", ")
         (fnspect.rparameters |> List.map (fun (s, kt) -> Printf.sprintf "%s : %s" s (Asttypprint.string_of_rktype kt) ) |> String.concat ", ")
       ) in *)
    let rmodule =
      specialised_functions
      |> List.fold_left
           (fun acc node -> RProgram.append_function_decl node acc)
           rprogram
      |> RProgram.remove_generics |> RProgram.create_compare_function
    in

    let () = Asttysizeof.compute_all_size rmodule () in

    rmodule
end
