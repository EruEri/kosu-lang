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
open KosuFrontend.Typecheck
open KosuFrontend.Ast.Env
open KosuFrontend

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
  | TInteger (sign, size) -> RTInteger (sign, size)
  | TPointer kt -> RTPointer (kt.v |> from_ktype)
  | TTuple kts ->
      RTTuple (kts |> List.map (fun kt -> kt |> Position.value |> from_ktype))
  | TFunction (parameters, return_type) ->
      RTFunction
        ( parameters |> List.map (fun kt -> kt |> Position.value |> from_ktype),
          return_type |> Position.value |> from_ktype )
  | TString_lit -> RTString_lit
  | TClosure (params, return_type, captured_env ) -> RTClosure {
    params = params |> List.map (fun kt -> kt |> Position.value |> from_ktype);
    return_type = from_ktype return_type.v;
    captured_env = captured_env |> List.map (fun (s, kt) -> s, from_ktype kt)
  }
  | TFloat -> RTFloat
  | TBool -> RTBool
  | TUnit -> RTUnit
  | TUnknow -> RTUnknow

and to_ktype = let open Position in function
| RTUnknow -> TUnknow
| RTFloat -> TFloat
| RTBool -> TBool
| RTUnit -> TUnit
| RTString_lit -> TString_lit
| RTInteger (sign, size) -> TInteger(sign, size)
| RTPointer rkt -> TPointer ( { v = to_ktype rkt; position = dummy})
| RTTuple rkts -> TTuple (rkts |> List.map (fun rkt -> { v = to_ktype rkt; position = dummy} ))
| RTFunction (parameters, return_type) -> TFunction (parameters |> List.map (fun kt -> val_dummy @@ to_ktype kt), val_dummy @@ to_ktype return_type)
| RTClosure {params; return_type; captured_env} ->
  TClosure (
    params |> List.map (fun rkt -> val_dummy @@ to_ktype rkt),
    val_dummy @@ to_ktype return_type,
    captured_env |> List.map (fun (s, kt) -> s, to_ktype kt) 
  )
| RTType_Identifier {module_path; name } -> TType_Identifier {module_path = val_dummy module_path; name = val_dummy name}
| RTParametric_identifier {module_path; parametrics_type; name} -> TParametric_identifier {
  module_path = val_dummy module_path;
  parametrics_type = List.map (fun kt -> val_dummy @@ to_ktype kt) parametrics_type;
  name = val_dummy name
}
| RTNamedTuple _ -> failwith "No matching from namedTuple astyyped to ast"

and from_switch_case = function
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
        |> typeof ~generics_resolver ~lambda_type:(hint_type |> to_ktype |> Position.val_dummy |> Option.some) env current_mod_name prog
        |> from_ktype)
        hint_type;
    rexpression =
      from_kexpression ~generics_resolver env current_mod_name prog ~hint_type
        expression.v;
  }

and rkbody_of_kbody ~generics_resolver (env : Env.t) current_module
    (program : module_path list) ~return_type
    ( (kstatements : Ast.kstatement Position.location list),
      (kexpression : kexpression Position.location) ) =
  let open Position in
  match kstatements with
  | kstatement :: q -> (
      match kstatement.v with
      | SDiscard expr ->
          let rktype =
            typeof ~generics_resolver env current_module program expr
            |> from_ktype
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
            typeof ~lambda_type:explicit_type ~generics_resolver env current_module program expression
          in
          let () = Printf.printf "TYPEOF %s : %s\n" variable_name.v (KosuFrontend.Pprint.string_of_ktype type_of_expression) in
          let variable_type =
            match explicit_type with
            | None -> type_of_expression
            | Some explicit_type -> if Ast.Type.is_type_full_known type_of_expression then type_of_expression else explicit_type.v
          in
          let typed_expression =
            typed_expression_of_kexpression ~generics_resolver env
              current_module program
              ~hint_type:(variable_type |> from_ktype)
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
            future_expr )
      | SAffection (variable, expression) ->
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
          ( RSAffection (variable.v, typed_expression) :: stmts_remains,
            future_expr )
      | SDerefAffectation (id, expression) ->
          let { is_const = _; ktype } =
            env |> Env.find_identifier_opt id.v |> Option.get
          in
          let ktype =
            Type.restrict_type (Type.pointee_fail ktype)
              (expression
              |> typeof ~generics_resolver env current_module program)
          in
          let rktype = from_ktype ktype in
          let stmts_remains, future_expr =
            rkbody_of_kbody ~generics_resolver env current_module program
              ~return_type (q, kexpression)
          in
          ( RSDerefAffectation
              ( id.v,
                {
                  rktype;
                  rexpression =
                    from_kexpression ~generics_resolver env current_module
                      program ~hint_type:rktype expression.v;
                } )
            :: stmts_remains,
            future_expr ))
  | [] ->
      let rktype =
        match return_type with
        | RTUnknow ->
            kexpression
            |> typeof ~generics_resolver env current_module program
            |> from_ktype
        | kt -> kt
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
  | Empty -> REmpty
  | True -> RTrue
  | False -> RFalse
  | ENullptr -> RENullptr
  | EInteger (sign, size, value) -> REInteger (sign, size, value)
  | EFloat float -> REFloat float
  | ESizeof either ->
      let ktype =
        match either with
        | Either.Left ktype -> ktype.v
        | Either.Right kexpression ->
            typeof ~generics_resolver env current_module program kexpression
      in
      let rktype = from_ktype ktype in
      RESizeof rktype
  | EString s -> REstring s
  | EAdress e -> REAdress e.v
  | EDeference (c, id) -> REDeference (c, id.v)
  | EIdentifier { modules_path; identifier } ->
      REIdentifier { modules_path = modules_path.v; identifier = identifier.v }
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
      REStruct
        {
          modules_path = modules_path.v;
          struct_name = struct_name.v;
          fields =
            fields
            |> List.map (fun (field, expr) ->
                   ( field.v,
                     typed_expression_of_kexpression ~generics_resolver env
                       current_module program expr ));
        }
  | ELambda {params; kbody} -> 
    let open Env in
    let params_explicit_type, explicit_type_return_type = 
      match hint_type with
      | RTFunction (lambad_param_explit, r) | RTClosure {params = lambad_param_explit; return_type = r; _} -> Some lambad_param_explit, Some r
      | _ -> failwith "Todo Error: R Explicit type should be an function" in 
      let paramas_typed =
         match params_explicit_type with
         | None -> params |> List.map (fun (s, kt_loc_option) -> 
          s.v, from_ktype (match kt_loc_option with Some kt_loc -> kt_loc | None -> failwith "Asttyped Need explicit type on parameters or statement").v
          )
          | Some lambda_param_explits -> 
          let () = if Util.are_diff_lenght lambda_param_explits params then failwith "Arity between parameters and explicit type is wrong" else () in
            params |> List.combine lambda_param_explits |> List.map (fun (given_statement_type, (s, kt_loc_opt)) -> 
              let rkt = match kt_loc_opt with
                | None -> given_statement_type
                | Some _ (* RTy .are_compatible_type given_statement_type.v kt_loc.v *) -> given_statement_type
                (* | Some _ -> failwith "Incomptabile type between explicit instatement en inner lambda" *)
              in
              s.v , rkt
            )
          in
    let () = Printf.printf "\nhint = %s\n%!" (Asttypprint.string_of_rktype hint_type) in
    let clo_env = env |> Env.push_context (paramas_typed |> List.map (fun (s, kt) -> 
            s, {is_const = true; ktype = to_ktype kt}
      )) in

    let body_ktype = rkbody_of_kbody ~generics_resolver clo_env current_module program ~return_type:(explicit_type_return_type |> Option.value ~default:RTUnknow )  kbody in
    let captured_var = free_variable_kbody ~closure_env:(paramas_typed |> List.map (fst) ) ~scope_env:env kbody in
    let captured_var = captured_var |> List.map (fun (s, kt) -> s, from_ktype kt) in
    RELambda {
      clofn_name = None;
      parameters = paramas_typed;
      body = body_ktype;
      captured_env = captured_var;
      return_ktype = (explicit_type_return_type |> function
      | None -> failwith "Hint type must be set for closure"
      | Some kt -> kt;
      );
    }
  | EEnum { modules_path; enum_name; variant; assoc_exprs } ->
      REEnum
        {
          modules_path = modules_path.v;
          enum_name = enum_name |> Option.map Position.value;
          variant = variant.v;
          assoc_exprs =
            assoc_exprs
            |> List.map
                 (typed_expression_of_kexpression ~generics_resolver env
                    current_module program);
        }
  | ETuple exprs ->
      RETuple
        (exprs
        |> List.map
             (typed_expression_of_kexpression ~generics_resolver env
                current_module program))
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
                    current_module program);
        }
  | EWhile (condition, body) ->
      let typed_condition =
        typed_expression_of_kexpression ~generics_resolver env current_module
          program condition
      in
      let rkbody =
        rkbody_of_kbody ~generics_resolver ~return_type:hint_type
          (Env.push_context [] env) current_module program body
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
            current_module program ~return_type:hint_type else_block )
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
                          current_module ~return_type:hint_type program ));
          else_case =
            else_case
            |> rkbody_of_kbody ~generics_resolver
                 (env |> Env.push_context [])
                 current_module ~return_type:hint_type program;
        }
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
                          }))
      in
      let expr_type =
        typeof ~generics_resolver env current_module program expression
      in

      let module_path, name =
        expr_type |> Asthelper.module_path_of_ktype_opt |> Option.get
      in
      let enum_decl =
        match
          Asthelper.Program.find_type_decl_from_ktype
            ~ktype_def_path:module_path ~ktype_name:name ~current_module program
        with
        | Ok (Type_Decl.Decl_Enum e) -> e
        | _ -> failwith "Wierd it supposed to be an enum"
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
                          |> List.mapi (fun index (v, l) -> (index, v, l)) ))
               in
               match combine_binding_type with
               | [] -> failwith "Unreachable case: empty case"
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
                            | None -> acc
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
                                    (wrong_index, wrong_bound_id) )) ->
                                Incompatible_Binding_Position
                                  {
                                    base_index;
                                    base_variant = first_variant;
                                    base_bound_id;
                                    wrong_index;
                                    wrong_variant = variant_name;
                                    wrong_bound_id;
                                  }
                                |> switch_error |> raise)
                          (reduce_binded_variable_combine ass_bin)
                     |> List.map (fun (i, variable_name, ktype) ->
                            ( i,
                              ( variable_name,
                                ({ is_const = true; ktype = ktype.v }
                                  : Env.variable_info) ) ))
                   in
                   let new_variable_info = new_conext |> List.split |> snd in
                   ( new_conext,
                     rkbody_of_kbody ~generics_resolver
                       (env
                       |> Env.push_context
                            (new_variable_info
                            |> List.map Position.assoc_value_left))
                       current_module program ~return_type:hint_type kb ))
        |> List.split
      in
      let bound_variables =
        bound_variables
        |> List.map
             (List.map (fun (index, (bound_varn, variable_info)) ->
                  ( index,
                    bound_varn.v,
                    variable_info |> Env.vi_ktype |> from_ktype )))
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
                    (env |> Env.push_context [])
                    current_module ~return_type:hint_type program);
        }
  | EFunction_call
      { modules_path; generics_resolver = grc; fn_name; parameters } -> (
      let fn_decl =
        Asthelper.Program.find_function_decl_from_fn_name modules_path fn_name
          current_module program
        |> Result.get_ok
      in

      match fn_decl with
      | Ast.Function_Decl.Decl_Syscall
          { syscall_name = _; parameters = sys_type_parameters; _ } ->
          let sys_rktype_parameters =
            sys_type_parameters
            |> List.map (fun stl -> stl |> Position.value |> from_ktype)
          in
          let typed_parameters =
            parameters
            |> List.map
                 (typed_expression_of_kexpression ~generics_resolver env
                    current_module program)
          in
          let mapped =
            List.map2 restrict_typed_expression sys_rktype_parameters
              typed_parameters
          in
          REFunction_call
            {
              modules_path = modules_path.v;
              generics_resolver = None;
              fn_name = fn_name.v;
              parameters = mapped;
            }
      | Ast.Function_Decl.Decl_External
          { sig_name = _; fn_parameters; is_variadic; _ } ->
          let mapped =
            if is_variadic then
              let known_parameters_len = fn_parameters |> List.length in
              let external_rktype_parameters =
                fn_parameters
                |> List.map (fun stl -> stl |> Position.value |> from_ktype)
              in
              parameters
              |> List.mapi (fun i expr ->
                     ( i,
                       typed_expression_of_kexpression ~generics_resolver env
                         current_module program expr ))
              |> List.partition (fun (index, _) -> index < known_parameters_len)
              |> fun (mappable, variadic) ->
              List.map2
                (fun rktype (i, typed_expr) ->
                  (i, restrict_typed_expression rktype typed_expr))
                external_rktype_parameters mappable
              @ variadic
              |> List.map snd
            else
              let external_rktype_parameters =
                fn_parameters
                |> List.map (fun stl -> stl |> Position.value |> from_ktype)
              in
              let typed_parameters =
                parameters
                |> List.map
                     (typed_expression_of_kexpression ~generics_resolver env
                        current_module program)
              in
              List.map2 restrict_typed_expression external_rktype_parameters
                typed_parameters
          in
          REFunction_call
            {
              modules_path = modules_path.v;
              generics_resolver = None;
              fn_name = fn_name.v;
              parameters = mapped;
            }
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
            parameters
            |> List.map
                 (typed_expression_of_kexpression
                    ~generics_resolver:new_map_generics env current_module
                    program)
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
                ())
              typed_parameters function_rktype_param
          in
          let typed_parameters =
            kosu_function.parameters
            |> List.map (fun (_, { v = kt; _ }) ->
                   kt |> from_ktype |> RType.remap_generic_ktype hashmap)
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
            })
  | EUn_op (UMinus expression) ->
      let typed =
        typed_expression_of_kexpression ~generics_resolver env current_module
          program expression
      in

      if typed.rktype |> RType.is_builtin_type then REUn_op (RUMinus typed)
      else REUnOperator_Function_call (RUMinus typed)
  | EUn_op (UNot expression) ->
      let typed =
        typed_expression_of_kexpression ~generics_resolver env current_module
          program expression
      in
      let runot = RUNot typed in
      if typed.rktype |> RType.is_builtin_type then REUn_op runot
      else REUnOperator_Function_call runot
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
      in
      let lhs, rhs = Binop.operands rkbin in
      if
        lhs.rktype |> RType.is_builtin_type |> not
        || rhs.rktype |> RType.is_builtin_type |> not
      then REBinOperator_Function_call rkbin
      else REBin_op rkbin

and from_module_node current_module (prog : module_path list) =
  let open Position in
  function
  | NStruct { struct_name; generics; fields } ->
      RNStruct
        {
          rstruct_name = struct_name.v;
          generics = generics |> List.map Position.value;
          rfields =
            fields
            |> List.map (fun (field, ktype) ->
                   (field.v, ktype |> Position.value |> from_ktype));
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
                            lkt |> Position.value |> from_ktype) ));
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
                 |> Env.add_fn_parameters ~const:true (field.v, ktype.v))
                 current_module prog
                 ~return_type:(return_type.v |> from_ktype)
                 kbody;
           })
  | NOperator
      (Binary
        { op; fields = (field1, ktype1), (field2, ktype2); return_type; kbody })
    ->
      let env =
        Env.create_empty_env
        |> Env.add_fn_parameters ~const:true (field1.v, ktype1.v)
        |> Env.add_fn_parameters ~const:true (field2.v, ktype2.v)
      in
      RNOperator
        (RBinary
           {
             op = op.v;
             rfields =
               ((field1.v, from_ktype ktype1.v), (field2.v, from_ktype ktype2.v));
             return_type = return_type |> Position.value |> from_ktype;
             kbody =
               rkbody_of_kbody ~generics_resolver:(Hashtbl.create 0) env
                 current_module prog
                 ~return_type:(return_type.v |> from_ktype)
                 kbody;
           })
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
            syscall_decl.parameters |> List.map (fun ktl -> ktl.v |> from_ktype);
          return_type = syscall_decl.return_type.v |> from_ktype;
          opcode = syscall_decl.opcode.v;
        }
  | NFunction { fn_name; generics; parameters; return_type; body } ->
      let generics_resolver =
        generics |> List.map (fun g -> (g, ())) |> List.to_seq |> Hashtbl.of_seq
      in
      let env =
        parameters
        |> List.fold_left
             (fun acc (para_name, ktype) ->
               acc |> Env.add_fn_parameters ~const:false (para_name.v, ktype.v))
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
                   (lf.v, lkt |> Position.value |> from_ktype));
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
        |> List.map (fun mn -> from_module_node path module_path_list mn));
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
           })
  in
  let specialised_functions =
    rprogram |> Asttyhelper.RProgram.specialise
    |> Asttyhelper.RProgram.FnSpec.to_seq |> List.of_seq
  in

  specialised_functions
  |> List.fold_left
       (fun acc node -> RProgram.append_function_decl node acc)
       rprogram
  |> RProgram.remove_generics
  |> Closure.generate_closure_from_lambda
