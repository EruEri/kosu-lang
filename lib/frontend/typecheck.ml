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

open Ast
open Ast.Error
open Position
open Pprint

module type TypeCheckerRuleS = sig
  val allow_generics_in_variadic : bool
end

module Make (Rule : TypeCheckerRuleS) = struct
  (**
  Return the type of the code block expression by checking each expression in this one
  @raise Ast_error
  @raise No_Occurence : if a type declartion wasn't found or a variant is not in enum variants
  @raise Too_Many_Occurence: if several type declarations matching was found
*)
  let validate_type ~constraint_type ktype =
    match constraint_type with
    | None ->
        ktype.v
    | Some cstr_kt -> (
        match Ast.Type.are_compatible_type cstr_kt ktype.v with
        | true ->
            Type.restrict_type cstr_kt ktype.v
        | false ->
            raise @@ stmt_error
            @@ Ast.Error.Uncompatible_type_Assign
                 { expected = cstr_kt; found = ktype }
      )

  let validate_position_type position ~constraint_type ktype =
    validate_type ~constraint_type { v = ktype; position }

  let validate_location_type location ~constraint_type ktype =
    validate_type ~constraint_type { v = ktype; position = location.position }

  let rec typeof_kbody ~generics_resolver (env : Env.t)
      (current_mod_name : string) (program : module_path list)
      ?(return_type = None) (kbody : kbody) =
    (* let () = Printf.printf "env %s\n" (Pprint.string_of_env env) in *)
    let statements, final_expr = kbody in
    match statements with
    | stamement :: q -> (
        match stamement.v with
        | SDiscard expr ->
            ignore
              (typeof ~constraint_type:None ~generics_resolver env
                 current_mod_name program expr
              );
            typeof_kbody ~generics_resolver env current_mod_name program
              ~return_type (q, final_expr)
        | SDeclaration { is_const; variable_name; explicit_type; expression } ->
            let type_init =
              expression
              |> Position.map_use
                   (typeof
                      ~constraint_type:(Option.map Position.value explicit_type)
                      ~generics_resolver env current_mod_name program
                   )
            in
            (* let () = Printf.printf "sizeof %s : %Lu\nalignement : %Lu\n" (Pprint.string_of_ktype type_init.v) (Asthelper.Sizeof.sizeof current_mod_name program type_init.v) (Asthelper.Sizeof.alignmentof current_mod_name program type_init.v) in *)
            if env |> Env.is_identifier_exists variable_name.v then
              raise
                (stmt_error
                   (Ast.Error.Already_Define_Identifier { name = variable_name })
                )
            else
              let kt =
                match explicit_type with
                | None ->
                    if Ast.Type.is_type_full_known type_init.v |> not then
                      Need_explicit_type_declaration
                        { variable_name; infer_type = type_init.v }
                      |> stmt_error |> raise
                    else
                      type_init
                | Some explicit_type_sure ->
                    let () =
                      if
                        not
                          (Type.are_compatible_type explicit_type_sure.v
                             type_init.v
                          )
                      then
                        raise @@ stmt_error
                        @@ Ast.Error.Uncompatible_type_Assign
                             {
                               expected = explicit_type_sure.v;
                               found = type_init;
                             }
                    in
                    explicit_type_sure
              in
              typeof_kbody ~generics_resolver
                (env
                |> Env.add_variable (variable_name.v, { is_const; ktype = kt.v })
                )
                current_mod_name program ~return_type (q, final_expr)
        | SAffection (affected_value, expr) -> (
            match affected_value with
            | AFVariable variable ->
                let is_const, ktype =
                  match env |> Env.find_identifier_opt variable.v with
                  | None ->
                      raise
                        (stmt_error
                           (Ast.Error.Undefine_Identifier { name = variable })
                        )
                  | Some { is_const; ktype } ->
                      (is_const, ktype)
                in
                let () =
                  if is_const then
                    raise @@ stmt_error
                    @@ Ast.Error.Reassign_Constante { name = variable }
                in
                let new_type =
                  typeof ~constraint_type:(Some ktype) ~generics_resolver env
                    current_mod_name program expr
                in
                let () =
                  if not (Ast.Type.are_compatible_type new_type ktype) then
                    raise @@ stmt_error
                    @@ Ast.Error.Uncompatible_type_Assign
                         {
                           expected = ktype;
                           found = expr |> Position.map (fun _ -> new_type);
                         }
                in
                typeof_kbody ~generics_resolver
                  (env |> Env.restrict_variable_type variable.v new_type)
                  current_mod_name program ~return_type (q, final_expr)
            | AFField { variable; fields } ->
                let is_const, ktype =
                  match env |> Env.find_identifier_opt variable.v with
                  | None ->
                      raise
                        (stmt_error
                           (Ast.Error.Undefine_Identifier { name = variable })
                        )
                  | Some { is_const; ktype } ->
                      (is_const, ktype)
                in
                let () =
                  if is_const then
                    raise @@ stmt_error
                    @@ Ast.Error.Reassign_Constante_Struct_field
                         { name = variable }
                in
                let field_type =
                  Asthelper.Affected_Value.field_type ~variable ktype
                    current_mod_name program fields
                in
                let new_type =
                  typeof ~constraint_type:(Some field_type) ~generics_resolver
                    env current_mod_name program expr
                in
                let () =
                  if not @@ Ast.Type.are_compatible_type new_type field_type
                  then
                    raise @@ stmt_error
                    @@ Ast.Error.Uncompatible_type_Assign
                         {
                           expected = field_type;
                           found = expr |> Position.map (fun _ -> new_type);
                         }
                in
                typeof_kbody ~generics_resolver
                  (env |> Env.restrict_variable_type variable.v new_type)
                  current_mod_name program ~return_type (q, final_expr)
          )
        | SDerefAffectation (affected_value, expression) -> (
            match affected_value with
            | AFVariable id -> (
                match env |> Env.find_identifier_opt id.v with
                | None ->
                    Ast.Error.Undefine_Identifier { name = id }
                    |> stmt_error |> raise
                | Some { ktype; _ } ->
                    let in_pointer_ktype =
                      match ktype with
                      | TPointer ktl ->
                          ktl.v
                      | _ ->
                          raise @@ stmt_error
                          @@ Ast.Error.Dereference_No_pointer
                               { name = id; ktype }
                    in
                    let expr_ktype =
                      expression
                      |> Position.map_use
                           (typeof ~constraint_type:(Some in_pointer_ktype)
                              ~generics_resolver env current_mod_name program
                           )
                    in
                    let () =
                      if
                        not
                        @@ Ast.Type.are_compatible_type in_pointer_ktype
                        @@ expr_ktype.v
                      then
                        Ast.Error.Dereference_Wrong_type
                          {
                            identifier = id;
                            expected = in_pointer_ktype;
                            found = expr_ktype;
                          }
                        |> stmt_error |> raise
                    in
                    typeof_kbody ~generics_resolver
                      (env |> Env.restrict_variable_type id.v expr_ktype.v)
                      current_mod_name program ~return_type (q, final_expr)
              )
            | AFField { variable; fields } ->
                let ktype =
                  match env |> Env.find_identifier_opt variable.v with
                  | None ->
                      raise @@ stmt_error
                      @@ Ast.Error.Undefine_Identifier { name = variable }
                  | Some { ktype; _ } ->
                      ktype
                in
                let in_pointer_ktype =
                  match ktype with
                  | TPointer ktl ->
                      ktl.v
                  | _ ->
                      Ast.Error.Dereference_No_pointer
                        { name = variable; ktype }
                      |> stmt_error |> raise
                in
                let field_type =
                  Asthelper.Affected_Value.field_type ~variable in_pointer_ktype
                    current_mod_name program fields
                in
                let expr_ktype =
                  expression
                  |> Position.map_use
                       (typeof ~constraint_type:(Some field_type)
                          ~generics_resolver env current_mod_name program
                       )
                in
                let () =
                  if
                    not
                    @@ Ast.Type.are_compatible_type field_type
                    @@ expr_ktype.v
                  then
                    Uncompatible_type_Assign
                      { expected = field_type; found = expr_ktype }
                    |> stmt_error |> raise
                in
                typeof_kbody ~generics_resolver env current_mod_name program
                  ~return_type (q, final_expr)
          )
      )
    | [] -> (
        (* Printf.printf "Final expr\n"; *)
        let final_expr_type =
          typeof ~constraint_type:return_type ~generics_resolver env
            current_mod_name program final_expr
        in
        match return_type with
        | None ->
            final_expr_type
        | Some kt ->
            if not (Type.are_compatible_type kt final_expr_type) then
              raise
                (ast_error
                   (Ast.Error.Uncompatible_type
                      {
                        expected = kt;
                        found =
                          {
                            v = final_expr_type;
                            position = final_expr.position;
                          };
                      }
                   )
                )
            else
              kt
      )

  and typeof_statement ~generics_resolver (env : Env.t)
      (current_mod_name : string) (program : module_path list) stamement =
    match stamement with
    | SDiscard expr ->
        ( env,
          typeof ~constraint_type:None ~generics_resolver env current_mod_name
            program expr
        )
    | SDeclaration { is_const; variable_name; explicit_type; expression } ->
        let type_init =
          expression
          |> Position.map_use
               (typeof
                  ~constraint_type:(Option.map Position.value explicit_type)
                  ~generics_resolver env current_mod_name program
               )
        in
        (* let () = Printf.printf "sizeof %s : %Lu\nalignement : %Lu\n" (Pprint.string_of_ktype type_init.v) (Asthelper.Sizeof.sizeof current_mod_name program type_init.v) (Asthelper.Sizeof.alignmentof current_mod_name program type_init.v) in *)
        let () =
          if env |> Env.is_identifier_exists variable_name.v then
            raise @@ stmt_error
            @@ Ast.Error.Already_Define_Identifier { name = variable_name }
        in
        let kt =
          match explicit_type with
          | None ->
              let () =
                if not @@ Ast.Type.is_type_full_known type_init.v then
                  raise @@ stmt_error
                  @@ Need_explicit_type_declaration
                       { variable_name; infer_type = type_init.v }
              in
              type_init
          | Some explicit_type_sure ->
              let () =
                if
                  not
                  @@ Type.are_compatible_type explicit_type_sure.v type_init.v
                then
                  raise
                    (Ast.Error.Uncompatible_type_Assign
                       { expected = explicit_type_sure.v; found = type_init }
                    |> stmt_error |> raise
                    )
              in
              explicit_type_sure
        in
        ( Env.add_variable (variable_name.v, { is_const; ktype = kt.v }) env,
          kt.v
        )
    | SAffection (affected_value, expr) -> (
        match affected_value with
        | AFVariable variable ->
            let is_const, ktype =
              match env |> Env.find_identifier_opt variable.v with
              | None ->
                  raise
                    (stmt_error
                       (Ast.Error.Undefine_Identifier { name = variable })
                    )
              | Some { is_const; ktype } ->
                  (is_const, ktype)
            in
            let () =
              if is_const then
                raise @@ stmt_error
                @@ Ast.Error.Reassign_Constante { name = variable }
            in
            let new_type =
              typeof ~constraint_type:(Some ktype) ~generics_resolver env
                current_mod_name program expr
            in
            let () =
              if not @@ Ast.Type.are_compatible_type new_type ktype then
                raise
                  (stmt_error
                     (Ast.Error.Uncompatible_type_Assign
                        {
                          expected = ktype;
                          found = expr |> Position.map (fun _ -> new_type);
                        }
                     )
                  )
            in
            let extended_env =
              Env.restrict_variable_type variable.v new_type env
            in
            ( extended_env,
              Env.vi_ktype @@ Option.get
              @@ Env.find_identifier_opt variable.v extended_env
            )
        | AFField { variable; fields } -> (
            match env |> Env.find_identifier_opt variable.v with
            | None ->
                raise
                  (stmt_error
                     (Ast.Error.Undefine_Identifier { name = variable })
                  )
            | Some { is_const; ktype } ->
                let () =
                  if is_const then
                    raise @@ stmt_error
                    @@ Ast.Error.Reassign_Constante_Struct_field
                         { name = variable }
                in
                let field_type =
                  Asthelper.Affected_Value.field_type ~variable ktype
                    current_mod_name program fields
                in
                let new_type =
                  typeof ~constraint_type:(Some ktype) ~generics_resolver env
                    current_mod_name program expr
                in
                let () =
                  if not (Ast.Type.are_compatible_type new_type field_type) then
                    raise @@ stmt_error
                    @@ Ast.Error.Uncompatible_type_Assign
                         {
                           expected = field_type;
                           found = expr |> Position.map (fun _ -> new_type);
                         }
                in
                let extended_env =
                  Env.restrict_variable_type variable.v new_type env
                in
                ( extended_env,
                  Env.vi_ktype @@ Option.get
                  @@ Env.find_identifier_opt variable.v extended_env
                )
          )
      )
    | SDerefAffectation (affected_value, expression) -> (
        match affected_value with
        | AFVariable id -> (
            match env |> Env.find_identifier_opt id.v with
            | None ->
                Ast.Error.Undefine_Identifier { name = id }
                |> stmt_error |> raise
            | Some { ktype; _ } ->
                let in_pointer_ktype =
                  match ktype with
                  | TPointer ktl ->
                      ktl.v
                  | _ ->
                      Ast.Error.Dereference_No_pointer { name = id; ktype }
                      |> stmt_error |> raise
                in
                let expr_ktype =
                  expression
                  |> Position.map_use
                       (typeof ~constraint_type:(Some in_pointer_ktype)
                          ~generics_resolver env current_mod_name program
                       )
                in
                if
                  not
                  @@ Ast.Type.are_compatible_type in_pointer_ktype
                  @@ expr_ktype.v
                then
                  Ast.Error.Dereference_Wrong_type
                    {
                      identifier = id;
                      expected = in_pointer_ktype;
                      found = expr_ktype;
                    }
                  |> stmt_error |> raise
                else
                  let extended_env =
                    Env.restrict_variable_type id.v expr_ktype.v env
                  in
                  ( extended_env,
                    Env.vi_ktype @@ Option.get
                    @@ Env.find_identifier_opt id.v extended_env
                  )
          )
        | AFField { variable; fields } -> (
            match env |> Env.find_identifier_opt variable.v with
            | None ->
                Ast.Error.Undefine_Identifier { name = variable }
                |> stmt_error |> raise
            | Some { ktype; _ } ->
                let in_pointer_ktype =
                  match ktype with
                  | TPointer ktl ->
                      ktl.v
                  | _ ->
                      Ast.Error.Dereference_No_pointer
                        { name = variable; ktype }
                      |> stmt_error |> raise
                in
                let field_type =
                  Asthelper.Affected_Value.field_type ~variable in_pointer_ktype
                    current_mod_name program fields
                in
                let expr_ktype =
                  expression
                  |> Position.map_use
                       (typeof ~constraint_type:(Some in_pointer_ktype)
                          ~generics_resolver env current_mod_name program
                       )
                in
                if
                  not @@ Ast.Type.are_compatible_type field_type @@ expr_ktype.v
                then
                  Uncompatible_type_Assign
                    { expected = field_type; found = expr_ktype }
                  |> stmt_error |> raise
                else
                  (env, expr_ktype.v)
          )
      )

  (**
Return the type of an expression
@raise Ast_error
@raise No_Occurence : if a type declartion wasn't found or a variant is not in enum variants
@raise Too_Many_Occurence: if several type declarations matching was found
*)
  and typeof ~constraint_type ~generics_resolver (env : Env.t)
      (current_mod_name : string) (prog : module_path list)
      (expression : kexpression location) =
    match expression.v with
    | Empty ->
        validate_location_type expression ~constraint_type TUnit
    | True | False ->
        validate_location_type expression ~constraint_type TBool
    | ECmpLess | ECmpEqual | ECmpGreater ->
        validate_location_type expression ~constraint_type TOredered
    | ENullptr ->
        validate_location_type expression ~constraint_type
        @@ TPointer { v = TUnknow; position = expression.position }
    | EInteger (sign_size, _) ->
        validate_location_type expression ~constraint_type @@ TInteger sign_size
    | EChar _ ->
        validate_location_type expression ~constraint_type TChar
    | EFloat (fsize, _) ->
        validate_location_type expression ~constraint_type @@ TFloat fsize
    | ESizeof either ->
        let () =
          match either with
          | Left ktype ->
              ignore
                ( match ktype.v with
                | TParametric_identifier
                    { module_path; parametrics_type = _; name }
                | TType_Identifier { module_path; name } -> (
                    match
                      Asthelper.Program.find_type_decl_from_ktype
                        ~ktype_def_path:module_path ~ktype_name:name
                        ~current_module:current_mod_name prog
                    with
                    | Ok _ ->
                        ignore ()
                    | Error (Undefine_Type _ as e) when module_path.v = "" -> (
                        match
                          generics_resolver |> Hashtbl.to_seq_keys
                          |> Seq.find (fun gen_loc -> gen_loc.v = name.v)
                        with
                        | Some _ ->
                            ignore ()
                        | None ->
                            e |> Ast.Error.ast_error |> raise
                      )
                    | Error e ->
                        e |> Ast.Error.ast_error |> raise
                  )
                | _ ->
                    ignore ()
                )
          | Right expr ->
              ignore
                (typeof ~constraint_type:None ~generics_resolver env
                   current_mod_name prog expr
                )
        in
        validate_location_type expression ~constraint_type
        @@ TInteger (Some (Unsigned, I64))
    | EString _ ->
        validate_location_type expression ~constraint_type @@ TString_lit
    | EAdress s ->
        validate_location_type expression ~constraint_type
          (env |> Env.flat_context |> List.assoc_opt s.v
          |> Option.map (fun (t : Env.variable_info) ->
                 TPointer { v = t.ktype; position = s.position }
             )
          |> function
          | None -> raise (ast_error (Undefined_Identifier s)) | Some s -> s
          )
    | EDeference (indirection_count, id) -> (
        let rec loop count ktype =
          match count with
          | 0 ->
              ktype
          | s -> (
              match ktype with
              | Ast.TPointer t ->
                  loop (s - 1) t.v
              | _ ->
                  raise (ast_error (Unvalid_Deference id))
            )
        in
        validate_location_type expression ~constraint_type
        @@
        match env |> Env.flat_context |> List.assoc_opt id.v with
        | None ->
            raise @@ ast_error @@ Undefined_Identifier id
        | Some t ->
            loop indirection_count t.ktype
      )
    | EIdentifier { modules_path = _; identifier } ->
        validate_location_type expression ~constraint_type
          (env |> Env.flat_context
          |> List.assoc_opt identifier.v
          |> Option.map (fun (var_info : Env.variable_info) -> var_info.ktype)
          |> function
          | None ->
              raise (ast_error (Undefined_Identifier identifier))
          | Some s ->
              s
          )
    | EConst_Identifier { modules_path; identifier } ->
        validate_location_type expression ~constraint_type
          (let consts_opt =
             ( if modules_path.v = "" then
                 Some
                   (prog |> Asthelper.Program.module_of_string current_mod_name)
               else
                 prog |> Asthelper.Program.module_of_string_opt modules_path.v
             )
             |> Option.map Asthelper.Module.retrieve_const_decl
           in

           match consts_opt with
           | None ->
               raise (ast_error (Unbound_Module modules_path))
           | Some consts -> (
               consts
               |> List.find_map (fun c ->
                      if c.const_name.v = identifier.v then
                        Some c.explicit_type
                      else
                        None
                  )
               |> function
               | None ->
                   raise (ast_error (Undefined_Const identifier))
               | Some s ->
                   s
             )
          )
    | EFieldAcces { first_expr; field } ->
        let first_type =
          typeof ~constraint_type:None ~generics_resolver env current_mod_name
            prog first_expr
        in
        (* let () = Printf.printf "ktype %s = %s\n%!" (Pprint.string_of_kexpression first_expr.v) (Pprint.string_of_ktype first_type) in *)
        let parametrics_types = Type.extract_parametrics_ktype first_type in
        let ktype =
          match Type.module_path_of_ktype_opt first_type with
          | None ->
              raise @@ ast_error
              @@ Field_access_for_non_struct_type
                   { location = first_expr.position; ktype = first_type }
          | Some (ktype_def_path, ktype_name) ->
              let type_decl =
                match
                  Asthelper.Program.find_type_decl_from_ktype ~ktype_def_path
                    ~ktype_name ~current_module:current_mod_name prog
                with
                | Error e ->
                    e |> Ast.Error.ast_error |> raise
                | Ok type_decl ->
                    type_decl
              in
              let ktype =
                Asthelper.Struct.resolve_fields_access_gen
                  (parametrics_types |> List.map Position.value)
                  field type_decl current_mod_name
              in
              (* Printf.sprintf "ktype = %s" (Pprint.string_of_ktype ktype) |> prerr_endline; *)
              ktype
        in
        validate_location_type expression ~constraint_type ktype
    | EArrayAccess { array_expr; index_expr } ->
        let array_ktype =
          array_expr
          |> Position.map_use
             @@ typeof ~constraint_type:None ~generics_resolver env
                  current_mod_name prog
        in
        let index_ktype =
          index_expr
          |> Position.map_use
             @@ typeof ~constraint_type:None ~generics_resolver env
                  current_mod_name prog
        in
        let array_of_type =
          match array_ktype.v with
          | TArray { ktype; _ } ->
              ktype
          | _ ->
              raise @@ ast_error
              @@ Array_subscript_None_array { found = array_ktype }
        in

        let () =
          match Type.is_any_integer index_ktype.v with
          | false ->
              raise @@ ast_error
              @@ Array_Non_Integer_Index { found = index_ktype }
          | true ->
              ()
        in

        validate_location_type expression ~constraint_type array_of_type.v
    | ETupleAccess { first_expr; index } ->
        let first_type =
          typeof ~constraint_type:None ~generics_resolver env current_mod_name
            prog first_expr
        in
        let kts =
          match first_type with
          | TTuple kts ->
              kts
          | _ ->
              raise @@ ast_error
              @@ Tuple_access_for_non_tuple_type
                   { location = first_expr.position; ktype = first_type }
        in
        let length = Int64.of_int @@ List.length kts in
        let ucmp = Int64.unsigned_compare length index.v in
        let () =
          if ucmp <= 0 then
            raise @@ ast_error
            @@ Impossible_tuple_access { index; ktypes = kts }
        in
        let kt =
          match List.nth_opt kts @@ Int64.to_int index.v with
          | None ->
              failwith "Shouldn't append: except with index > max(int)"
          | Some kt ->
              kt
        in
        validate_location_type expression ~constraint_type kt.v
    | EStruct { modules_path; struct_name; fields } ->
        let struct_decl =
          match
            Asthelper.Program.find_struct_decl_opt current_mod_name modules_path
              struct_name prog
          with
          | Ok str ->
              str
          | Error e ->
              e |> ast_error |> raise
        in

        let parameters_length = List.length fields in
        let expected_length = List.length struct_decl.fields in
        let () =
          if parameters_length <> expected_length then
            raise
              (Ast.Error.struct_error
                 (Wrong_field_count
                    {
                      struct_name;
                      expected = expected_length;
                      found = parameters_length;
                    }
                 )
              )
        in
        let init_types, initialisation_types =
          fields
          |> List.map (fun (s, expr) ->
                 let type_of_field =
                   match
                     Asthelper.Struct.is_ktype_generic_field s struct_decl
                   with
                   | None ->
                       failwith "Unknown field"
                   | Some ktopt ->
                       ktopt
                 in
                 let loc_type =
                   expr
                   |> Position.map_use
                        (typeof
                           ~constraint_type:
                             (Option.map Position.value type_of_field)
                           ~generics_resolver env current_mod_name prog
                        )
                 in
                 ((s, loc_type), loc_type)
             )
          |> List.split
        in
        let generic_table = Ast.Type.default_generic_map struct_decl.generics in
        let () =
          List.iter2
            (fun kt (_, param_kt) ->
              (* let () = Printf.printf "init_ktype = %s, param type = %s\n" (Pprint.string_of_ktype kt.v) (Pprint.string_of_ktype param_kt.v) in *)
              Ast.Type.update_generics generic_table kt param_kt ()
            )
            initialisation_types struct_decl.fields
        in
        let () =
          struct_decl.fields |> List.combine init_types
          |> List.iter
               (fun
                 ( (init_field_name, init_type),
                   (struct_field_name, expected_typed)
                 )
               ->
                 let () =
                   if init_field_name.v <> struct_field_name.v then
                     raise
                       (struct_error
                          (Unexpected_field
                             {
                               expected = struct_field_name;
                               found = init_field_name;
                             }
                          )
                       )
                 in
                 if
                   not
                   @@ Asthelper.Struct.is_type_compatible_hashgen generic_table
                        init_type.v expected_typed.v struct_decl
                 then
                   raise @@ Ast.Error.ast_error
                   @@ Ast.Error.Uncompatible_type
                        { expected = expected_typed.v; found = init_type }
             )
        in
        let modules_path =
          modules_path
          |> Position.map (fun mp ->
                 if mp = "" then
                   current_mod_name
                 else
                   mp
             )
        in
        let kt =
          Asthelper.Struct.to_ktype_hash generic_table modules_path struct_decl
        in
        validate_location_type expression ~constraint_type kt
    (* validate_and_type_struct_initialisation ~env ~current_mod_name ~program:prog ~struct_module_path:modules_path ~fields: fields ~struct_decl *)
    | EEnum { modules_path; enum_name; variant; assoc_exprs } ->
        let enum_decl =
          match
            Asthelper.Program.find_enum_decl_opt current_mod_name modules_path
              (Option.map Position.value enum_name)
              variant assoc_exprs prog
          with
          | Error e ->
              raise @@ Ast.Error.ast_error e
          | Ok e ->
              e
        in

        let infered_map = Ast.Type.default_generic_map enum_decl.generics in
        let raw_associated_types =
          match Asthelper.Enum.associate_type variant enum_decl with
          | Some assoc ->
              assoc
          | None ->
              failwith "Wierd to fall here"
        in
        let () =
          if Util.are_diff_lenght assoc_exprs raw_associated_types then
            raise @@ enum_error
            @@ Ast.Error.Wrong_length_assoc_type
                 {
                   variant;
                   expected = List.length raw_associated_types;
                   found = List.length assoc_exprs;
                 }
        in
        let init_types =
          raw_associated_types |> List.combine assoc_exprs
          |> List.map (fun (expr, raw_kt) ->
                 let constraint_type =
                   if Asthelper.Enum.is_type_generic raw_kt.v enum_decl then
                     None
                   else
                     Some raw_kt.v
                 in
                 expr
                 |> Position.map_use
                    @@ typeof ~constraint_type ~generics_resolver env
                         current_mod_name prog
             )
        in
        let () =
          enum_decl.variants
          |> List.find_map (fun (var, assoc_types) ->
                 if var.v = variant.v then
                   Some assoc_types
                 else
                   None
             )
          (* |> Option.map (fun k -> print_endline (k |> List.map Asthelper.string_of_ktype |> String.concat ", "); k ) *)
          (* |> function Some s -> s | None -> (raise Not_found) *)
          |> Option.get
          |> fun assoc_types ->
          let () =
            if Util.are_diff_lenght init_types assoc_types then
              raise @@ Ast.Error.enum_error
              @@ Ast.Error.Wrong_length_assoc_type
                   {
                     variant;
                     expected = List.length assoc_types;
                     found = List.length assoc_exprs;
                   }
          in
          let () =
            List.iter2
              (fun kt param_kt ->
                (* let () = Printf.printf "init_ktype = %s, param type = %s\n" (Pprint.string_of_ktype kt.v) (Pprint.string_of_ktype param_kt.v) in *)
                Ast.Type.update_generics infered_map kt param_kt ()
              )
              init_types assoc_types
          in
          assoc_types |> List.combine init_types
          |> List.iter (fun (init, expected) ->
                 match
                   Asthelper.Enum.is_type_compatible_hashgen infered_map init.v
                     expected.v enum_decl
                 with
                 | false ->
                     raise @@ ast_error
                     @@ Uncompatible_type
                          { expected = expected.v; found = init }
                 | true ->
                     ()
             )
        in
        let modules_path =
          modules_path
          |> Position.map (fun mp ->
                 if mp = "" then
                   current_mod_name
                 else
                   mp
             )
        in
        let kt =
          Asthelper.Enum.to_ktype_hash infered_map modules_path enum_decl
        in
        validate_location_type expression ~constraint_type kt
    | ETuple expected_types ->
        let tuple_kt =
          TTuple
            (expected_types
            |> List.map @@ Position.map_use
               @@ typeof ~constraint_type:None ~generics_resolver env
                    current_mod_name prog
            )
        in
        validate_location_type expression ~constraint_type tuple_kt
    | EArray (expr :: xesprs as exprs) ->
        let _, hint_type =
          match constraint_type with
          | None ->
              (None, None)
          | Some (TArray { size; ktype }) ->
              (Some size, Some ktype)
          | Some _ ->
              (None, None)
        in
        let size =
          expression |> Position.map (fun _ -> Int64.of_int @@ List.length exprs)
        in
        let expr_type =
          expr
          |> Position.map_use
             @@ typeof
                  ~constraint_type:(Option.map Position.value hint_type)
                  ~generics_resolver env current_mod_name prog
        in
        let ktype =
          xesprs
          |> List.fold_left
               (fun type_acc elt ->
                 let t =
                   elt
                   |> Position.map_use
                      @@ typeof
                           ~constraint_type:(Option.map Position.value hint_type)
                           ~generics_resolver env current_mod_name prog
                 in
                 let restrict_rktype =
                   validate_location_type t ~constraint_type:(Some type_acc) t.v
                 in
                 restrict_rktype
               )
               expr_type.v
        in
        let ktype_loc = { v = ktype; position = expression.position } in
        let array_type = TArray { size; ktype = ktype_loc } in
        validate_location_type expression ~constraint_type array_type
    | EArray [] ->
        failwith "Unreachable: Array can not be empty"
    | EWhile (condition, body) ->
        let if_condition =
          typeof ~constraint_type:None ~generics_resolver env current_mod_name
            prog condition
        in
        let () =
          if Ast.Type.( !== ) if_condition TBool then
            raise @@ ast_error
            @@ Not_Boolean_Type_Condition
                 { found = condition |> Position.map (fun _ -> if_condition) }
        in
        let body_type =
          typeof_kbody ~generics_resolver
            (Env.push_empty_context env)
            current_mod_name prog body
        in
        let () =
          if not @@ Type.(TUnit === body_type) then
            raise @@ ast_error
            @@ Ast.Error.Not_unit_type_while
                 {
                   position = expression |> Position.map (fun _ -> ());
                   wrong_type = body_type;
                 }
        in
        validate_location_type expression ~constraint_type TUnit
    | EIf (if_expression, if_block, else_block) ->
        let if_condition =
          typeof ~constraint_type:None ~generics_resolver env current_mod_name
            prog if_expression
        in
        let () =
          if Ast.Type.( !== ) if_condition TBool then
            raise @@ ast_error
            @@ Not_Boolean_Type_Condition
                 {
                   found = if_expression |> Position.map (fun _ -> if_condition);
                 }
        in
        let if_type =
          typeof_kbody ~generics_resolver
            (Env.push_empty_context env)
            current_mod_name prog if_block
        in
        let else_type =
          typeof_kbody ~generics_resolver
            (Env.push_empty_context env)
            current_mod_name prog else_block
        in
        let () =
          if not @@ Type.are_compatible_type if_type else_type then
            raise @@ ast_error
            @@ Ast.Error.Uncompatible_type_If_Else
                 {
                   position = expression |> Position.map (fun _ -> ());
                   if_type;
                   else_type;
                 }
        in
        let restrited_type = Type.restrict_type else_type if_type in
        validate_location_type expression ~constraint_type restrited_type
    | ECases { cases; else_case } ->
        let else_case_type =
          typeof_kbody ~generics_resolver
            (Env.push_empty_context env)
            ~return_type:constraint_type current_mod_name prog else_case
        in
        cases
        |> List.map (fun (expr, kbody) ->
               let expr_type =
                 expr
                 |> Position.map_use
                      (typeof ~constraint_type:None ~generics_resolver env
                         current_mod_name prog
                      )
               in
               let () =
                 if Ast.Type.( !== ) expr_type.v TBool then
                   raise @@ ast_error
                   @@ Not_Boolean_Type_Condition { found = expr_type }
               in
               let _stmts, { v = _; position } = kbody in
               let type_kbody =
                 typeof_kbody ~generics_resolver
                   (Env.push_empty_context env)
                   current_mod_name prog kbody
               in
               (type_kbody, position)
           )
        |> List.fold_left
             (fun acc (new_type, position) ->
               let () =
                 if not @@ Type.are_compatible_type acc new_type then
                   raise @@ ast_error
                   @@ Uncompatible_type
                        { expected = acc; found = { v = new_type; position } }
               in
               Type.restrict_type acc new_type
             )
             else_case_type
    | EBuiltin_Function_call { fn_name; parameters } ->
        let ( >>= ) = Result.bind in
        let parameters_type =
          parameters
          |> List.map
               (Position.map_use
                  (typeof ~constraint_type:None ~generics_resolver env
                     current_mod_name prog
                  )
               )
        in

        let kt =
          fn_name |> Asthelper.Builtin_Function.builtin_fn_of_fn_name
          >>= (fun builtin ->
                Asthelper.Builtin_Function.is_valide_parameters_type fn_name
                  parameters_type current_mod_name prog builtin
              )
          |> Result.map Asthelper.Builtin_Function.builtin_return_type
          |> function Ok kt -> kt | Error e -> raise @@ built_in_func_error e
        in
        validate_location_type expression ~constraint_type kt
    | EFunction_call
        { modules_path; generics_resolver = grc; fn_name; parameters } -> (
        let fn_decl =
          match
            Asthelper.Program.find_function_decl_from_fn_name modules_path
              fn_name current_mod_name prog
          with
          | Error e ->
              raise @@ ast_error e
          | Ok fn_decl ->
              fn_decl
        in
        match fn_decl with
        | Ast.Function_Decl.Decl_Kosu_Function kosu_function_decl ->
            let () =
              if Util.are_diff_lenght parameters kosu_function_decl.parameters
              then
                raise @@ func_error
                @@ Unmatched_Parameters_length
                     {
                       fn_name;
                       expected = List.length kosu_function_decl.parameters;
                       found = List.length parameters;
                     }
            in
            let new_map_generics =
              kosu_function_decl.generics
              |> List.map (fun k -> (k, ()))
              |> List.to_seq |> Hashtbl.of_seq
            in

            let init_type_parameters =
              kosu_function_decl.parameters |> List.combine parameters
              |> List.map (fun (expr_loc, (_, fn_para_kt_loc)) ->
                     let constraint_type =
                       if
                         Asthelper.Function.is_ktype_generic fn_para_kt_loc.v
                           kosu_function_decl
                       then
                         None
                       else
                         Some fn_para_kt_loc.v
                     in
                     expr_loc
                     |> Position.map_use
                        @@ typeof ~constraint_type
                             ~generics_resolver:new_map_generics env
                             current_mod_name prog
                 )
            in
            let infered_map =
              Ast.Type.default_generic_map kosu_function_decl.generics
            in
            let () =
              List.iter2
                (fun kt (_, param_kt) ->
                  (* let () = Printf.printf "init_ktype = %s, param type = %s\n" (Pprint.string_of_ktype kt.v) (Pprint.string_of_ktype param_kt.v) in *)
                  Ast.Type.update_generics infered_map kt param_kt ()
                )
                init_type_parameters kosu_function_decl.parameters
            in

            (* let init_type_parameters = init_type_parameters |> List.map ( Position.map (Type.remap_generic_ktype ~current_module:current_mod_name infered_map)) in *)
            let hashtal =
              Hashtbl.create @@ List.length kosu_function_decl.generics
            in
            let () =
              match
                Asthelper.Function.does_need_generic_resolver kosu_function_decl
              with
              | true ->
                  let () =
                    if
                      Util.are_diff_lenght
                        (grc |> Option.value ~default:[])
                        kosu_function_decl.generics
                    then
                      raise @@ func_error
                      @@ Unmatched_Generics_Resolver_length
                           {
                             fn_name;
                             expected =
                               kosu_function_decl.generics |> List.length;
                             found =
                               grc |> Option.value ~default:[] |> List.length;
                           }
                  in
                  ()
              | false ->
                  ()
            in
            let () =
              match grc with
              | Some grc_safe ->
                  List.combine kosu_function_decl.generics grc_safe
                  |> List.iteri (fun index (generic_name, field_ktype) ->
                         let () =
                           Hashtbl.add infered_map generic_name.v
                             (index, field_ktype.v)
                         in
                         Hashtbl.add hashtal generic_name.v
                           (index, field_ktype.v)
                     )
              | None ->
                  ()
            in
            let () =
              init_type_parameters
              |> List.combine kosu_function_decl.parameters
              |> List.iter (fun ((_, para_type), init_type) ->
                     (* let () = Printf.printf "init_ktype = %s, expected = %s\n\n" (Pprint.string_of_ktype init_type.v) (Pprint.string_of_ktype para_type.v) in *)
                     if
                       not
                       @@ Asthelper.Function.is_type_compatible_hashgen hashtal
                            init_type.v para_type.v kosu_function_decl
                     then
                       raise @@ func_error
                       @@ Mismatched_Parameters_Type
                            {
                              fn_name = fn_name.v;
                              expected =
                                para_type
                                |> Position.map
                                     (Ast.Type.extract_mapped_ktype hashtal)
                                |> Position.value;
                              found = init_type;
                            }
                 )
            in

            (* let () = Printf.printf "infered_map = %s \n\n" (infered_map |> Hashtbl.to_seq |> List.of_seq |> List.map (fun (gene, (_, kt)) -> Printf.sprintf "%s = %s" (gene) (Pprint.string_of_ktype kt)) |> String.concat "\n" ) in  *)
            let kt =
              Asthelper.Function.to_return_ktype_hashtab
                ~current_module:current_mod_name
                ~module_type_path:modules_path.v infered_map kosu_function_decl
            in
            (* let () = Printf.printf "cm = %s, mp = %s fn return = %s\n" (current_mod_name) (modules_path.v) (Pprint.string_of_ktype kt) in *)
            validate_location_type expression ~constraint_type kt
        | Ast.Function_Decl.Decl_External external_func_decl ->
            let generics =
              generics_resolver |> Hashtbl.to_seq_keys |> List.of_seq
              |> List.map Position.value
            in
            let generics =
              if Rule.allow_generics_in_variadic then
                generics
              else
                []
            in
            let () =
              match external_func_decl.is_variadic with
              | true ->
                  let parameters_length = List.length parameters in
                  let fn_args_len =
                    List.length external_func_decl.fn_parameters
                  in
                  let () =
                    if parameters_length < fn_args_len then
                      raise @@ func_error
                      @@ Unmatched_Parameters_length
                           {
                             fn_name;
                             expected =
                               external_func_decl.fn_parameters |> List.length;
                             found = parameters |> List.length;
                           }
                  in
                  ()
              | false ->
                  let () =
                    if
                      Util.are_diff_lenght external_func_decl.fn_parameters
                        parameters
                    then
                      raise @@ func_error
                      @@ Unmatched_Parameters_length
                           {
                             fn_name;
                             expected =
                               external_func_decl.fn_parameters |> List.length;
                             found = parameters |> List.length;
                           }
                  in
                  ()
            in
            let external_constraint_types =
              List.init (List.length parameters) (fun index ->
                  index
                  |> List.nth_opt external_func_decl.fn_parameters
                  |> Option.map Position.value
              )
            in
            let init_exprs =
              external_constraint_types |> List.combine parameters
              |> List.map (fun (expr, constraint_type) ->
                     expr
                     |> Position.map_use
                        @@ typeof ~constraint_type ~generics_resolver env
                             current_mod_name prog
                 )
            in
            let () =
              init_exprs
              |> List.iter (fun kt ->
                     let () =
                       if
                         not
                         @@ Asthelper.Program.is_c_type_from_ktype ~generics
                              current_mod_name kt.v prog
                       then
                         raise @@ func_error
                         @@ Ast.Error.Uncompatible_type_for_C_Function
                              { fn_name; ktype = kt }
                     in
                     ()
                 )
            in
            (* Check if the return type is a C type should be done in Astvalidation. The typecher should just type*)
            validate_location_type expression ~constraint_type
              external_func_decl.r_type.v
        | Ast.Function_Decl.Decl_Syscall syscall_decl ->
            let () =
              if Util.are_diff_lenght syscall_decl.parameters parameters then
                raise @@ func_error
                @@ Unmatched_Parameters_length
                     {
                       fn_name;
                       expected = syscall_decl.parameters |> List.length;
                       found = parameters |> List.length;
                     }
            in
            let _mapped_type =
              syscall_decl.parameters |> List.combine parameters
              |> List.map (fun (expr, constraint_type) ->
                     expr
                     |> Position.map_use
                        @@ typeof ~constraint_type:(Some constraint_type.v)
                             ~generics_resolver env current_mod_name prog
                 )
            in
            validate_location_type expression ~constraint_type
              syscall_decl.return_type.v
      )
    | EBin_op (BMult (lhs, rhs)) ->
        typecheck_binary ~constraint_type
          ~fvalid:Asthelper.Program.is_valid_mult_operation ~generics_resolver
          ~expression ~env ~current_mod_name OperatorFunction.Mult lhs rhs prog
    | EBin_op (BDiv (lhs, rhs)) ->
        typecheck_binary ~constraint_type
          ~fvalid:Asthelper.Program.is_valid_div_operation ~generics_resolver
          ~expression ~env ~current_mod_name OperatorFunction.Div lhs rhs prog
    | EBin_op (BMod (lhs, rhs)) ->
        typecheck_binary ~constraint_type
          ~fvalid:Asthelper.Program.is_valid_mod_operation ~generics_resolver
          ~expression ~env ~current_mod_name OperatorFunction.Modulo lhs rhs
          prog
    | EBin_op (BBitwiseOr (lhs, rhs)) ->
        typecheck_binary ~constraint_type
          ~fvalid:Asthelper.Program.is_valid_bitwiseor_operation
          ~generics_resolver ~expression ~env ~current_mod_name
          OperatorFunction.BitwiseOr lhs rhs prog
    | EBin_op (BBitwiseAnd (lhs, rhs)) ->
        typecheck_binary ~constraint_type
          ~fvalid:Asthelper.Program.is_valid_bitwiseand_operation
          ~generics_resolver ~expression ~env ~current_mod_name
          OperatorFunction.BitwiseAnd lhs rhs prog
    | EBin_op (BBitwiseXor (lhs, rhs)) ->
        typecheck_binary ~constraint_type
          ~fvalid:Asthelper.Program.is_valid_bitwisexor_operation
          ~generics_resolver ~expression ~env ~current_mod_name
          OperatorFunction.BitwiseXor lhs rhs prog
    | EBin_op (BShiftLeft (lhs, rhs)) ->
        typecheck_binary ~constraint_type
          ~fvalid:Asthelper.Program.is_valid_shiftleft_operation
          ~generics_resolver ~expression ~env ~current_mod_name
          OperatorFunction.ShiftLeft lhs rhs prog
    | EBin_op (BShiftRight (lhs, rhs)) ->
        typecheck_binary ~constraint_type
          ~fvalid:Asthelper.Program.is_valid_shiftright_operation
          ~generics_resolver ~expression ~env ~current_mod_name
          OperatorFunction.ShiftRight lhs rhs prog
    | EBin_op (BAdd (lhs, rhs)) ->
        typecheck_binary ~constraint_type
          ~fvalid:Asthelper.Program.is_valid_add_operation ~generics_resolver
          ~expression ~env ~current_mod_name OperatorFunction.Add lhs rhs prog
    | EBin_op (BMinus (lhs, rhs)) ->
        typecheck_binary ~constraint_type
          ~fvalid:Asthelper.Program.is_valid_minus_operation ~generics_resolver
          ~expression ~env ~current_mod_name OperatorFunction.Minus lhs rhs prog
    | EBin_op (BAnd (lhs, rhs)) -> (
        let l_type =
          lhs
          |> Position.map_use
               (typeof ~constraint_type:None ~generics_resolver env
                  current_mod_name prog
               )
        in
        let r_type =
          rhs
          |> Position.map_use
               (typeof ~constraint_type:None ~generics_resolver env
                  current_mod_name prog
               )
        in
        match (l_type.v, r_type.v) with
        | TBool, TBool ->
            TBool
        | TBool, _ ->
            Not_Boolean_operand_in_And r_type |> operator_error |> raise
        | _, TBool ->
            Not_Boolean_operand_in_And l_type |> operator_error |> raise
        | _, _ ->
            Not_Boolean_operand_in_And l_type |> operator_error |> raise
      )
    | EBin_op (BOr (lhs, rhs)) -> (
        let l_type =
          lhs
          |> Position.map_use
               (typeof ~constraint_type:None ~generics_resolver env
                  current_mod_name prog
               )
        in
        let r_type =
          rhs
          |> Position.map_use
               (typeof ~constraint_type:None ~generics_resolver env
                  current_mod_name prog
               )
        in
        match (l_type.v, r_type.v) with
        | TBool, TBool ->
            validate_location_type expression ~constraint_type TBool
        | TBool, _ ->
            Not_Boolean_operand_in_Or r_type |> operator_error |> raise
        | _, TBool ->
            Not_Boolean_operand_in_Or l_type |> operator_error |> raise
        | _, _ ->
            Not_Boolean_operand_in_Or l_type |> operator_error |> raise
      )
    | EBin_op (BEqual (lhs, rhs) | BDif (lhs, rhs)) ->
        typecheck_binary ~constraint_type ~ktype:TBool ~freturn:TOredered
          ~fvalid:Asthelper.Program.is_valid_equal_operation ~generics_resolver
          ~expression ~env ~current_mod_name OperatorFunction.Equal lhs rhs prog
    | EBin_op (BSup (lhs, rhs)) ->
        typecheck_binary ~constraint_type ~ktype:TBool ~freturn:TOredered
          ~fvalid:Asthelper.Program.is_valid_sup_operation ~generics_resolver
          ~expression ~env ~current_mod_name OperatorFunction.Sup lhs rhs prog
    | EBin_op (BSupEq (lhs, rhs)) ->
        typecheck_binary ~constraint_type ~ktype:TBool ~freturn:TOredered
          ~fvalid:Asthelper.Program.is_valid_supeq_operation ~generics_resolver
          ~expression ~env ~current_mod_name OperatorFunction.SupEq lhs rhs prog
    | EBin_op (BInf (lhs, rhs)) ->
        typecheck_binary ~constraint_type ~ktype:TBool ~freturn:TOredered
          ~fvalid:Asthelper.Program.is_valid_inf_operation ~generics_resolver
          ~expression ~env ~current_mod_name OperatorFunction.Inf lhs rhs prog
    | EBin_op (BInfEq (lhs, rhs)) ->
        typecheck_binary ~constraint_type ~ktype:TBool ~freturn:TOredered
          ~fvalid:Asthelper.Program.is_valid_infeq_operation ~generics_resolver
          ~expression ~env ~current_mod_name OperatorFunction.InfEq lhs rhs prog
    | EBin_op (BCmp (lhs, rhs)) ->
        typecheck_binary ~constraint_type ~ktype:TOredered ~freturn:TOredered
          ~fvalid:Asthelper.Program.is_valid_cmp_operation ~generics_resolver
          ~expression ~env ~current_mod_name OperatorFunction.CompareOp lhs rhs
          prog
    | EUn_op (UNot lhs) -> (
        let l_type =
          lhs
          |> Position.map_use
               (typeof ~constraint_type ~generics_resolver env current_mod_name
                  prog
               )
        in
        match Asthelper.Program.is_valid_not_operation l_type.v prog with
        | `no_function_found ->
            Operator_not_found
              { bin_op = Ast.OperatorFunction.Not; ktype = l_type }
            |> operator_error |> raise
        | `valid _ ->
            l_type.v
        | `to_many_declaration operator_decls ->
            Too_many_operator_declaration
              {
                operator_decls;
                bin_op = Ast.OperatorFunction.Not;
                ktype = l_type;
              }
            |> operator_error |> raise
        | `built_in_valid ->
            l_type.v
        | `no_not_for_built_in ->
            No_built_in_op { bin_op = Ast.OperatorFunction.Not; ktype = l_type }
            |> operator_error |> raise
      )
    | EUn_op (UMinus lhs) ->
        let l_type =
          lhs
          |> Position.map_use
               (typeof ~constraint_type:None ~generics_resolver env
                  current_mod_name prog
               )
        in
        let kt =
          match Asthelper.Program.is_valid_uminus_operation l_type.v prog with
          | `no_function_found ->
              Operator_not_found
                { bin_op = Ast.OperatorFunction.UMinus; ktype = l_type }
              |> operator_error |> raise
          | `valid _ ->
              l_type.v
          | `to_many_declaration operator_decls ->
              Too_many_operator_declaration
                {
                  operator_decls;
                  bin_op = Ast.OperatorFunction.UMinus;
                  ktype = l_type;
                }
              |> operator_error |> raise
          | `built_in_valid ->
              l_type.v
          | `invalid_unsigned_op size ->
              Invalid_Uminus_for_Unsigned_integer
                { v = size; position = l_type.position }
              |> operator_error |> raise
          | `no_uminus_for_built_in ->
              No_built_in_op
                { bin_op = Ast.OperatorFunction.UMinus; ktype = l_type }
              |> operator_error |> raise
        in
        validate_location_type expression ~constraint_type kt
    | EMatch {
      expression;
      patterns;
    } -> 
      let scrutinee_type = 
        expression |> Position.map_use @@ typeof ~constraint_type:None ~generics_resolver env current_mod_name prog in
        let p, pxs = match patterns with
          | [] -> failwith "Unreachable: empty pattern"
          | t::q -> t, q
        in
        let type_pattern (pattern, body) = 
          let bound, ptype = typeof_pattern ~scrutinee_type:(scrutinee_type.v) env current_mod_name prog pattern in
          let ptype = {
            v = ptype;
            position = pattern.position
          } in
          let _ = validate_type ~constraint_type:(Some scrutinee_type.v) ptype in
          (* later bind variable *)
     
          let bounds = bound |> List.map (fun (id, kt) -> 
              id.v, Env.{
                is_const = false;
                ktype = kt
              }
            ) 
          in
          let env = Env.push_context bounds env in
          typeof_kbody 
          ~generics_resolver 
          (Env.push_empty_context env)
          ~return_type:constraint_type
          current_mod_name
          prog
          body
        in
        let match_base_type = type_pattern p in
        let kbody_type = pxs |> List.fold_left (fun acc_type pattern_body -> 
          let _, { v = _; position } = snd pattern_body in
          let body_type = {
            v = type_pattern pattern_body;
            position
          }  in
          validate_type ~constraint_type:(Some acc_type) body_type
        ) match_base_type in
        kbody_type
    | ESwitch { expression = expr; cases; wildcard_case } -> (
        let variant_cases =
          cases |> List.map (fun (v, _) -> v) |> List.flatten
        in
        let expr_type =
          typeof ~constraint_type:None ~generics_resolver env current_mod_name
            prog expr
        in
        let () =
          if expr_type |> Ast.Type.is_type_full_known |> not then
            raise @@ switch_error
            @@ Not_fully_known_ktype (expr |> Position.map (fun _ -> expr_type))
        in
        let module_path, name =
          expr_type |> Ast.Type.module_path_of_ktype_opt
          |> function
          | None ->
              raise @@ switch_error
              @@ Not_enum_type_in_switch_Expression
                   (expr |> Position.map (fun _ -> expr_type))
          | Some s ->
              s
        in
        let enum_decl =
          match
            Asthelper.Program.find_type_decl_from_ktype
              ~ktype_def_path:module_path ~ktype_name:name
              ~current_module:current_mod_name prog
          with
          | Ok (Type_Decl.Decl_Enum e) ->
              e
          | Ok (Type_Decl.Decl_Struct _s) ->
              raise @@ switch_error
              @@ Not_enum_type_in_switch_Expression
                   (expr |> Position.map (fun _ -> expr_type))
          | Error e ->
              raise @@ ast_error e
        in
        let params_type = Type.extract_parametrics_ktype expr_type in
        let mapped_generics = List.combine enum_decl.generics params_type in
        let () =
          enum_decl.variants
          |> List.iter (fun (variant_name, _) ->
                 match
                   Asthelper.Switch_case.cases_duplicated variant_name.v
                     variant_cases
                 with
                 | None ->
                     ()
                 | Some duplicate ->
                     Ast.Error.Duplicated_case duplicate |> switch_error
                     |> raise
             )
        in
        let bound_assoc_var_type_map =
          mapped_generics
          |> List.map Position.assocs_value
          |> List.to_seq |> Hashtbl.of_seq
        in

        let generics_mapped =
          expr_type 
          |> Ast.Type.extract_parametrics_ktype
          |> List.combine  enum_decl.generics
        in
        let open Asthelper.Enum in
        let open Asthelper.Switch_case in
        let () =
          if wildcard_case |> Option.is_none then
            match is_all_cases_handled ~expression variant_cases enum_decl with
            | Error e ->
                e |> switch_error |> raise
            | Ok _ ->
                ()
        in

        cases
        |> List.map (fun (sc_list, kb) ->
               let combine_binding_type =
                 sc_list
                 |> List.map (fun sc ->
                        let variant_name = variant_name sc in
                        let assoc_types =
                          Option.get @@ extract_assoc_type_variant generics_mapped
                            variant_name enum_decl
                        in
                        let assoc_binding = assoc_binding sc in
                        ( variant_name,
                          assoc_types |> List.combine assoc_binding
                          |> List.mapi (fun index (v, l) -> (index, v, l))
                        )
                    )
               in
               (* let () =  combine_binding_type |> List.iter (fun (var_name, list) ->
                    Printf.printf "\nvariant = %s(%s)\n"
                    (var_name.v)
                    (list |> List.map (fun (_, binding, kt) -> Printf.sprintf "%s => %s" (binding |> Option.map Position.value |> Option.value ~default:"_" ) (string_of_ktype kt.v)) |> String.concat ", ")
                    )
                  in *)
               match combine_binding_type with
               | [] ->
                   failwith "Unreachable case: empty case"
               | (first_variant, ass_bin) :: q ->
                   let new_context =
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
                                    base_bound_id = fst lhs;
                                    wrong_variant = variant_name;
                                    wrong_bound_id = fst rhs;
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
                     |> List.map (fun (_, variable_name, ktype) ->
                            let ktype =
                              ktype
                              |> Position.map
                                   (Type.remap_naif_generic_ktype
                                      bound_assoc_var_type_map
                                   )
                            in
                            (* let () = Printf.printf "bound %s : %s\n\n" (variable_name.v) (Pprint.string_of_ktype ktype.v) in *)
                            ( variable_name,
                              ( { is_const = true; ktype = ktype.v }
                                : Env.variable_info
                                )
                            )
                        )
                     |> List.map (fun (binding_name, var_info) ->
                            if env |> Env.is_identifier_exists binding_name.v
                            then
                              Identifier_already_Bound binding_name
                              |> switch_error |> raise
                            else
                              (binding_name, var_info)
                        )
                   in
                   let _stmt, { v = _; position } = kb in
                   ( typeof_kbody ~generics_resolver
                       (env
                       |> Env.push_context
                            (new_context |> List.map Position.assoc_value_left)
                       )
                       current_mod_name prog kb,
                     position
                   )
           )
        |> (fun l ->
             match wildcard_case with
             | None ->
                 l
             | Some wild ->
                 let _stmt, { v = _; position } = wild in
                 let wildcard_type =
                   typeof_kbody ~generics_resolver env current_mod_name prog
                     wild
                 in
                 (wildcard_type, position) :: l
           )
        |> function
        | [] ->
            failwith "unreachable case: empty kbody"
        | (t, _) :: q ->
            q
            |> List.fold_left
                 (fun acc (case_type, position) ->
                   if not (Type.are_compatible_type acc case_type) then
                     let () =
                       Printf.printf "Not compatible acc = %s; value = %s\n"
                         (string_of_ktype acc)
                         (string_of_ktype case_type)
                     in
                     Uncompatible_type
                       { expected = acc; found = { v = case_type; position } }
                     |> ast_error |> raise
                   else
                     Type.restrict_type acc case_type
                 )
                 t
      )

  and typecheck_binary ~constraint_type ~fvalid ?freturn ?ktype
      ~generics_resolver ~expression ~env ~current_mod_name op lhs rhs program =
    let l_type =
      lhs
      |> Position.map_use
           (typeof ~constraint_type:None ~generics_resolver env current_mod_name
              program
           )
    in
    let r_type =
      rhs
      |> Position.map_use
           (typeof ~constraint_type:None ~generics_resolver env current_mod_name
              program
           )
    in
    let l_type, r_type =
      match Type.are_compatible_type l_type.v r_type.v with
      | false ->
          (l_type, r_type)
      | true ->
          let l_type =
            l_type |> Position.map (fun l -> Type.restrict_type l r_type.v)
          in
          let r_type =
            r_type |> Position.map (fun r -> Type.restrict_type r l_type.v)
          in
          (l_type, r_type)
    in
    let rtype = Option.value ~default:l_type.v ktype in
    let freturn = Option.value ~default:rtype freturn in
    match fvalid ~freturn ~rtype l_type.v r_type.v program with
    | Ok (None | Some _) ->
        validate_location_type expression ~constraint_type rtype
    | Error VInvalid_Pointer_A ->
        Invalid_pointer_arithmetic r_type |> operator_error |> raise
    | Error Diff_type ->
        Incompatible_Type
          { expr_loc = expression; bin_op = op; lhs = l_type; rhs = r_type }
        |> operator_error |> raise
    | Error No_declaration_found ->
        Operator_not_found { bin_op = op; ktype = l_type }
        |> operator_error |> raise
    | Error (Too_many_decl { decls = operator_decls }) ->
        Too_many_operator_declaration
          { operator_decls; bin_op = op; ktype = l_type }
        |> operator_error |> raise
    | Error Builin_Invalid ->
        No_built_in_op { bin_op = op; ktype = l_type }
        |> operator_error |> raise
  and typeof_pattern ~scrutinee_type env current_module program pattern = match pattern.v with
  | PTrue | PFalse -> [], TBool
  | PEmpty -> [], TUnit
  | PCmpEqual | PCmpGreater | PCmpLess -> [], TOredered
  | PChar _ -> [], TChar
  | PFloat _ -> [], TFloat None
  | PInteger _ -> [], TInteger None
  | PWildcard -> [], scrutinee_type
  | PIdentifier id -> 
    let () = match Env.find_identifier_opt id.v env with
      | None -> ()
      | Some _ -> failwith "Cannot bind exisiting scope variable"      
  in
  (id, scrutinee_type)::[], scrutinee_type
  | PNullptr -> [], Type.kt_ptr_unknown
  | PTuple patterns ->
    let tuple_scrutinee = match scrutinee_type with 
      | TTuple kts -> kts
      | _ -> failwith "unmatched between scrutinee type and pattern type"
    in
    let () = match Util.are_same_lenght patterns tuple_scrutinee with
      | true -> ()
      | false -> failwith "unmatched length between scrutinee type tuple and pattern type tuple"
    in
    let bounds, ptypes = 
      tuple_scrutinee
      |> List.combine patterns
      |> List.map (fun (pattern, ktype) -> 
        let bound, kt = typeof_pattern ~scrutinee_type:ktype.v env current_module program pattern in
        let kt = { v = kt; position = pattern.position} in
        bound, kt
      )
      |> List.split
    in
    let bounds = List.flatten bounds in
    let duplicated = Util.ListHelper.duplicated (fun (lhs, _) (rhs, _) -> lhs.v = rhs.v) bounds
    in
    let () = match duplicated with
      | [] -> ()
      | _::_ -> failwith "identifier is already bound"
    in
    bounds, TTuple ptypes
  | PCase { variant; assoc_patterns } -> 
    let enum_decl = match Asthelper.Program.find_type_decl_from_true_ktype scrutinee_type current_module program with
      | None -> failwith "enum case as builint type"
      | Some (Decl_Struct _) -> failwith "match struct instead of enum"
      | Some (Decl_Enum e) -> e
    in
    let generic_map = 
      scrutinee_type
      |> Type.extract_parametrics_ktype
      |> List.combine enum_decl.generics
    in
    let assoc_types_opt = Asthelper.Enum.extract_assoc_type_variant generic_map variant enum_decl in
    let assoc_type = match assoc_types_opt with
      | None -> failwith @@ "type has not variant " ^ variant.v
      | Some assoc -> assoc 
    in
    let () = match Util.are_same_lenght assoc_patterns assoc_type with
      | true -> ()
      | false -> failwith "assoc case arithy issue"
    in
    let bounds, _ = 
      assoc_type
      |> List.combine assoc_patterns
      |> List.map (fun (pattern, ktype) -> 
        let bound, kt = typeof_pattern ~scrutinee_type:ktype.v env current_module program pattern in
        let kt = { v = kt; position = pattern.position} in
        bound, kt
      )
      |> List.split
    in
    let bounds = List.flatten bounds in
    let duplicated = Util.ListHelper.duplicated (fun (lhs, _) (rhs, _) -> lhs.v = rhs.v) bounds
    in
    let () = match duplicated with
      | [] -> ()
      | _::_ -> failwith "identifier is already bound"
    in
    bounds, scrutinee_type
  | POr patterns -> 
    let _, _ = match patterns with 
      | [] -> failwith "Unreachable: pattern POr cannot be empty"
      | t::q -> t,q
    in
    let bounds, _ = patterns 
    |> List.map ( fun pattern -> 
      let bound, kt =  typeof_pattern ~scrutinee_type env current_module program pattern in
      let kt = {
        v = kt;
        position = pattern.position
      } in
      bound, kt
    ) 
    |> List.split
  in

    let string_compare lhs rhs = String.compare lhs.v rhs.v in
  
    let identifier_bound = bounds |> List.map (fun bound -> 
      bound |> List.map (fun b -> fst b) |> List.sort string_compare
    )
  in

    let first_identifiers, others_identifier = match identifier_bound with
      | [] -> failwith "Unreachable"
      | t::q -> t, q 
    in

    let _ = others_identifier |> List.fold_left (fun acc bounds -> 
      match Util.ListHelper.ldiff string_compare acc bounds with
      | [] -> acc
      | t::_ -> failwith @@ Printf.sprintf "The identifier %s is not always bound" t.v
    ) first_identifiers 
    in

    let first_boud = List.hd bounds in

    first_boud, scrutinee_type
end
