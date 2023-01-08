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

(**
  Return the type of the code block expression by checking each expression in this one
  @raise Ast_error
  @raise No_Occurence : if a type declartion wasn't found or a variant is not in enum variants
  @raise Too_Many_Occurence: if several type declarations matching was found
*)
let rec typeof_kbody ~generics_resolver (env : Env.t)
    (current_mod_name : string) (program : module_path list)
    ?(return_type = None) (kbody : kbody) =
  (* let () = Printf.printf "env %s\n" (Pprint.string_of_env env) in *)
  let statements, final_expr = kbody in
  match statements with
  | stamement :: q -> (
      match stamement.v with
      | SDiscard expr ->
          ignore (typeof ~generics_resolver env current_mod_name program expr);
          typeof_kbody ~generics_resolver env current_mod_name program
            ~return_type (q, final_expr)
      | SDeclaration { is_const; variable_name; explicit_type; expression } ->
          let type_init =
            expression
            |> Position.map_use
                 (typeof ~generics_resolver env current_mod_name program)
          in
          (* let () = Printf.printf "sizeof %s : %Lu\nalignement : %Lu\n" (Pprint.string_of_ktype type_init.v) (Asthelper.Sizeof.sizeof current_mod_name program type_init.v) (Asthelper.Sizeof.alignmentof current_mod_name program type_init.v) in *)
          if env |> Env.is_identifier_exists variable_name.v then
            raise
              (stmt_error
                 (Ast.Error.Already_Define_Identifier { name = variable_name }))
          else
            let kt =
              match explicit_type with
              | None ->
                  if Ast.Type.is_type_full_known type_init.v |> not then
                    Need_explicit_type_declaration
                      { variable_name; infer_type = type_init.v }
                    |> stmt_error |> raise
                  else type_init
              | Some explicit_type_sure ->
                  if
                    not
                      (Type.are_compatible_type explicit_type_sure.v type_init.v)
                  then
                    raise
                      (Ast.Error.Uncompatible_type_Assign
                         { expected = explicit_type_sure.v; found = type_init }
                      |> stmt_error |> raise)
                  else explicit_type_sure
            in
            typeof_kbody ~generics_resolver
              (env
              |> Env.add_variable (variable_name.v, { is_const; ktype = kt.v })
              )
              current_mod_name program ~return_type (q, final_expr)
      | SAffection (variable, expr) -> (
          match env |> Env.find_identifier_opt variable.v with
          | None ->
              raise
                (stmt_error (Ast.Error.Undefine_Identifier { name = variable }))
          | Some { is_const; ktype } ->
              if is_const then
                raise
                  (stmt_error
                     (Ast.Error.Reassign_Constante { name = variable }))
              else
                let new_type =
                  typeof ~generics_resolver env current_mod_name program expr
                in
                if not (Ast.Type.are_compatible_type new_type ktype) then
                  raise
                    (stmt_error
                       (Ast.Error.Uncompatible_type_Assign
                          {
                            expected = ktype;
                            found = expr |> Position.map (fun _ -> new_type);
                          }))
                else
                  typeof_kbody ~generics_resolver
                    (env |> Env.restrict_variable_type variable.v new_type)
                    current_mod_name program ~return_type (q, final_expr))
      | SDerefAffectation (id, expression) -> (
          match env |> Env.find_identifier_opt id.v with
          | None ->
              Ast.Error.Undefine_Identifier { name = id } |> stmt_error |> raise
          | Some { ktype; _ } ->
              let in_pointer_ktype =
                match ktype with
                | TPointer ktl -> ktl.v
                | _ ->
                    Ast.Error.Dereference_No_pointer { name = id; ktype }
                    |> stmt_error |> raise
              in
              let expr_ktype =
                expression
                |> Position.map_use
                     (typeof ~generics_resolver env current_mod_name program)
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
                typeof_kbody ~generics_resolver
                  (env |> Env.restrict_variable_type id.v expr_ktype.v)
                  current_mod_name program ~return_type (q, final_expr)))
  | [] -> (
      (* Printf.printf "Final expr\n"; *)
      let final_expr_type =
        typeof ~generics_resolver env current_mod_name program final_expr
      in
      match return_type with
      | None -> final_expr_type
      | Some kt ->
          if not (Type.are_compatible_type kt final_expr_type) then
            raise
              (ast_error
                 (Ast.Error.Uncompatible_type
                    {
                      expected = kt;
                      found =
                        { v = final_expr_type; position = final_expr.position };
                    }))
          else kt)

(**
  Return the type of an expression
  @raise Ast_error
  @raise No_Occurence : if a type declartion wasn't found or a variant is not in enum variants
  @raise Too_Many_Occurence: if several type declarations matching was found
*)
and typeof ~generics_resolver (env : Env.t) (current_mod_name : string)
    (prog : module_path list) (expression : kexpression location) =
  match expression.v with
  | Empty -> TUnit
  | True | False -> TBool
  | ENullptr -> TPointer { v = TUnknow; position = expression.position }
  | EInteger (sign, size, _) -> TInteger (sign, size)
  | EFloat _ -> TFloat
  | ESizeof either ->
      let () =
        match either with
        | Left ktype ->
            ignore
              (match ktype.v with
              | TParametric_identifier
                  { module_path; parametrics_type = _; name }
              | TType_Identifier { module_path; name } -> (
                  match
                    Asthelper.Program.find_type_decl_from_ktype
                      ~ktype_def_path:module_path ~ktype_name:name
                      ~current_module:current_mod_name prog
                  with
                  | Ok _ -> ignore ()
                  | Error (Undefine_Type _ as e) when module_path.v = "" -> (
                      match
                        generics_resolver |> Hashtbl.to_seq_keys
                        |> Seq.find (fun gen_loc -> gen_loc.v = name.v)
                      with
                      | Some _ -> ignore ()
                      | None -> e |> Ast.Error.ast_error |> raise)
                  | Error e -> e |> Ast.Error.ast_error |> raise)
              | _ -> ignore ())
        | Right expr ->
            ignore (typeof ~generics_resolver env current_mod_name prog expr)
      in
      TInteger (Unsigned, I64)
  | EString _ -> TString_lit
  | EAdress s -> (
      env |> Env.flat_context |> List.assoc_opt s.v
      |> Option.map (fun (t : Env.variable_info) ->
             TPointer { v = t.ktype; position = s.position })
      |> function
      | None -> raise (ast_error (Undefined_Identifier s))
      | Some s -> s)
  | EDeference (indirection_count, id) -> (
      let rec loop count ktype =
        match count with
        | 0 -> ktype
        | s -> (
            match ktype with
            | Ast.TPointer t -> loop (s - 1) t.v
            | _ -> raise (ast_error (Unvalid_Deference id)))
      in
      match env |> Env.flat_context |> List.assoc_opt id.v with
      | None -> raise (ast_error (Undefined_Identifier id))
      | Some t -> loop indirection_count t.ktype)
  | EIdentifier { modules_path = _; identifier } -> (
      env |> Env.flat_context
      |> List.assoc_opt identifier.v
      |> Option.map (fun (var_info : Env.variable_info) -> var_info.ktype)
      |> function
      | None -> raise (ast_error (Undefined_Identifier identifier))
      | Some s -> s)
  | EConst_Identifier { modules_path; identifier } -> (
      let consts_opt =
        (if modules_path.v = "" then
         Some (prog |> Asthelper.Program.module_of_string current_mod_name)
        else prog |> Asthelper.Program.module_of_string_opt current_mod_name)
        |> Option.map Asthelper.Module.retrieve_const_decl
      in

      match consts_opt with
      | None -> raise (ast_error (Unbound_Module modules_path))
      | Some consts -> (
          consts
          |> List.find_map (fun c ->
                 if c.const_name.v = identifier.v then Some c.explicit_type
                 else None)
          |> function
          | None -> raise (ast_error (Undefined_Const identifier))
          | Some s -> s))
  | EFieldAcces { first_expr; field } ->
      let first_type =
        typeof ~generics_resolver env current_mod_name prog first_expr
      in
      let parametrics_types = Type.extract_parametrics_ktype first_type in
      let ktype_def_path = Type.module_path_opt first_type |> Option.get in
      let ktype_name = Type.type_name_opt first_type |> Option.get in
      let type_decl =
        match
          Asthelper.Program.find_type_decl_from_ktype ~ktype_def_path
            ~ktype_name ~current_module:current_mod_name prog
        with
        | Error e -> e |> Ast.Error.ast_error |> raise
        | Ok type_decl -> type_decl
      in
      Asthelper.Struct.resolve_fields_access_gen
        (parametrics_types |> List.map Position.value)
        field type_decl current_mod_name
  | EStruct { modules_path; struct_name; fields } ->
      let struct_decl =
        match
          Asthelper.Program.find_struct_decl_opt current_mod_name modules_path
            struct_name prog
        with
        | Ok str -> str
        | Error e -> e |> ast_error |> raise
      in

      let parameters_length = fields |> List.length in
      let expected_length = struct_decl.fields |> List.length in
      if parameters_length <> expected_length then
        raise
          (Ast.Error.struct_error
             (Wrong_field_count
                {
                  struct_name;
                  expected = expected_length;
                  found = parameters_length;
                }));

      let generic_table =
        Hashtbl.create (struct_decl.generics |> List.length)
      in
      let init_types =
        fields
        |> List.map (fun (s, expr) ->
               ( s,
                 expr
                 |> Position.map_use (fun expr_loc ->
                        typeof ~generics_resolver env current_mod_name prog
                          expr_loc) ))
      in
      List.combine init_types struct_decl.fields
      |> List.iter
           (fun
             ((init_field_name, init_type), (struct_field_name, expected_typed))
           ->
             if init_field_name.v <> struct_field_name.v then
               raise
                 (struct_error
                    (Unexpected_field
                       { expected = struct_field_name; found = init_field_name }));
             if
               Asthelper.Struct.is_type_compatible_hashgen generic_table
                 init_type.v expected_typed.v struct_decl
               |> not
             then
               Ast.Error.Uncompatible_type
                 { expected = expected_typed.v; found = init_type }
               |> Ast.Error.ast_error |> raise);
      let modules_path =
        modules_path
        |> Position.map (fun mp -> if mp = "" then current_mod_name else mp)
      in
      Asthelper.Struct.to_ktype_hash generic_table modules_path struct_decl
  (* validate_and_type_struct_initialisation ~env ~current_mod_name ~program:prog ~struct_module_path:modules_path ~fields: fields ~struct_decl *)
  | EEnum { modules_path; enum_name; variant; assoc_exprs } ->
      let enum_decl =
        match
          Asthelper.Program.find_enum_decl_opt current_mod_name modules_path
            (enum_name |> Option.map Position.value)
            variant assoc_exprs prog
        with
        | Error e -> raise (Ast.Error.ast_error e)
        | Ok e -> e
      in

      let hashtbl = Hashtbl.create (enum_decl.generics |> List.length) in
      enum_decl.generics
      |> List.iteri (fun i generic_name ->
             Hashtbl.add hashtbl generic_name.v (i, TUnknow));
      let init_types =
        assoc_exprs
        |> List.map
             (Position.map_use
                (typeof ~generics_resolver env current_mod_name prog))
      in
      let infered_map =
        enum_decl.generics
        |> List.mapi (fun index s -> (s.v, (index, TUnknow)))
        |> List.to_seq |> Hashtbl.of_seq
      in
      let () =
        enum_decl.variants
        |> List.find_map (fun (var, assoc_types) ->
               if var.v = variant.v then Some assoc_types else None)
        (* |> Option.map (fun k -> print_endline (k |> List.map Asthelper.string_of_ktype |> String.concat ", "); k ) *)
        (* |> function Some s -> s | None -> (raise Not_found) *)
        |> Option.get
        |> fun assoc_types ->
        if Util.are_diff_lenght init_types assoc_types then
          raise
            (Ast.Error.enum_error
               (Ast.Error.Wrong_length_assoc_type
                  {
                    variant;
                    expected = assoc_types |> List.length;
                    found = assoc_exprs |> List.length;
                  }))
        else
         let () = List.iter2
          (fun kt (param_kt) ->
            (* let () = Printf.printf "init_ktype = %s, param type = %s\n" (Pprint.string_of_ktype kt.v) (Pprint.string_of_ktype param_kt.v) in *)
            Ast.Type.update_generics infered_map kt param_kt ())
          init_types assoc_types
        in
          assoc_types |> List.combine init_types
          |> List.iter (fun (init, expected) ->
                 match
                   Asthelper.Enum.is_type_compatible_hashgen infered_map init.v
                     expected.v enum_decl
                 with
                 | false ->
                     Uncompatible_type { expected = expected.v; found = init }
                     |> ast_error |> raise
                 | true -> ())
      in
      let modules_path =
        modules_path
        |> Position.map (fun mp -> if mp = "" then current_mod_name else mp)
      in
      let kt = Asthelper.Enum.to_ktype_hash infered_map modules_path enum_decl in
      kt
  | ETuple expected_types ->
      TTuple
        (expected_types
        |> List.map (fun expr ->
               {
                 v = typeof ~generics_resolver env current_mod_name prog expr;
                 position = expr.position;
               }))
  | EIf (if_expression, if_block, else_block) ->
      let if_condition =
        typeof ~generics_resolver env current_mod_name prog if_expression
      in
      if Ast.Type.( !== ) if_condition TBool then
        raise
          (ast_error
             (Not_Boolean_Type_Condition
                {
                  found = if_expression |> Position.map (fun _ -> if_condition);
                }))
      else
        let if_type =
          typeof_kbody ~generics_resolver
            (env |> Env.push_context [])
            current_mod_name prog if_block
        in
        let else_type =
          typeof_kbody ~generics_resolver
            (env |> Env.push_context [])
            current_mod_name prog else_block
        in
        if not (Type.are_compatible_type if_type else_type) then
          raise
            (ast_error
               (Ast.Error.Uncompatible_type_If_Else
                  {
                    position = expression |> Position.map (fun _ -> ());
                    if_type;
                    else_type;
                  }))
        else Type.restrict_type else_type if_type
  | ECases { cases; else_case } ->
      cases
      |> List.map (fun (expr, kbody) ->
             let expr_type =
               expr
               |> Position.map_use
                    (typeof ~generics_resolver env current_mod_name prog)
             in
             if Ast.Type.( !== ) expr_type.v TBool then
               raise
                 (ast_error (Not_Boolean_Type_Condition { found = expr_type }))
             else
               let _stmts, { v = _; position } = kbody in
               ( typeof_kbody ~generics_resolver
                   (env |> Env.push_context [])
                   current_mod_name prog kbody,
                 position ))
      |> List.fold_left
           (fun acc (new_type, position) ->
             if not (Type.are_compatible_type acc new_type) then
               raise
                 (ast_error
                    (Uncompatible_type
                       { expected = acc; found = { v = new_type; position } }))
             else Type.restrict_type acc new_type)
           (typeof_kbody ~generics_resolver
              (env |> Env.push_context [])
              current_mod_name prog else_case)
  | EBuiltin_Function_call { fn_name; parameters } -> (
      let ( >>= ) = Result.bind in
      let parameters_type =
        parameters
        |> List.map
             (Position.map_use
                (typeof ~generics_resolver env current_mod_name prog))
      in

      fn_name |> Asthelper.Builtin_Function.builtin_fn_of_fn_name
      >>= (fun builtin ->
            Asthelper.Builtin_Function.is_valide_parameters_type fn_name
              parameters_type builtin)
      |> Result.map Asthelper.Builtin_Function.builtin_return_type
      |> function
      | Ok kt -> kt
      | Error e -> e |> built_in_func_error |> raise)
  | EFunction_call
      { modules_path; generics_resolver = grc; fn_name; parameters } -> (
      let fn_decl =
        match
          Asthelper.Program.find_function_decl_from_fn_name modules_path fn_name
            current_mod_name prog
        with
        | Error e -> e |> ast_error |> raise
        | Ok fn_decl -> fn_decl
      in
      match fn_decl with
      | Ast.Function_Decl.Decl_Kosu_Function e ->
          if Util.are_diff_lenght parameters e.parameters then
            Unmatched_Parameters_length
              {
                fn_name;
                expected = e.parameters |> List.length;
                found = parameters |> List.length;
              }
            |> func_error |> raise
          else
            let new_map_generics =
              Hashtbl.of_seq
                (e.generics |> List.map (fun k -> (k, ())) |> List.to_seq)
            in
            let init_type_parameters =
              parameters
              |> List.map
                   (Position.map_use
                      (typeof ~generics_resolver:new_map_generics env
                         current_mod_name prog))
            in
            let infered_map =
              e.generics
              |> List.mapi (fun index s -> (s.v, (index, TUnknow)))
              |> List.to_seq |> Hashtbl.of_seq
            in
            let () =
              List.iter2
                (fun kt (_, param_kt) ->
                  (* let () = Printf.printf "init_ktype = %s, param type = %s\n" (Pprint.string_of_ktype kt.v) (Pprint.string_of_ktype param_kt.v) in *)
                  Ast.Type.update_generics infered_map kt param_kt ())
                init_type_parameters e.parameters
            in

            (* let init_type_parameters = init_type_parameters |> List.map ( Position.map (Type.remap_generic_ktype ~current_module:current_mod_name infered_map)) in *)
            let hashtal = Hashtbl.create (e.generics |> List.length) in
            let () =
              match Asthelper.Function.does_need_generic_resolver e with
              | true ->
                  if
                    Util.are_diff_lenght
                      (grc |> Option.value ~default:[])
                      e.generics
                  then
                    Unmatched_Generics_Resolver_length
                      {
                        fn_name;
                        expected = e.generics |> List.length;
                        found = grc |> Option.value ~default:[] |> List.length;
                      }
                    |> func_error |> raise
                  else ()
              | false -> ()
            in
            let () =
              match grc with
              | Some grc_safe ->
                  List.combine e.generics grc_safe
                  |> List.iteri (fun index (generic_name, field_ktype) ->
                         Hashtbl.add hashtal generic_name.v
                           (index, field_ktype.v))
              | None -> ()
            in
            let () =
              init_type_parameters |> List.combine e.parameters
              |> List.iter (fun ((_, para_type), init_type) ->
                     (* let () = Printf.printf "init_ktype = %s, expected = %s\n\n" (Pprint.string_of_ktype init_type.v) (Pprint.string_of_ktype para_type.v) in *)
                     if
                       e
                       |> Asthelper.Function.is_type_compatible_hashgen hashtal
                            init_type.v para_type.v
                       |> not
                     then
                       Mismatched_Parameters_Type
                         {
                           fn_name = fn_name.v;
                           expected =
                             para_type
                             |> Position.map
                                  (Ast.Type.extract_mapped_ktype hashtal)
                             |> Position.value;
                           found = init_type;
                         }
                       |> func_error |> raise)
            in
            (* let () = Printf.printf "infered_map = %s \n\n" (infered_map |> Hashtbl.to_seq |> List.of_seq |> List.map (fun (gene, (_, kt)) -> Printf.sprintf "%s = %s" (gene) (Pprint.string_of_ktype kt)) |> String.concat "\n" ) in  *)

            let kt = Asthelper.Function.to_return_ktype_hashtab
              ~current_module:current_mod_name ~module_type_path:modules_path.v
              infered_map e in
              (* let () = Printf.printf "cm = %s, mp = %s fn return = %s\n" (current_mod_name) (modules_path.v) (Pprint.string_of_ktype kt) in *)
            kt
      | Ast.Function_Decl.Decl_External external_func_decl -> (
          if external_func_decl.is_variadic then
            parameters
            |> List.map
                 (Position.map_use
                    (typeof ~generics_resolver env current_mod_name prog))
            |> List.map (fun t ->
                   if
                     Asthelper.Program.is_c_type_from_ktype current_mod_name t.v
                       prog
                   then t
                   else
                     Ast.Error.Uncompatible_type_for_C_Function
                       { fn_name; ktype = t }
                     |> func_error |> raise)
            |> fun types ->
            if
              types |> List.length
              < (external_func_decl.fn_parameters |> List.length)
            then
              Unmatched_Parameters_length
                {
                  fn_name;
                  expected = external_func_decl.fn_parameters |> List.length;
                  found = parameters |> List.length;
                }
              |> func_error |> raise
            else
              types
              |> List.mapi (fun i t -> (i, t))
              |> List.partition (fun (i, _) ->
                     i < (external_func_decl.fn_parameters |> List.length))
              |> fun (lhs, _) ->
              lhs
              |> List.map (fun (_, t) -> t)
              |> List.combine external_func_decl.fn_parameters
              |> List.for_all (fun (para_type, init_type) ->
                     match
                       ( Asthelper.Program.is_c_type_from_ktype current_mod_name
                           para_type.v prog,
                         Asthelper.Program.is_c_type_from_ktype current_mod_name
                           init_type.v prog )
                     with
                     | true, true ->
                         if
                           not
                             (Ast.Type.are_compatible_type para_type.v
                                init_type.v)
                         then
                           Uncompatible_type_Assign
                             { expected = para_type.v; found = init_type }
                           |> stmt_error |> raise
                         else true
                     | _ ->
                         Ast.Error.Uncompatible_type_for_C_Function
                           { fn_name; ktype = para_type }
                         |> func_error |> raise)
              |> fun b ->
              if b then external_func_decl.r_type.v
              else Unknow_Function_Error |> func_error |> raise
          else
            match
              Util.are_same_lenght external_func_decl.fn_parameters parameters
            with
            | false ->
                Unmatched_Parameters_length
                  {
                    fn_name;
                    expected = external_func_decl.fn_parameters |> List.length;
                    found = parameters |> List.length;
                  }
                |> func_error |> raise
            | true ->
                let mapped_type =
                  parameters
                  |> List.map (fun located_expr ->
                         {
                           v =
                             typeof ~generics_resolver env current_mod_name prog
                               located_expr;
                           position = located_expr.position;
                         })
                in
                let zipped =
                  List.combine external_func_decl.fn_parameters mapped_type
                in
                if
                  zipped
                  |> List.for_all (fun (para_type, init_type) ->
                         match
                           ( Asthelper.Program.is_c_type_from_ktype
                               current_mod_name para_type.v prog,
                             Asthelper.Program.is_c_type_from_ktype
                               current_mod_name init_type.v prog )
                         with
                         | true, true ->
                             if
                               not
                                 (Ast.Type.are_compatible_type para_type.v
                                    init_type.v)
                             then
                               Uncompatible_type_Assign
                                 { expected = para_type.v; found = init_type }
                               |> stmt_error |> raise
                             else true
                         | _ ->
                             Ast.Error.Uncompatible_type_for_C_Function
                               { fn_name; ktype = para_type }
                             |> func_error |> raise)
                then
                  if
                    Asthelper.Program.is_c_type_from_ktype current_mod_name
                      external_func_decl.r_type.v prog
                  then external_func_decl.r_type.v
                  else
                    Ast.Error.Uncompatible_type_for_C_Function
                      { fn_name; ktype = external_func_decl.r_type }
                    |> func_error |> raise
                else Unknow_Function_Error |> func_error |> raise)
      | Ast.Function_Decl.Decl_Syscall syscall_decl -> (
          match Util.are_same_lenght syscall_decl.parameters parameters with
          | false ->
              Unmatched_Parameters_length
                {
                  fn_name;
                  expected = syscall_decl.parameters |> List.length;
                  found = parameters |> List.length;
                }
              |> func_error |> raise
          | true ->
              let mapped_type =
                parameters
                |> List.map (fun located_expr ->
                       {
                         v =
                           typeof ~generics_resolver env current_mod_name prog
                             located_expr;
                         position = located_expr.position;
                       })
              in
              let zipped =
                List.combine syscall_decl.parameters mapped_type
                |> List.mapi (fun i a -> (i, a))
              in
              if
                zipped
                |> List.for_all (fun (i, (para_type, init_type)) ->
                       if
                         not
                           (Ast.Type.are_compatible_type para_type.v init_type.v)
                       then
                         Uncompatible_type_Assign
                           { expected = para_type.v; found = init_type }
                         |> stmt_error |> raise
                       else
                         match
                           ( Asthelper.Program.is_c_type_from_ktype
                               current_mod_name para_type.v prog,
                             Asthelper.Program.is_c_type_from_ktype
                               current_mod_name init_type.v prog )
                         with
                         | true, true -> true
                         | _, _ ->
                             Ast.Error.Uncompatible_type_for_Syscall
                               { index = Some i; syscall_decl }
                             |> func_error |> raise)
              then
                if
                  Asthelper.Program.is_c_type_from_ktype current_mod_name
                    syscall_decl.return_type.v prog
                then syscall_decl.return_type.v
                else
                  Ast.Error.Uncompatible_type_for_Syscall
                    { index = None; syscall_decl }
                  |> func_error |> raise
              else Unknow_Function_Error |> func_error |> raise))
  | EBin_op (BAdd (lhs, rhs)) -> (
      let l_type =
        lhs
        |> Position.map_use
             (typeof ~generics_resolver env current_mod_name prog)
      in
      let r_type =
        rhs
        |> Position.map_use
             (typeof ~generics_resolver env current_mod_name prog)
      in
      match Asthelper.Program.is_valid_add_operation l_type.v r_type.v prog with
      | `built_in_ptr_valid -> l_type.v
      | `invalid_add_pointer ->
          Invalid_pointer_arithmetic r_type |> operator_error |> raise
      | `diff_types ->
          Incompatible_Type
            {
              expr_loc = expression;
              bin_op = Ast.OperatorFunction.Add;
              lhs = l_type;
              rhs = r_type;
            }
          |> operator_error |> raise
      | `no_function_found ->
          Operator_not_found
            { bin_op = Ast.OperatorFunction.Add; ktype = l_type }
          |> operator_error |> raise
      | `valid _ -> l_type.v
      | `to_many_declaration operator_decls ->
          Too_many_operator_declaration
            {
              operator_decls;
              bin_op = Ast.OperatorFunction.Add;
              ktype = l_type;
            }
          |> operator_error |> raise
      | `built_in_valid -> l_type.v
      | `no_add_for_built_in ->
          No_built_in_op { bin_op = Ast.OperatorFunction.Add; ktype = l_type }
          |> operator_error |> raise)
  | EBin_op (BMinus (lhs, rhs)) -> (
      let l_type =
        lhs
        |> Position.map_use
             (typeof ~generics_resolver env current_mod_name prog)
      in
      let r_type =
        rhs
        |> Position.map_use
             (typeof ~generics_resolver env current_mod_name prog)
      in
      match
        Asthelper.Program.is_valid_minus_operation l_type.v r_type.v prog
      with
      | `built_in_ptr_valid -> l_type.v
      | `invalid_add_pointer ->
          Invalid_pointer_arithmetic r_type |> operator_error |> raise
      | `diff_types ->
          Incompatible_Type
            {
              expr_loc = expression;
              bin_op = Ast.OperatorFunction.Minus;
              lhs = l_type;
              rhs = r_type;
            }
          |> operator_error |> raise
      | `no_function_found ->
          Operator_not_found
            { bin_op = Ast.OperatorFunction.Minus; ktype = l_type }
          |> operator_error |> raise
      | `valid _ -> l_type.v
      | `to_many_declaration operator_decls ->
          Too_many_operator_declaration
            {
              operator_decls;
              bin_op = Ast.OperatorFunction.Minus;
              ktype = l_type;
            }
          |> operator_error |> raise
      | `built_in_valid -> l_type.v
      | `no_minus_for_built_in ->
          No_built_in_op { bin_op = Ast.OperatorFunction.Minus; ktype = l_type }
          |> operator_error |> raise)
  | EBin_op (BMult (lhs, rhs)) -> (
      let l_type =
        lhs
        |> Position.map_use
             (typeof ~generics_resolver env current_mod_name prog)
      in
      let r_type =
        rhs
        |> Position.map_use
             (typeof ~generics_resolver env current_mod_name prog)
      in
      match
        Asthelper.Program.is_valid_mult_operation l_type.v r_type.v prog
      with
      | `diff_types ->
          Incompatible_Type
            {
              expr_loc = expression;
              bin_op = Ast.OperatorFunction.Mult;
              lhs = l_type;
              rhs = r_type;
            }
          |> operator_error |> raise
      | `no_function_found ->
          Operator_not_found
            { bin_op = Ast.OperatorFunction.Mult; ktype = l_type }
          |> operator_error |> raise
      | `valid _ -> l_type.v
      | `to_many_declaration operator_decls ->
          Too_many_operator_declaration
            {
              operator_decls;
              bin_op = Ast.OperatorFunction.Mult;
              ktype = l_type;
            }
          |> operator_error |> raise
      | `built_in_valid -> l_type.v
      | `no_mult_for_built_in ->
          No_built_in_op { bin_op = Ast.OperatorFunction.Mult; ktype = l_type }
          |> operator_error |> raise)
  | EBin_op (BDiv (lhs, rhs)) -> (
      let l_type =
        lhs
        |> Position.map_use
             (typeof ~generics_resolver env current_mod_name prog)
      in
      let r_type =
        rhs
        |> Position.map_use
             (typeof ~generics_resolver env current_mod_name prog)
      in
      match Asthelper.Program.is_valid_div_operation l_type.v r_type.v prog with
      | `diff_types ->
          Incompatible_Type
            {
              expr_loc = expression;
              bin_op = Ast.OperatorFunction.Div;
              lhs = l_type;
              rhs = r_type;
            }
          |> operator_error |> raise
      | `no_function_found ->
          Operator_not_found
            { bin_op = Ast.OperatorFunction.Div; ktype = l_type }
          |> operator_error |> raise
      | `valid _ -> l_type.v
      | `to_many_declaration operator_decls ->
          Too_many_operator_declaration
            {
              operator_decls;
              bin_op = Ast.OperatorFunction.Div;
              ktype = l_type;
            }
          |> operator_error |> raise
      | `built_in_valid -> l_type.v
      | `no_div_for_built_in ->
          No_built_in_op { bin_op = Ast.OperatorFunction.Div; ktype = l_type }
          |> operator_error |> raise)
  | EBin_op (BMod (lhs, rhs)) -> (
      let l_type =
        lhs
        |> Position.map_use
             (typeof ~generics_resolver env current_mod_name prog)
      in
      let r_type =
        rhs
        |> Position.map_use
             (typeof ~generics_resolver env current_mod_name prog)
      in
      match Asthelper.Program.is_valid_mod_operation l_type.v r_type.v prog with
      | `diff_types ->
          Incompatible_Type
            {
              expr_loc = expression;
              bin_op = Ast.OperatorFunction.Modulo;
              lhs = l_type;
              rhs = r_type;
            }
          |> operator_error |> raise
      | `no_function_found ->
          Operator_not_found
            { bin_op = Ast.OperatorFunction.Modulo; ktype = l_type }
          |> operator_error |> raise
      | `valid _ -> l_type.v
      | `to_many_declaration operator_decls ->
          Too_many_operator_declaration
            {
              operator_decls;
              bin_op = Ast.OperatorFunction.Modulo;
              ktype = l_type;
            }
          |> operator_error |> raise
      | `built_in_valid -> l_type.v
      | `no_mod_for_built_in ->
          No_built_in_op
            { bin_op = Ast.OperatorFunction.Modulo; ktype = l_type }
          |> operator_error |> raise)
  | EBin_op (BBitwiseOr (lhs, rhs)) -> (
      let l_type =
        lhs
        |> Position.map_use
             (typeof ~generics_resolver env current_mod_name prog)
      in
      let r_type =
        rhs
        |> Position.map_use
             (typeof ~generics_resolver env current_mod_name prog)
      in
      match
        Asthelper.Program.is_valid_bitwiseor_operation l_type.v r_type.v prog
      with
      | `diff_types ->
          Incompatible_Type
            {
              expr_loc = expression;
              bin_op = Ast.OperatorFunction.BitwiseOr;
              lhs = l_type;
              rhs = r_type;
            }
          |> operator_error |> raise
      | `no_function_found ->
          Operator_not_found
            { bin_op = Ast.OperatorFunction.BitwiseOr; ktype = l_type }
          |> operator_error |> raise
      | `valid _ -> l_type.v
      | `to_many_declaration operator_decls ->
          Too_many_operator_declaration
            {
              operator_decls;
              bin_op = Ast.OperatorFunction.BitwiseOr;
              ktype = l_type;
            }
          |> operator_error |> raise
      | `built_in_valid -> l_type.v
      | `no_bitwiseor_for_built_in ->
          No_built_in_op
            { bin_op = Ast.OperatorFunction.BitwiseOr; ktype = l_type }
          |> operator_error |> raise)
  | EBin_op (BBitwiseAnd (lhs, rhs)) -> (
      let l_type =
        lhs
        |> Position.map_use
             (typeof ~generics_resolver env current_mod_name prog)
      in
      let r_type =
        rhs
        |> Position.map_use
             (typeof ~generics_resolver env current_mod_name prog)
      in
      match
        Asthelper.Program.is_valid_bitwiseand_operation l_type.v r_type.v prog
      with
      | `diff_types ->
          Incompatible_Type
            {
              expr_loc = expression;
              bin_op = Ast.OperatorFunction.BitwiseAnd;
              lhs = l_type;
              rhs = r_type;
            }
          |> operator_error |> raise
      | `no_function_found ->
          Operator_not_found
            { bin_op = Ast.OperatorFunction.BitwiseAnd; ktype = l_type }
          |> operator_error |> raise
      | `valid _ -> l_type.v
      | `to_many_declaration operator_decls ->
          Too_many_operator_declaration
            {
              operator_decls;
              bin_op = Ast.OperatorFunction.BitwiseAnd;
              ktype = l_type;
            }
          |> operator_error |> raise
      | `built_in_valid -> l_type.v
      | `no_bitwiseand_for_built_in ->
          No_built_in_op
            { bin_op = Ast.OperatorFunction.BitwiseAnd; ktype = l_type }
          |> operator_error |> raise)
  | EBin_op (BBitwiseXor (lhs, rhs)) -> (
      let l_type =
        lhs
        |> Position.map_use
             (typeof ~generics_resolver env current_mod_name prog)
      in
      let r_type =
        rhs
        |> Position.map_use
             (typeof ~generics_resolver env current_mod_name prog)
      in
      match
        Asthelper.Program.is_valid_bitwisexor_operation l_type.v r_type.v prog
      with
      | `diff_types ->
          Incompatible_Type
            {
              expr_loc = expression;
              bin_op = Ast.OperatorFunction.BitwiseXor;
              lhs = l_type;
              rhs = r_type;
            }
          |> operator_error |> raise
      | `no_function_found ->
          Operator_not_found
            { bin_op = Ast.OperatorFunction.BitwiseXor; ktype = l_type }
          |> operator_error |> raise
      | `valid _ -> l_type.v
      | `to_many_declaration operator_decls ->
          Too_many_operator_declaration
            {
              operator_decls;
              bin_op = Ast.OperatorFunction.BitwiseXor;
              ktype = l_type;
            }
          |> operator_error |> raise
      | `built_in_valid -> l_type.v
      | `no_bitwisexor_for_built_in ->
          No_built_in_op
            { bin_op = Ast.OperatorFunction.BitwiseXor; ktype = l_type }
          |> operator_error |> raise)
  | EBin_op (BShiftLeft (lhs, rhs)) -> (
      let l_type =
        lhs
        |> Position.map_use
             (typeof ~generics_resolver env current_mod_name prog)
      in
      let r_type =
        rhs
        |> Position.map_use
             (typeof ~generics_resolver env current_mod_name prog)
      in
      match
        Asthelper.Program.is_valid_shiftleft_operation l_type.v r_type.v prog
      with
      | `diff_types ->
          Incompatible_Type
            {
              expr_loc = expression;
              bin_op = Ast.OperatorFunction.ShiftLeft;
              lhs = l_type;
              rhs = r_type;
            }
          |> operator_error |> raise
      | `no_function_found ->
          Operator_not_found
            { bin_op = Ast.OperatorFunction.ShiftLeft; ktype = l_type }
          |> operator_error |> raise
      | `valid _ -> l_type.v
      | `to_many_declaration operator_decls ->
          Too_many_operator_declaration
            {
              operator_decls;
              bin_op = Ast.OperatorFunction.ShiftLeft;
              ktype = l_type;
            }
          |> operator_error |> raise
      | `built_in_valid -> l_type.v
      | `no_shiftleft_for_built_in ->
          No_built_in_op
            { bin_op = Ast.OperatorFunction.ShiftLeft; ktype = l_type }
          |> operator_error |> raise)
  | EBin_op (BShiftRight (lhs, rhs)) -> (
      let l_type =
        lhs
        |> Position.map_use
             (typeof ~generics_resolver env current_mod_name prog)
      in
      let r_type =
        rhs
        |> Position.map_use
             (typeof ~generics_resolver env current_mod_name prog)
      in
      match
        Asthelper.Program.is_valid_shiftright_operation l_type.v r_type.v prog
      with
      | `diff_types ->
          Incompatible_Type
            {
              expr_loc = expression;
              bin_op = Ast.OperatorFunction.ShiftLeft;
              lhs = l_type;
              rhs = r_type;
            }
          |> operator_error |> raise
      | `no_function_found ->
          Operator_not_found
            { bin_op = Ast.OperatorFunction.ShiftLeft; ktype = l_type }
          |> operator_error |> raise
      | `valid _ -> l_type.v
      | `to_many_declaration operator_decls ->
          Too_many_operator_declaration
            {
              operator_decls;
              bin_op = Ast.OperatorFunction.ShiftLeft;
              ktype = l_type;
            }
          |> operator_error |> raise
      | `built_in_valid -> l_type.v
      | `no_shiftright_for_built_in ->
          No_built_in_op
            { bin_op = Ast.OperatorFunction.ShiftLeft; ktype = l_type }
          |> operator_error |> raise)
  | EBin_op (BAnd (lhs, rhs)) -> (
      let l_type =
        lhs
        |> Position.map_use
             (typeof ~generics_resolver env current_mod_name prog)
      in
      let r_type =
        rhs
        |> Position.map_use
             (typeof ~generics_resolver env current_mod_name prog)
      in
      match (l_type.v, r_type.v) with
      | TBool, TBool -> TBool
      | TBool, _ -> Not_Boolean_operand_in_And r_type |> operator_error |> raise
      | _, TBool -> Not_Boolean_operand_in_And l_type |> operator_error |> raise
      | _, _ -> Not_Boolean_operand_in_And l_type |> operator_error |> raise)
  | EBin_op (BOr (lhs, rhs)) -> (
      let l_type =
        lhs
        |> Position.map_use
             (typeof ~generics_resolver env current_mod_name prog)
      in
      let r_type =
        rhs
        |> Position.map_use
             (typeof ~generics_resolver env current_mod_name prog)
      in
      match (l_type.v, r_type.v) with
      | TBool, TBool -> TBool
      | TBool, _ -> Not_Boolean_operand_in_Or r_type |> operator_error |> raise
      | _, TBool -> Not_Boolean_operand_in_Or l_type |> operator_error |> raise
      | _, _ -> Not_Boolean_operand_in_Or l_type |> operator_error |> raise)
  | EBin_op (BEqual (lhs, rhs) | BDif (lhs, rhs)) -> (
      let l_type =
        lhs
        |> Position.map_use
             (typeof ~generics_resolver env current_mod_name prog)
      in
      let r_type =
        rhs
        |> Position.map_use
             (typeof ~generics_resolver env current_mod_name prog)
      in
      match
        Asthelper.Program.is_valid_equal_operation l_type.v r_type.v prog
      with
      | `diff_types ->
          Incompatible_Type
            {
              expr_loc = expression;
              bin_op = Ast.OperatorFunction.Equal;
              lhs = l_type;
              rhs = r_type;
            }
          |> operator_error |> raise
      | `no_function_found ->
          Operator_not_found
            { bin_op = Ast.OperatorFunction.Equal; ktype = l_type }
          |> operator_error |> raise
      | `valid _ -> TBool
      | `to_many_declaration operator_decls ->
          Too_many_operator_declaration
            {
              operator_decls;
              bin_op = Ast.OperatorFunction.Equal;
              ktype = l_type;
            }
          |> operator_error |> raise
      | `built_in_valid -> TBool
      | `no_equal_for_built_in ->
          No_built_in_op { bin_op = Ast.OperatorFunction.Equal; ktype = l_type }
          |> operator_error |> raise)
  | EBin_op (BSup (lhs, rhs)) -> (
      let l_type =
        lhs
        |> Position.map_use
             (typeof ~generics_resolver env current_mod_name prog)
      in
      let r_type =
        rhs
        |> Position.map_use
             (typeof ~generics_resolver env current_mod_name prog)
      in
      match Asthelper.Program.is_valid_sup_operation l_type.v r_type.v prog with
      | `diff_types ->
          Incompatible_Type
            {
              expr_loc = expression;
              bin_op = Ast.OperatorFunction.Sup;
              lhs = l_type;
              rhs = r_type;
            }
          |> operator_error |> raise
      | `no_function_found ->
          Operator_not_found
            { bin_op = Ast.OperatorFunction.Sup; ktype = l_type }
          |> operator_error |> raise
      | `valid _ -> TBool
      | `to_many_declaration operator_decls ->
          Too_many_operator_declaration
            {
              operator_decls;
              bin_op = Ast.OperatorFunction.Sup;
              ktype = l_type;
            }
          |> operator_error |> raise
      | `built_in_valid -> TBool
      | `no_sup_for_built_in ->
          No_built_in_op { bin_op = Ast.OperatorFunction.Sup; ktype = l_type }
          |> operator_error |> raise)
  | EBin_op (BSupEq (lhs, rhs)) -> (
      let l_type =
        lhs
        |> Position.map_use
             (typeof ~generics_resolver env current_mod_name prog)
      in
      let r_type =
        rhs
        |> Position.map_use
             (typeof ~generics_resolver env current_mod_name prog)
      in
      match
        Asthelper.Program.is_valid_supeq_operation l_type.v r_type.v prog
      with
      | `diff_types ->
          Incompatible_Type
            {
              expr_loc = expression;
              bin_op = Ast.OperatorFunction.SupEq;
              lhs = l_type;
              rhs = r_type;
            }
          |> operator_error |> raise
      | `no_function_found ->
          Operator_not_found
            { bin_op = Ast.OperatorFunction.SupEq; ktype = l_type }
          |> operator_error |> raise
      | `valid _ -> TBool
      | `to_many_declaration operator_decls ->
          Too_many_operator_declaration
            {
              operator_decls;
              bin_op = Ast.OperatorFunction.SupEq;
              ktype = l_type;
            }
          |> operator_error |> raise
      | `built_in_valid -> TBool
      | `no_equal_for_built_in ->
          No_built_in_op { bin_op = Ast.OperatorFunction.Equal; ktype = l_type }
          |> operator_error |> raise
      | `no_sup_for_built_in ->
          No_built_in_op { bin_op = Ast.OperatorFunction.Sup; ktype = l_type }
          |> operator_error |> raise)
  | EBin_op (BInf (lhs, rhs)) -> (
      let l_type =
        lhs
        |> Position.map_use
             (typeof ~generics_resolver env current_mod_name prog)
      in
      let r_type =
        rhs
        |> Position.map_use
             (typeof ~generics_resolver env current_mod_name prog)
      in
      match Asthelper.Program.is_valid_inf_operation l_type.v r_type.v prog with
      | `diff_types ->
          Incompatible_Type
            {
              expr_loc = expression;
              bin_op = Ast.OperatorFunction.Inf;
              lhs = l_type;
              rhs = r_type;
            }
          |> operator_error |> raise
      | `no_function_found ->
          Operator_not_found
            { bin_op = Ast.OperatorFunction.Inf; ktype = l_type }
          |> operator_error |> raise
      | `valid _ -> TBool
      | `to_many_declaration operator_decls ->
          Too_many_operator_declaration
            {
              operator_decls;
              bin_op = Ast.OperatorFunction.Inf;
              ktype = l_type;
            }
          |> operator_error |> raise
      | `built_in_valid -> TBool
      | `no_inf_for_built_in ->
          No_built_in_op { bin_op = Ast.OperatorFunction.Inf; ktype = l_type }
          |> operator_error |> raise)
  | EBin_op (BInfEq (lhs, rhs)) -> (
      let l_type =
        lhs
        |> Position.map_use
             (typeof ~generics_resolver env current_mod_name prog)
      in
      let r_type =
        rhs
        |> Position.map_use
             (typeof ~generics_resolver env current_mod_name prog)
      in
      match
        Asthelper.Program.is_valid_infeq_operation l_type.v r_type.v prog
      with
      | `diff_types ->
          Incompatible_Type
            {
              expr_loc = expression;
              bin_op = Ast.OperatorFunction.InfEq;
              lhs = l_type;
              rhs = r_type;
            }
          |> operator_error |> raise
      | `no_function_found ->
          Operator_not_found
            { bin_op = Ast.OperatorFunction.InfEq; ktype = l_type }
          |> operator_error |> raise
      | `valid _ -> TBool
      | `to_many_declaration operator_decls ->
          Too_many_operator_declaration
            {
              operator_decls;
              bin_op = Ast.OperatorFunction.InfEq;
              ktype = l_type;
            }
          |> operator_error |> raise
      | `built_in_valid -> TBool
      | `no_equal_for_built_in ->
          No_built_in_op { bin_op = Ast.OperatorFunction.Equal; ktype = l_type }
          |> operator_error |> raise
      | `no_inf_for_built_in ->
          No_built_in_op { bin_op = Ast.OperatorFunction.Inf; ktype = l_type }
          |> operator_error |> raise)
  | EUn_op (UNot lhs) -> (
      let l_type =
        lhs
        |> Position.map_use
             (typeof ~generics_resolver env current_mod_name prog)
      in
      match Asthelper.Program.is_valid_not_operation l_type.v prog with
      | `no_function_found ->
          Operator_not_found
            { bin_op = Ast.OperatorFunction.Not; ktype = l_type }
          |> operator_error |> raise
      | `valid _ -> l_type.v
      | `to_many_declaration operator_decls ->
          Too_many_operator_declaration
            {
              operator_decls;
              bin_op = Ast.OperatorFunction.Not;
              ktype = l_type;
            }
          |> operator_error |> raise
      | `built_in_valid -> l_type.v
      | `no_not_for_built_in ->
          No_built_in_op { bin_op = Ast.OperatorFunction.Not; ktype = l_type }
          |> operator_error |> raise)
  | EUn_op (UMinus lhs) -> (
      let l_type =
        lhs
        |> Position.map_use
             (typeof ~generics_resolver env current_mod_name prog)
      in
      match Asthelper.Program.is_valid_uminus_operation l_type.v prog with
      | `no_function_found ->
          Operator_not_found
            { bin_op = Ast.OperatorFunction.UMinus; ktype = l_type }
          |> operator_error |> raise
      | `valid _ -> l_type.v
      | `to_many_declaration operator_decls ->
          Too_many_operator_declaration
            {
              operator_decls;
              bin_op = Ast.OperatorFunction.UMinus;
              ktype = l_type;
            }
          |> operator_error |> raise
      | `built_in_valid -> l_type.v
      | `invalid_unsigned_op size ->
          Invalid_Uminus_for_Unsigned_integer
            { v = size; position = l_type.position }
          |> operator_error |> raise
      | `no_uminus_for_built_in ->
          No_built_in_op
            { bin_op = Ast.OperatorFunction.UMinus; ktype = l_type }
          |> operator_error |> raise)
  | ESwitch { expression = expr; cases; wildcard_case } -> (
      let variant_cases = cases |> List.map (fun (v, _) -> v) |> List.flatten in
      let expr_type =
        typeof ~generics_resolver env current_mod_name prog expr
      in
      if expr_type |> Ast.Type.is_type_full_known |> not then
        Not_fully_known_ktype (expr |> Position.map (fun _ -> expr_type))
        |> switch_error |> raise
      else
        let module_path, name =
          expr_type |> Asthelper.module_path_of_ktype_opt |> function
          | None ->
              Not_enum_type_in_switch_Expression
                (expr |> Position.map (fun _ -> expr_type))
              |> switch_error |> raise
          | Some s -> s
        in
        let enum_decl =
          match
            Asthelper.Program.find_type_decl_from_ktype
              ~ktype_def_path:module_path ~ktype_name:name
              ~current_module:current_mod_name prog
          with
          | Ok (Type_Decl.Decl_Enum e) -> e
          | Ok (Type_Decl.Decl_Struct _s) ->
              Not_enum_type_in_switch_Expression
                (expr |> Position.map (fun _ -> expr_type))
              |> switch_error |> raise
          | Error e -> e |> ast_error |> raise
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
                 | None -> ()
                 | Some duplicate ->
                     Ast.Error.Duplicated_case duplicate |> switch_error
                     |> raise)
        in
        let bound_assoc_var_type_map = mapped_generics 
        |> List.map Position.assocs_value  
        |> List.to_seq
        |> Hashtbl.of_seq
      in

        let generics_mapped =
          expr_type |> Ast.Type.extract_parametrics_ktype
          |> List.combine
               (enum_decl.generics
               |> List.map (fun name ->
                      TType_Identifier
                        {
                          module_path = { v = ""; position = Position.dummy };
                          name;
                        }))
        in
        let open Asthelper.Enum in
        let open Asthelper.Switch_case in
        let () =
          if wildcard_case |> Option.is_none then
            match is_all_cases_handled ~expression variant_cases enum_decl with
            | Error e -> e |> switch_error |> raise
            | Ok _ -> ()
        in

        ( cases
        |> List.map (fun (sc_list, kb) ->
               let combine_binding_type =
                 sc_list
                 |> List.map (fun sc ->
                        let variant_name = sc |> variant_name in
                        let assoc_types =
                          extract_assoc_type_variant generics_mapped
                            variant_name enum_decl
                          |> Option.get
                        in
                        let assoc_binding = assoc_binding sc in
                        ( variant_name,
                          assoc_types |> List.combine assoc_binding
                          |> List.mapi (fun index (v, l) -> (index, v, l)) ))
               in
               (* let () =  combine_binding_type |> List.iter (fun (var_name, list) ->
                    Printf.printf "\nvariant = %s(%s)\n"
                    (var_name.v)
                    (list |> List.map (fun (_, binding, kt) -> Printf.sprintf "%s => %s" (binding |> Option.map Position.value |> Option.value ~default:"_" ) (string_of_ktype kt.v)) |> String.concat ", ")
                    )
                  in *)
               match combine_binding_type with
               | [] -> failwith "Unreachable case: empty case"
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
                     |> List.map (fun (_, variable_name, ktype) ->
                      let ktype = ktype |> Position.map (Type.remap_naif_generic_ktype bound_assoc_var_type_map) in 
                      (* let () = Printf.printf "bound %s : %s\n\n" (variable_name.v) (Pprint.string_of_ktype ktype.v) in *)
                            ( variable_name,
                              ({ is_const = true; ktype = ktype.v }
                                : Env.variable_info) ))
                     |> List.map (fun (binding_name, var_info) ->
                            if env |> Env.is_identifier_exists binding_name.v
                            then
                              Identifier_already_Bound binding_name
                              |> switch_error |> raise
                            else (binding_name, var_info))
                   in
                   let _stmt, { v = _; position } = kb in
                   ( typeof_kbody ~generics_resolver
                       (env
                       |> Env.push_context
                            (new_context |> List.map Position.assoc_value_left)
                       )
                       current_mod_name prog kb,
                     position ))
        |> fun l ->
          match wildcard_case with
          | None -> l
          | Some wild ->
              let _stmt, { v = _; position } = wild in
              let wildcard_type =
                typeof_kbody ~generics_resolver env current_mod_name prog wild
              in
              (wildcard_type, position) :: l )
        |> function
        | [] -> failwith "unreachable case: empty kbody"
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
                   else Type.restrict_type acc case_type)
                 t)
