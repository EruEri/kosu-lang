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
open Printf
open Position
open Ast.Error

let f = sprintf "%Ld"

module Module = struct
  let retrieve_enum_decl = function
    | Ast.Mod nodes ->
        nodes
        |> List.filter_map (fun node ->
               match node with Ast.NEnum e -> Some e | _ -> None)

  let retrieve_struct_decl = function
    | Ast.Mod nodes ->
        nodes
        |> List.filter_map (fun node ->
               match node with Ast.NStruct s -> Some s | _ -> None)

  let retrieve_external_func_decl = function
    | Ast.Mod nodes ->
        nodes
        |> List.filter_map (fun node ->
               match node with Ast.NExternFunc s -> Some s | _ -> None)

  let retrieve_func_decl = function
    | Ast.Mod nodes ->
        nodes
        |> List.filter_map (fun node ->
               match node with Ast.NFunction s -> Some s | _ -> None)

  let retrieve_operator_decl = function
    | Ast.Mod nodes ->
        nodes
        |> List.filter_map (function Ast.NOperator s -> Some s | _ -> None)

  let retrieve_syscall_decl = function
    | Ast.Mod nodes ->
        nodes
        |> List.filter_map (fun node ->
               match node with Ast.NSyscall s -> Some s | _ -> None)

  let retrieve_const_decl = function
    | Ast.Mod nodes ->
        nodes
        |> List.filter_map (fun node ->
               match node with Ast.NConst s -> Some s | _ -> None)

  let retrieve_type_decl = function
    | Ast.Mod nodes ->
        nodes
        |> List.filter_map (fun node ->
               match node with
               | Ast.NEnum e -> Some (Ast.Type_Decl.decl_enum e)
               | Ast.NStruct s -> Some (Ast.Type_Decl.decl_struct s)
               | _ -> None)

  let retrieve_functions_decl = function
    | Ast.Mod nodes ->
        nodes
        |> List.filter_map (fun node ->
               match node with
               | Ast.NExternFunc e -> Some (Ast.Function_Decl.Decl_External e)
               | Ast.NFunction e ->
                   Some (Ast.Function_Decl.decl_kosu_function e)
               | Ast.NSyscall e -> Some (Ast.Function_Decl.decl_syscall e)
               | _ -> None)

  let retrieve_function_decl_from_name_and_types fn_name parameters_types
      r_types _module =
    _module |> retrieve_func_decl
    |> Util.Occurence.find_occurence (fun fn_decl ->
           fn_decl.fn_name = fn_name
           && Ast.Type.are_compatible_type r_types.v fn_decl.return_type.v
           && Util.are_same_lenght fn_decl.parameters parameters_types
           && List.for_all2
                (fun para init -> Ast.Type.are_compatible_type para.v init.v)
                (fn_decl.parameters |> List.map (fun (_, kt) -> kt))
                parameters_types)

  let type_decl_occurence (type_name : string) (module_definition : Ast._module)
      =
    module_definition |> retrieve_type_decl
    |> Util.Occurence.find_occurence (function
         | Ast.Type_Decl.Decl_Enum e -> e.enum_name.v = type_name
         | Ast.Type_Decl.Decl_Struct s -> s.struct_name.v = type_name)

  let function_decl_occurence (fn_name : string)
      (module_definition : Ast._module) =
    module_definition |> retrieve_functions_decl
    |> Util.Occurence.find_occurence (function
         | Ast.Function_Decl.Decl_External e -> fn_name = e.sig_name.v
         | Ast.Function_Decl.Decl_Kosu_Function e -> fn_name = e.fn_name.v
         | Ast.Function_Decl.Decl_Syscall e -> fn_name = e.syscall_name.v)
end

module Program = struct
  type t = Ast.program

  let to_module_path_list (program : t) =
    program |> List.map (fun named_module_path -> named_module_path.module_path)

  let map_module_path f (program : Ast.program) =
    program
    |> List.map (fun { filename; module_path } ->
           { filename; module_path = f module_path })

  let module_of_string_opt mod_name program =
    program
    |> List.filter_map (fun (mp : Ast.module_path) ->
           if mod_name = mp.path then Some mp._module else None)
    |> function
    | [] -> None
    | t :: _ -> Some t

  let module_of_string mod_name program =
    program
    |> List.filter_map (fun (mp : Ast.module_path) ->
           if mod_name = mp.path then Some mp._module else None)
    |> List.hd

  let find_struct_decl_opt (current_module_name : string)
      (module_path : string location) (struct_name : string location)
      (program : module_path list) =
    let ( >>= ) = Result.bind in

    (if module_path.v = "" then
       Some (program |> module_of_string current_module_name)
     else program |> module_of_string_opt module_path.v)
    |> Option.map Module.retrieve_struct_decl
    |> Option.to_result ~none:(Ast.Error.Unbound_Module module_path)
    >>= fun structs ->
    structs
    |> List.find_opt (fun s -> s.struct_name.v = struct_name.v)
    |> Option.to_result ~none:(Ast.Error.Undefined_Struct struct_name)

  let find_enum_decl_opt current_module_name module_enum_path
      (enum_name_opt : string option) (variant : string location)
      (assoc_exprs : kexpression location list) (program : module_path list) =
    match
      if module_enum_path.v = "" then
        Some (program |> module_of_string current_module_name)
      else program |> module_of_string_opt module_enum_path.v
    with
    | None -> Ast.Error.Unbound_Module module_enum_path |> Result.error
    | Some _module -> (
        _module |> Module.retrieve_enum_decl
        |> Util.Occurence.find_occurence (fun enum_decl ->
               match enum_name_opt with
               | None ->
                   enum_decl.variants
                   |> List.exists (fun (variant_decl, assoc_type) ->
                          variant_decl.v = variant.v
                          && Util.are_same_lenght assoc_exprs assoc_type)
               | Some enum_name ->
                   enum_name = enum_decl.enum_name.v
                   && enum_decl.variants
                      |> List.exists (fun (variant_decl, assoc_type) ->
                             variant_decl.v = variant.v
                             && Util.are_same_lenght assoc_exprs assoc_type))
        |> function
        | Empty ->
            Ast.Error.Enum_Error
              (Ast.Error.No_variant_found_for_enum { variant })
            |> Result.error
        | Util.Occurence.Multiple enum_decls ->
            Ast.Error.Enum_Error
              (Ast.Error.Conflict_variant_multiple_decl
                 { module_path = current_module_name; variant; enum_decls })
            |> Result.error
        | One enum_decl -> enum_decl |> Result.ok)

  let find_module_of_ktype ktype_def_path current_module program =
    match ktype_def_path with
    | "" ->
        program
        |> Util.Occurence.find_occurence (fun module_path ->
               module_path.path = current_module)
    | s ->
        program
        |> Util.Occurence.find_occurence (fun module_path ->
               module_path.path = s)

  let find_module_of_function fn_def_path current_module program =
    match fn_def_path with
    | "" ->
        program
        |> Util.Occurence.find_occurence (fun module_path ->
               module_path.path = current_module)
    | s ->
        program
        |> Util.Occurence.find_occurence (fun module_path ->
               module_path.path = s)

  (**
    Find type declaration from ktype
  *)
  let find_type_decl_from_ktype ~ktype_def_path ~ktype_name ~current_module
      program =
    let ( >>= ) = Result.bind in
    (find_module_of_ktype ktype_def_path.v current_module program |> function
     | Util.Occurence.Empty ->
         Ast.Error.Unbound_Module ktype_def_path |> Result.error
     | Util.Occurence.One module_path -> Ok module_path
     | Util.Occurence.Multiple module_paths ->
         Ast.Error.Conflicting_module_path_declaration
           { module_path = ktype_def_path; choices = module_paths }
         |> Result.error)
    >>= fun m ->
    m._module |> Module.type_decl_occurence ktype_name.v |> function
    | Util.Occurence.Empty -> Ast.Error.Undefine_Type ktype_name |> Result.error
    | Util.Occurence.One m -> m |> Result.ok
    | Util.Occurence.Multiple type_decls ->
        Ast.Error.Conflicting_type_declaration
          { path = m.path; ktype_name; type_decls }
        |> Result.error

  (**
      Find type declaration from ktype
      @return type_decl if ktype came from a type declaration, [None] if ktype is a builtin type
      @raise Ast_error: if length of assoc_type is not the same as the length of the generic of the type declaration found
    *)
  let rec find_type_decl_from_true_ktype ?(generics = []) ktype current_module
      program =
    let type_generics = function
      | Ast.Type_Decl.Decl_Enum e -> e.generics
      | Ast.Type_Decl.Decl_Struct s -> s.generics
    in
    let type_name = function
      | Ast.Type_Decl.Decl_Enum e -> e.enum_name
      | Ast.Type_Decl.Decl_Struct s -> s.struct_name
    in
    match ktype with
    | TType_Identifier { module_path = ktype_def_path; name = ktype_name } ->
        if
          ktype_def_path.v = ""
          && generics |> List.exists (fun gl -> gl.v = ktype_name.v)
        then None
        else
          let type_decl =
            match
              find_type_decl_from_ktype ~ktype_def_path ~ktype_name
                ~current_module program
            with
            | Error e -> e |> Ast.Error.ast_error |> raise
            | Ok type_decl -> type_decl
          in
          let generics_len = type_decl |> type_generics |> List.length in
          if generics_len <> 0 then
            Wrong_Assoc_Length_for_Parametrics
              {
                type_name = type_decl |> type_name;
                expected = generics_len;
                found = 0;
              }
            |> Ast.Error.ast_error |> raise
          else type_decl |> Option.some
    | TParametric_identifier
        { module_path = ktype_def_path; parametrics_type; name = ktype_name } ->
        let _ =
          parametrics_type
          |> List.iter (fun ktl ->
                 let _ =
                   find_type_decl_from_true_ktype ~generics ktl.v current_module
                     program
                 in
                 ())
        in
        let type_decl =
          match
            find_type_decl_from_ktype ~ktype_def_path ~ktype_name
              ~current_module program
          with
          | Error e -> e |> Ast.Error.ast_error |> raise
          | Ok type_decl -> type_decl
        in
        let generics_len = type_decl |> type_generics |> List.length in
        let assoc_type = parametrics_type |> List.length in
        if generics_len <> assoc_type then
          Wrong_Assoc_Length_for_Parametrics
            {
              type_name = type_decl |> type_name;
              expected = generics_len;
              found = assoc_type;
            }
          |> Ast.Error.ast_error |> raise
        else type_decl |> Option.some
    | TPointer kt ->
        find_type_decl_from_true_ktype ~generics kt.v current_module program
    | TTuple kts ->
        kts
        |> List.iter (fun kt ->
               let _ =
                 find_type_decl_from_true_ktype ~generics kt.v current_module
                   program
               in
               ());
        None
    | _ -> None

  (**
    Find function declaration from function name
    *)
  let find_function_decl_from_fn_name fn_def_path fn_name current_module program
      =
    let ( >>= ) = Result.bind in
    (program |> find_module_of_function fn_def_path.v current_module |> function
     | Util.Occurence.Empty ->
         Ast.Error.Unbound_Module fn_def_path |> Result.error
     | Util.Occurence.Multiple module_paths ->
         Ast.Error.Conflicting_module_path_declaration
           { module_path = fn_def_path; choices = module_paths }
         |> Result.error
     | Util.Occurence.One mp -> Ok mp)
    >>= fun m ->
    m._module |> Module.function_decl_occurence fn_name.v |> function
    | Util.Occurence.Empty ->
        Ast.Error.Undefine_function fn_name |> Result.error
    | Util.Occurence.Multiple fn_decls ->
        Ast.Error.Conflicting_function_declaration
          { fn_def_path = m.path; fn_name; fn_decls }
        |> Result.error
    | Util.Occurence.One fn_decl -> Ok fn_decl

  let rec is_c_type current_mod_name (type_decl : Ast.Type_Decl.type_decl)
      program =
    match type_decl with
    | Decl_Enum e ->
        e.generics = []
        && e.variants |> List.for_all (fun (_, assoc) -> assoc = [])
    | Decl_Struct s ->
        s.generics = []
        && s.fields
           |> List.for_all (fun (_, { v = ktype; _ }) ->
                  is_c_type_from_ktype current_mod_name ktype program)

  (**
      @return whether the [ktype] is a c compatible type or not
      @raise Ast.Error.Ast_error e if no type declaration is found for the [ktype]
  *)
  and is_c_type_from_ktype ?(generics = []) current_mod_name (ktype : ktype)
      program =
    match ktype with
    | TParametric_identifier _ -> false
    | TType_Identifier { module_path; name } -> (
        match
          find_type_decl_from_ktype ~ktype_def_path:module_path ~ktype_name:name
            ~current_module:current_mod_name program
        with
        | Error _ when module_path.v = "" && List.mem name.v generics -> true
        | Error e -> e |> Ast.Error.ast_error |> raise
        | Ok type_decl -> is_c_type current_mod_name type_decl program)
    | TFunction _ -> false
    | TTuple _ -> false
    | _ -> true

  let find_binary_operator (op : Ast.parser_binary_op) (lhs, rhs) r_type program
      =
    let open Ast.Type in
    (* let open Ast.Type in *)
    program
    |> List.map (fun t ->
           ( t.path,
             Module.retrieve_operator_decl t._module
             |> List.filter (fun op_decl ->
                    match op_decl with
                    | Unary _ -> false
                    | Binary record ->
                        let (_, kt1), (_, kt2) = record.fields in
                        record.return_type.v === r_type
                        && kt1.v === lhs && kt2.v === rhs && record.op.v = op)
           ))
    |> List.filter (fun (_path, operator_list) -> operator_list <> [])

  let find_unary_operator (op : Ast.parser_unary_op) lhs r_type program =
    let open Ast.Type in
    program
    |> List.map (fun t ->
           ( t.path,
             Module.retrieve_operator_decl t._module
             |> List.filter (fun op_decl ->
                    match op_decl with
                    | Binary _ -> false
                    | Unary record ->
                        let _, kt1 = record.field in
                        record.return_type.v === r_type
                        && kt1.v === lhs && record.op.v = op) ))
    |> List.filter (fun (_path, operator_list) -> operator_list <> [])

  let find_function_exact fn_name ktypes_parameters return_type
      (program : module_path list) =
    let open Util.Occurence in
    program
    |> List.map (fun t -> t._module)
    |> List.map
         (Module.retrieve_function_decl_from_name_and_types fn_name
            ktypes_parameters return_type)
    |> List.filter (function One _ -> true | _ -> false)

  let is_valid_add_minus_operator ~freturn ~rtype op lhs rhs program =
    ignore freturn;
    match lhs with
    | TPointer _ -> (
        match rhs with
        | TInteger _ -> Ok None
        | _ -> Result.error @@ VInvalid_Pointer_A)
    | _ -> (
        if Ast.Type.( !== ) lhs rhs then Result.error Diff_type
        else
          match lhs with
          | TType_Identifier _ as kt -> (
              let declaration =
                program |> find_binary_operator op (kt, kt) rtype
              in
              match declaration |> Util.ListHelper.inner_count with
              | 0 -> Result.error No_declaration_found
              | 1 ->
                  Result.ok @@ Option.some
                  @@ (declaration
                     |> List.find (fun (_, value) -> List.length value = 1))
              | _ -> Result.error @@ Too_many_decl { decls = declaration })
          | TInteger _ | TFloat _ -> Ok None
          | _ -> Result.error Error.Builin_Invalid)

  let is_valid_add_operation = is_valid_add_minus_operator Ast.Add
  let is_valid_minus_operation = is_valid_add_minus_operator Ast.Minus

  let is_valid_op ?no_decl op fvalid ~freturn ~rtype lhs rhs program =
    if Ast.Type.( !== ) lhs rhs then Result.error Diff_type
    else
      match lhs with
      | TType_Identifier _ as kt -> (
          let declaration = program |> find_binary_operator op (kt, kt) rtype in
          match declaration |> Util.ListHelper.inner_count with
          | 0 -> (
              match no_decl with
              | Some f -> f ~rtype:freturn lhs rhs program
              | None -> Result.error No_declaration_found)
          | 1 ->
              Result.ok @@ Option.some
              @@ (declaration
                 |> List.find (fun (_, value) -> List.length value = 1))
          | _ -> Result.error @@ Too_many_decl { decls = declaration })
      | kt when fvalid kt -> Ok None
      | _ -> Result.error Error.Builin_Invalid

  let is_valid_mult_operation ~freturn =
    is_valid_op ~freturn Ast.Mult (function
      | TInteger _ | TFloat _ -> true
      | _ -> false)

  let is_valid_div_operation ~freturn =
    is_valid_op ~freturn Ast.Div (function
      | TInteger _ | TFloat _ -> true
      | _ -> false)

  let is_valid_mod_operation ~freturn =
    is_valid_op ~freturn Ast.Modulo (function TInteger _ -> true | _ -> false)

  let is_valid_bitwiseor_operation ~freturn =
    is_valid_op ~freturn Ast.BitwiseOr (function
      | TInteger _ -> true
      | _ -> false)

  let is_valid_bitwiseand_operation ~freturn =
    is_valid_op ~freturn Ast.BitwiseAnd (function
      | TInteger _ -> true
      | _ -> false)

  let is_valid_bitwisexor_operation ~freturn =
    is_valid_op ~freturn Ast.BitwiseXor (function
      | TInteger _ -> true
      | _ -> false)

  let is_valid_shiftleft_operation ~freturn =
    is_valid_op ~freturn Ast.ShiftLeft (function
      | TInteger _ -> true
      | _ -> false)

  let is_valid_shiftright_operation ~(freturn : ktype) =
    is_valid_op ~freturn Ast.ShiftRight (function
      | TInteger _ -> true
      | _ -> false)

  let is_valid_spaceship_operator =
    is_valid_op Ast.Spaceship (function
      | TInteger _ | TFloat _ | TOredered -> true
      | _ -> false)

  let is_valid_equal_operation ~freturn =
    is_valid_op Ast.Equal ~freturn
      ~no_decl:(is_valid_spaceship_operator ~freturn) (function
      | TInteger _ | TBool | TFloat _ | TPointer _ | TOredered -> true
      | _ -> false)

  let is_valid_diff_operation ~freturn = is_valid_equal_operation ~freturn

  let is_valid_sup_operation ~freturn =
    is_valid_op Ast.Spaceship ~freturn
      ~no_decl:(is_valid_spaceship_operator ~freturn) (function
      | TInteger _ | TFloat _ | TOredered -> true
      | _ -> false)

  let is_valid_supeq_operation ~freturn = is_valid_sup_operation ~freturn
  let is_valid_inf_operation = is_valid_sup_operation
  let is_valid_infeq_operation = is_valid_sup_operation

  let is_valid_cmp_operation ~(freturn : ktype) =
    is_valid_op Ast.Spaceship ~freturn
      ~no_decl:(is_valid_spaceship_operator ~freturn) (function
      | TInteger _ | TFloat _ | TOredered -> true
      | _ -> false)

  let is_valid_not_operation ktype program =
    match ktype with
    | TType_Identifier _ as kt -> (
        let declaration = program |> find_unary_operator Ast.PNot kt kt in
        match declaration |> Util.ListHelper.inner_count with
        | 0 -> `no_function_found
        | 1 ->
            `valid
              (declaration
              |> List.find (fun (_, value) -> List.length value = 1))
        | _ -> `to_many_declaration declaration)
    | TInteger _ | TBool -> `built_in_valid
    | _ -> `no_not_for_built_in

  let is_valid_uminus_operation ktype program =
    match ktype with
    | TType_Identifier _ as kt -> (
        let declaration = program |> find_unary_operator Ast.PUMinus kt kt in
        match declaration |> Util.ListHelper.inner_count with
        | 0 -> `no_function_found
        | 1 ->
            `valid
              (declaration
              |> List.find (fun (_, value) -> List.length value = 1))
        | _ -> `to_many_declaration declaration)
    | TInteger (Signed, _) | TFloat _ -> `built_in_valid
    | TInteger (Unsigned, size) -> `invalid_unsigned_op size
    | _ -> `no_uminus_for_built_in
end

module Kbody = struct
  let abs_module s mp = mp |> Position.map (fun m -> if m = "" then s else m)

  let rec remap_body_explicit_type generics current_module kbody =
    let stmts_located, (expr_located : kexpression location) = kbody in
    ( stmts_located
      |> List.map (fun located_stmt ->
             located_stmt
             |> Position.map (fun stmt ->
                    match stmt with
                    | SDerefAffectation (s, le) ->
                        SDerefAffectation
                          ( s,
                            remap_located_expr_explicit_type generics
                              current_module le )
                    | SDiscard le ->
                        SDiscard
                          (remap_located_expr_explicit_type generics
                             current_module le)
                    | SAffection (s, le) ->
                        SAffection
                          ( s,
                            remap_located_expr_explicit_type generics
                              current_module le )
                    | SDeclaration
                        {
                          is_const;
                          variable_name;
                          explicit_type;
                          expression = ex;
                        } ->
                        SDeclaration
                          {
                            is_const;
                            variable_name;
                            explicit_type =
                              explicit_type
                              |> Option.map
                                   (Position.map
                                      (Type.set_module_path generics
                                         current_module));
                            expression =
                              remap_located_expr_explicit_type generics
                                current_module ex;
                          })),
      remap_located_expr_explicit_type generics current_module expr_located )

  and remap_located_expr_explicit_type generics current_module located_expr =
    located_expr
    |> Position.map (remap_expr_explicit_type generics current_module)

  and remap_expr_explicit_type generics current_module = function
    | ESizeof (Either.Left ktype) ->
        ESizeof
          (Either.Left
             (ktype
             |> Position.map (Type.set_module_path generics current_module)))
    | ESizeof (Either.Right expr) ->
        ESizeof
          (Either.Right
             (expr |> remap_located_expr_explicit_type generics current_module))
    | EFieldAcces { first_expr; field } ->
        EFieldAcces
          {
            first_expr =
              remap_located_expr_explicit_type generics current_module
                first_expr;
            field;
          }
    | ETupleAccess {first_expr; index} ->
        ETupleAccess 
        {
          first_expr = remap_located_expr_explicit_type generics current_module
                first_expr;
          index
        }
    | EConst_Identifier { modules_path; identifier } ->
        EConst_Identifier
          { modules_path = abs_module current_module modules_path; identifier }
    | EStruct { modules_path; struct_name; fields } ->
        EStruct
          {
            modules_path = abs_module current_module modules_path;
            struct_name;
            fields =
              fields
              |> List.map (fun (fn, expr) ->
                     ( fn,
                       remap_located_expr_explicit_type generics current_module
                         expr ));
          }
    | EEnum { modules_path; enum_name; variant; assoc_exprs } ->
        EEnum
          {
            modules_path = abs_module current_module modules_path;
            enum_name;
            variant;
            assoc_exprs =
              assoc_exprs
              |> List.map
                   (remap_located_expr_explicit_type generics current_module);
          }
    | ETuple lexprs ->
        ETuple
          (lexprs
          |> List.map (remap_located_expr_explicit_type generics current_module)
          )
    | EBuiltin_Function_call { fn_name; parameters } ->
        EBuiltin_Function_call
          {
            fn_name;
            parameters =
              parameters
              |> List.map
                   (remap_located_expr_explicit_type generics current_module);
          }
    | EFunction_call { modules_path; generics_resolver; fn_name; parameters } ->
        EFunction_call
          {
            modules_path = abs_module current_module modules_path;
            generics_resolver =
              generics_resolver
              |> Option.map
                   (List.map
                      (Position.map
                         (Type.set_module_path generics current_module)));
            fn_name;
            parameters =
              parameters
              |> List.map
                   (remap_located_expr_explicit_type generics current_module);
          }
    | EIf (if_cond, ifbody, elsebody) ->
        EIf
          ( remap_located_expr_explicit_type generics current_module if_cond,
            remap_body_explicit_type generics current_module ifbody,
            remap_body_explicit_type generics current_module elsebody )
    | ECases { cases; else_case } ->
        ECases
          {
            cases =
              cases
              |> List.map (fun (expr, body) ->
                     ( remap_located_expr_explicit_type generics current_module
                         expr,
                       remap_body_explicit_type generics current_module body ));
            else_case =
              remap_body_explicit_type generics current_module else_case;
          }
    | ESwitch { expression; cases; wildcard_case } ->
        ESwitch
          {
            expression =
              remap_located_expr_explicit_type generics current_module
                expression;
            cases =
              cases
              |> List.map (fun (scs, body) ->
                     (scs, remap_body_explicit_type generics current_module body));
            wildcard_case =
              wildcard_case
              |> Option.map (remap_body_explicit_type generics current_module);
          }
    | EWhile (te, body) ->
        EWhile
          ( remap_located_expr_explicit_type generics current_module te,
            remap_body_explicit_type generics current_module body )
    | EUn_op (UMinus expr) ->
        EUn_op
          (UMinus
             (remap_located_expr_explicit_type generics current_module expr))
    | EUn_op (UNot expr) ->
        EUn_op
          (UNot (remap_located_expr_explicit_type generics current_module expr))
    | EBin_op (BAdd (lhs, rhs)) ->
        EBin_op
          (BAdd
             ( remap_located_expr_explicit_type generics current_module lhs,
               remap_located_expr_explicit_type generics current_module rhs ))
    | EBin_op (BMinus (lhs, rhs)) ->
        EBin_op
          (BMinus
             ( remap_located_expr_explicit_type generics current_module lhs,
               remap_located_expr_explicit_type generics current_module rhs ))
    | EBin_op (BMult (lhs, rhs)) ->
        EBin_op
          (BMult
             ( remap_located_expr_explicit_type generics current_module lhs,
               remap_located_expr_explicit_type generics current_module rhs ))
    | EBin_op (BDiv (lhs, rhs)) ->
        EBin_op
          (BDiv
             ( remap_located_expr_explicit_type generics current_module lhs,
               remap_located_expr_explicit_type generics current_module rhs ))
    | EBin_op (BMod (lhs, rhs)) ->
        EBin_op
          (BMod
             ( remap_located_expr_explicit_type generics current_module lhs,
               remap_located_expr_explicit_type generics current_module rhs ))
    | EBin_op (BBitwiseAnd (lhs, rhs)) ->
        EBin_op
          (BBitwiseAnd
             ( remap_located_expr_explicit_type generics current_module lhs,
               remap_located_expr_explicit_type generics current_module rhs ))
    | EBin_op (BBitwiseOr (lhs, rhs)) ->
        EBin_op
          (BBitwiseOr
             ( remap_located_expr_explicit_type generics current_module lhs,
               remap_located_expr_explicit_type generics current_module rhs ))
    | EBin_op (BBitwiseXor (lhs, rhs)) ->
        EBin_op
          (BBitwiseXor
             ( remap_located_expr_explicit_type generics current_module lhs,
               remap_located_expr_explicit_type generics current_module rhs ))
    | EBin_op (BShiftLeft (lhs, rhs)) ->
        EBin_op
          (BShiftLeft
             ( remap_located_expr_explicit_type generics current_module lhs,
               remap_located_expr_explicit_type generics current_module rhs ))
    | EBin_op (BShiftRight (lhs, rhs)) ->
        EBin_op
          (BShiftRight
             ( remap_located_expr_explicit_type generics current_module lhs,
               remap_located_expr_explicit_type generics current_module rhs ))
    | EBin_op (BAnd (lhs, rhs)) ->
        EBin_op
          (BAnd
             ( remap_located_expr_explicit_type generics current_module lhs,
               remap_located_expr_explicit_type generics current_module rhs ))
    | EBin_op (BOr (lhs, rhs)) ->
        EBin_op
          (BOr
             ( remap_located_expr_explicit_type generics current_module lhs,
               remap_located_expr_explicit_type generics current_module rhs ))
    | EBin_op (BSup (lhs, rhs)) ->
        EBin_op
          (BSup
             ( remap_located_expr_explicit_type generics current_module lhs,
               remap_located_expr_explicit_type generics current_module rhs ))
    | EBin_op (BSupEq (lhs, rhs)) ->
        EBin_op
          (BSupEq
             ( remap_located_expr_explicit_type generics current_module lhs,
               remap_located_expr_explicit_type generics current_module rhs ))
    | EBin_op (BInf (lhs, rhs)) ->
        EBin_op
          (BInf
             ( remap_located_expr_explicit_type generics current_module lhs,
               remap_located_expr_explicit_type generics current_module rhs ))
    | EBin_op (BInfEq (lhs, rhs)) ->
        EBin_op
          (BInfEq
             ( remap_located_expr_explicit_type generics current_module lhs,
               remap_located_expr_explicit_type generics current_module rhs ))
    | EBin_op (BEqual (lhs, rhs)) ->
        EBin_op
          (BEqual
             ( remap_located_expr_explicit_type generics current_module lhs,
               remap_located_expr_explicit_type generics current_module rhs ))
    | EBin_op (BDif (lhs, rhs)) ->
        EBin_op
          (BDif
             ( remap_located_expr_explicit_type generics current_module lhs,
               remap_located_expr_explicit_type generics current_module rhs ))
    | EBin_op (BCmp (lhs, rhs)) ->
        EBin_op
          (BCmp
             ( remap_located_expr_explicit_type generics current_module lhs,
               remap_located_expr_explicit_type generics current_module rhs ))
    | ( Empty | True | False | ECmpEqual | ECmpLess | ECmpGreater | ENullptr
      | EInteger _ | EFloat _ | EChar _ | EString _ | EAdress _
      | EDeference (_, _)
      | EIdentifier _ ) as t ->
        t
end

module Switch_case = struct
  type t = switch_case

  let variant_name = function
    | SC_Enum_Identifier { variant }
    | SC_Enum_Identifier_Assoc { variant; assoc_ids = _ } ->
        variant

  let assoc_binding = function
    | SC_Enum_Identifier _ -> []
    | SC_Enum_Identifier_Assoc e -> e.assoc_ids

  let is_case_matched (variant, (assoc_types : ktype location list))
      (switch_case : t) =
    match switch_case with
    | SC_Enum_Identifier { variant = matched_variant } ->
        matched_variant.v = variant.v && assoc_types |> List.length = 0
    | SC_Enum_Identifier_Assoc { variant = matched_variant; assoc_ids } ->
        matched_variant.v = variant.v
        && Util.are_same_lenght assoc_ids assoc_types

  let is_cases_matched variant (switch_cases : t list) =
    switch_cases |> List.exists (is_case_matched variant)

  let is_cases_duplicated variant (switch_cases : t list) =
    let open Util.Occurence in
    switch_cases
    |> find_occurence (fun sc ->
           sc |> variant_name |> Position.value |> ( = ) variant)
    |> function
    | Multiple _ -> true
    | _ -> false

  let cases_duplicated variant (switch_cases : t list) =
    let open Util.Occurence in
    switch_cases
    |> find_map_occurence (fun sc ->
           let vn_loc = sc |> variant_name in
           if vn_loc.v = variant then Some vn_loc else None)
    |> function
    | Multiple t -> Some (t |> List.rev |> List.hd)
    | _ -> None
end

module Enum = struct
  type t = enum_decl

  let rec are_type_compatible (init_type : ktype) (expected_type : ktype)
      (enum_decl : t) =
    (* let () = Printf.printf "expected : %s :: found : %s\n" (string_of_ktype expected_type) (string_of_ktype init_type) in *)
    match (init_type, expected_type) with
    | ( TType_Identifier { module_path = init_path; name = init_name },
        TType_Identifier { module_path = exp_path; name = exp_name } ) ->
        enum_decl.generics |> List.map Position.value |> List.mem exp_name.v
        || (init_path.v = exp_path.v && init_name.v = exp_name.v)
    | ( TParametric_identifier
          {
            module_path = init_path;
            parametrics_type = init_pt;
            name = init_name;
          },
        TParametric_identifier
          { module_path = exp_path; parametrics_type = exp_pt; name = exp_name }
      ) ->
        if
          init_path.v <> exp_path.v || init_name.v <> exp_name.v
          || List.compare_lengths init_pt exp_pt <> 0
        then false
        else
          List.combine init_pt exp_pt
          |> List.for_all (fun (i, e) -> are_type_compatible i.v e.v enum_decl)
    | TPointer l_type, TPointer r_type ->
        are_type_compatible l_type.v r_type.v enum_decl
    | TTuple lhs, TTuple rhs ->
        Util.are_same_lenght lhs rhs
        && List.combine lhs rhs
           |> List.for_all (fun (lhs_type, rhs_type) ->
                  are_type_compatible lhs_type.v rhs_type.v enum_decl)
    | TUnknow, _ -> true
    | _, TType_Identifier { module_path; name }
      when module_path.v = ""
           && enum_decl.generics |> List.map Position.value |> List.mem name.v
      ->
        true
    | lhs, rhs -> Ast.Type.(lhs === rhs)

  let rec is_type_compatible_hashgen generic_table (init_type : ktype)
      (expected_type : ktype) (enum_decl : t) =
    let open Ast.Type in
    match (init_type, expected_type) with
    | kt, TType_Identifier { module_path = { v = ""; _ }; name }
      when match Hashtbl.find_opt generic_table name.v with
           | None -> false
           | Some (_, find_kt) ->
               if find_kt === TUnknow then
                 let () =
                   Hashtbl.replace generic_table name.v
                     ( enum_decl.generics
                       |> Util.ListHelper.index_of (fun ge -> ge.v = name.v),
                       kt )
                 in
                 true
               else Ast.Type.(find_kt === kt) ->
        true
    | ( TType_Identifier { module_path = init_path; name = init_name },
        TType_Identifier { module_path = exp_path; name = exp_name } ) ->
        enum_decl.generics |> List.map Position.value |> List.mem exp_name.v
        || (init_path.v = exp_path.v && init_name.v = exp_name.v)
    | ( TParametric_identifier
          {
            module_path = init_path;
            parametrics_type = init_pt;
            name = init_name;
          },
        TParametric_identifier
          { module_path = exp_path; parametrics_type = exp_pt; name = exp_name }
      ) ->
        if
          init_path.v <> exp_path.v || init_name.v <> exp_name.v
          || List.compare_lengths init_pt exp_pt <> 0
        then false
        else
          List.combine init_pt exp_pt
          |> List.for_all (fun (i, e) ->
                 is_type_compatible_hashgen generic_table i.v e.v enum_decl)
    | TUnknow, _ -> true
    | TPointer l_type, TPointer r_type ->
        is_type_compatible_hashgen generic_table l_type.v r_type.v enum_decl
    | TTuple lhs, TTuple rhs ->
        Util.are_same_lenght lhs rhs
        && List.combine lhs rhs
           |> List.for_all (fun (lhs_type, rhs_type) ->
                  is_type_compatible_hashgen generic_table lhs_type.v rhs_type.v
                    enum_decl)
    | lhs, rhs -> Ast.Type.(lhs === rhs)

  let to_ktype_hash generics module_def_path (enum_decl : t) =
    if enum_decl.generics = [] then
      TType_Identifier
        { module_path = module_def_path; name = enum_decl.enum_name }
    else
      TParametric_identifier
        {
          module_path = module_def_path;
          parametrics_type =
            generics |> Hashtbl.to_seq |> List.of_seq
            |> List.sort (fun (_, (i, _)) (_, (b, _)) -> compare i b)
            |> List.map (fun (_, (_, kt)) ->
                   { v = kt; position = Position.dummy });
          name = enum_decl.enum_name;
        }

  let is_valide_assoc_type_init ~init_types ~expected_types enum_decl =
    Util.are_same_lenght init_types expected_types
    && List.combine init_types expected_types
       |> List.for_all (fun (init_type, expected_type) ->
              are_type_compatible init_type expected_type enum_decl)

  let is_simple_enum (enum_decl : t) =
    enum_decl.variants |> List.for_all (fun (_, assoc) -> assoc = [])

  let is_tagged_union (enum_decl : t) =
    enum_decl.variants |> List.exists (fun (_, assoc_type) -> assoc_type <> [])

  let contains_generics (enum_decl : t) = enum_decl.generics <> []

  let is_valid_case_match (switch_case : switch_case) (enum_decl : t) =
    let open Ast.Error in
    let ( >>= ) = Result.bind in
    let variant, given_len =
      match switch_case with
      | SC_Enum_Identifier { variant } -> (variant, 0)
      | SC_Enum_Identifier_Assoc { variant; assoc_ids } ->
          (variant, assoc_ids |> List.length)
    in
    enum_decl.variants
    |> List.find_map (fun (variant_enum, assoc_types) ->
           if variant.v = variant_enum.v then Some (variant, assoc_types)
           else None)
    |> Option.to_result ~none:(Variant_not_found { enum_decl; variant })
    >>= fun (_, assoc_types) ->
    let assoc_len = assoc_types |> List.length in
    if assoc_len = given_len then Ok ()
    else
      Error
        (Mismatched_Assoc_length
           { variant; expected = assoc_len; found = given_len })

  let is_all_cases_handled ~expression (switch_cases : switch_case list)
      (enum_decl : t) =
    let open Ast.Error in
    let _, missing =
      enum_decl.variants
      |> List.partition_map (fun enum_variant ->
             if Switch_case.is_cases_matched enum_variant switch_cases then
               Either.left enum_variant
             else Either.right enum_variant)
    in

    match missing with
    | [] ->
        switch_cases
        |> List.fold_left
             (fun acc sc ->
               if acc |> Result.is_error then acc
               else is_valid_case_match sc enum_decl)
             (Result.ok ())
    | t :: _ ->
        Error
          (Not_all_cases_handled
             { expression_loc = expression; missing_variant = t })

  let rec is_type_generic ktype (enum_decl : t) =
    match ktype with
    | TType_Identifier { module_path = { v = ""; _ }; name } ->
        enum_decl.generics |> List.map Position.value |> List.mem name.v
    | TParametric_identifier { module_path = _; parametrics_type; name = _ } ->
        parametrics_type
        |> List.exists (fun kt -> is_type_generic kt.v enum_decl)
    | TPointer kt -> is_type_generic kt.v enum_decl
    | TTuple kts ->
        kts |> List.exists (fun kt -> is_type_generic kt.v enum_decl)
    | _ -> false

  let extract_assoc_type_variant generics variant (enum_decl : t) =
    let open Ast.Type in
    enum_decl.variants
    |> List.find_map (fun (case, assoc_type) ->
           if case.v = variant.v then Some assoc_type else None)
    |> Option.map (fun assoc_ktypes ->
           assoc_ktypes
           |> List.map (fun kt ->
                  generics
                  |> List.find_map (fun (gen_kt, associated_gen_value) ->
                         if gen_kt === kt.v then Some associated_gen_value
                         else None)
                  |> Option.value ~default:kt))

  let extract_assoc_remap_type_variant generics variant (enum_decl : t) =
    enum_decl.variants
    |> List.find_map (fun (case, assoc_type) ->
           if case.v = variant.v then Some assoc_type else None)
    |> Option.map (fun assoc_ktypes ->
           assoc_ktypes
           |> List.map
                (Position.map (Ast.Type.remap_naif_generic_ktype generics)))

  let is_ktype_generic_level_zero ktype (enum_decl : t) =
    match ktype with
    | TType_Identifier { module_path = { v = ""; _ }; name } ->
        enum_decl.generics |> List.map Position.value |> List.mem name.v
    | _ -> false

  let remove_level_zero_genenics ktypes (enum_decl : t) =
    ktypes
    |> List.filter (fun kt -> is_ktype_generic_level_zero kt enum_decl |> not)

  let reduce_binded_variable (assoc_binded : string option list)
      (ktype_list : ktype list) =
    ktype_list |> List.combine assoc_binded
    |> List.filter_map (fun (name, ktypes) ->
           match name with None -> None | Some s -> Some (s, ktypes))

  let reduce_binded_variable_combine assoc =
    assoc
    |> List.filter_map (fun (index, name, ktype) ->
           match name with None -> None | Some s -> Some (index, s, ktype))

  let bind_enum_decl (ktypes : ktype list) primitive_generics (enum_decl : t) =
    let combined = List.combine enum_decl.generics ktypes in
    {
      enum_decl with
      generics = primitive_generics;
      variants =
        enum_decl.variants
        |> List.map (fun (name, kts) ->
               ( name,
                 kts
                 |> List.map
                      (Position.map
                         (Ast.Type.map_generics_type combined primitive_generics))
               ));
    }

  let rename_parameter_explicit_module new_module_path (enum_decl : t) =
    {
      enum_decl with
      variants =
        enum_decl.variants
        |> List.map (fun (variant, assoc_types) ->
               ( variant,
                 assoc_types
                 |> List.map
                      (Position.map
                         (Type.set_module_path enum_decl.generics
                            new_module_path)) ));
    }
end

module Struct = struct
  type t = struct_decl

  let bind_struct_decl (ktypes : ktype list) primitive_generics
      (struct_decl : t) =
    let combined = List.combine struct_decl.generics ktypes in
    {
      struct_decl with
      generics = primitive_generics;
      fields =
        struct_decl.fields
        |> List.map (fun (name, kt) ->
               ( name,
                 kt
                 |> Position.map
                      (Ast.Type.map_generics_type combined primitive_generics)
               ));
    }

  let contains_generics (struct_decl : t) = struct_decl.generics <> []

  let rec is_type_generic ktype (struct_decl : t) =
    match ktype with
    | TType_Identifier { module_path = { v = ""; _ }; name } ->
        struct_decl.generics |> List.map Position.value |> List.mem name.v
    | TParametric_identifier { module_path = _; parametrics_type; name = _ } ->
        parametrics_type
        |> List.exists (fun kt -> is_type_generic kt.v struct_decl)
    | TPointer kt -> is_type_generic kt.v struct_decl
    | TTuple kts ->
        kts |> List.exists (fun kt -> is_type_generic kt.v struct_decl)
    | _ -> false

  let to_ktype ?(position = Position.dummy) (module_def_path : string)
      (struct_decl : t) =
    if not (struct_decl |> contains_generics) then
      TType_Identifier
        {
          module_path = { v = module_def_path; position };
          name = struct_decl.struct_name;
        }
    else
      TParametric_identifier
        {
          module_path = { v = module_def_path; position };
          parametrics_type =
            struct_decl.generics |> List.map (Position.map (fun _ -> TUnknow));
          name = struct_decl.struct_name;
        }

  let ktype_of_field_gen ~current_module (parametrics_types : ktype list)
      (field : string) (struct_decl : t) =
    let list_len = parametrics_types |> List.length in
    let dummy_list = List.init list_len (fun _ -> ()) in
    let dummy_parametrics = List.combine dummy_list parametrics_types in
    let generics_mapped =
      List.combine
        (struct_decl.generics |> List.map Position.value)
        dummy_parametrics
    in
    let hashtbl = Hashtbl.of_seq (generics_mapped |> List.to_seq) in
    struct_decl.fields
    |> List.map Position.assocs_value
    |> List.assoc_opt field
    |> Option.map (fun kt ->
           if not (is_type_generic kt struct_decl) then kt
           else Type.remap_generic_ktype ~current_module hashtbl kt)

  let resolve_fields_access_gen (parametrics_types : ktype list)
      (field : string location) (type_decl : Ast.Type_Decl.type_decl)
      (current_mod_name : string) =
    let open Ast.Type_Decl in
    let open Ast.Error in
    match type_decl with
    | Decl_Enum enum_decl ->
        Enum_Access_field { field; enum_decl } |> ast_error |> raise
    | Decl_Struct struct_decl -> (
        match
          ktype_of_field_gen ~current_module:current_mod_name parametrics_types
            field.v struct_decl
        with
        | None ->
            Impossible_field_Access { field; struct_decl } |> ast_error |> raise
        | Some kt -> kt)

  let is_ktype_generic_level_zero ktype (struct_decl : t) =
    match ktype with
    | TType_Identifier { module_path = { v = ""; _ }; name } ->
        struct_decl.generics |> List.map Position.value |> List.mem name.v
    | _ -> false

  let remove_level_zero_genenics ktypes (struct_decl : t) =
    ktypes
    |> List.filter (fun kt -> is_ktype_generic_level_zero kt struct_decl |> not)

  let rec is_type_compatible_hashgen generic_table (init_type : ktype)
      (expected_type : ktype) (struct_decl : t) =
    match (init_type, expected_type) with
    | kt, TType_Identifier { module_path; name }
      when match Hashtbl.find_opt generic_table name.v with
           | None ->
               if
                 module_path.v = ""
                 && struct_decl.generics |> List.map Position.value
                    |> List.mem name.v
               then
                 let () =
                   Hashtbl.replace generic_table name.v
                     ( struct_decl.generics |> List.map Position.value
                       |> Util.ListHelper.index_of (( = ) name.v),
                       kt )
                 in
                 true
               else false
           | Some (_, find_kt) -> Ast.Type.(find_kt === kt) ->
        true
    | ( TType_Identifier { module_path = init_path; name = init_name },
        TType_Identifier { module_path = exp_path; name = exp_name } ) ->
        struct_decl.generics |> List.map Position.value |> List.mem exp_name.v
        || (init_path.v = exp_path.v && init_name.v = exp_name.v)
    | ( TParametric_identifier
          {
            module_path = init_path;
            parametrics_type = init_pt;
            name = init_name;
          },
        TParametric_identifier
          { module_path = exp_path; parametrics_type = exp_pt; name = exp_name }
      ) ->
        if
          init_path.v <> exp_path.v || init_name.v <> exp_name.v
          || List.compare_lengths init_pt exp_pt <> 0
        then false
        else
          List.combine init_pt exp_pt
          |> List.map Position.assocs_value
          |> List.for_all (fun (i, e) ->
                 is_type_compatible_hashgen generic_table i e struct_decl)
    | TPointer lhs, TPointer rhs ->
        is_type_compatible_hashgen generic_table lhs.v rhs.v struct_decl
    | TTuple lhs, TTuple rhs ->
        Util.are_same_lenght lhs rhs
        && List.for_all2
             (fun init exptected ->
               is_type_compatible_hashgen generic_table init exptected
                 struct_decl)
             (lhs |> List.map value)
             (rhs |> List.map value)
    | TUnknow, _ -> true
    | lhs, rhs -> Ast.Type.(lhs === rhs)

  let to_ktype_hash generics module_def_path (struct_decl : t) =
    if struct_decl.generics = [] then
      TType_Identifier
        { module_path = module_def_path; name = struct_decl.struct_name }
    else
      TParametric_identifier
        {
          module_path = module_def_path;
          parametrics_type =
            generics |> Hashtbl.to_seq |> List.of_seq
            |> List.sort (fun (_, (i, _)) (_, (b, _)) -> compare i b)
            |> List.map (fun (_, (_, kt)) ->
                   { v = kt; position = Position.dummy });
          name = struct_decl.struct_name;
        }

  let rename_parameter_explicit_module new_module_path (struct_decl : t) =
    {
      struct_decl with
      fields =
        struct_decl.fields
        |> List.map (fun (field, ktype) ->
               ( field,
                 ktype
                 |> Position.map
                      (Ast.Type.set_module_path struct_decl.generics
                         new_module_path) ));
    }
end

module Type_Decl = struct
  type t = Ast.Type_Decl.type_decl

  let is_enum = function Ast.Type_Decl.Decl_Enum _ -> true | _ -> false
  let is_struct = function Ast.Type_Decl.Decl_Struct _ -> true | _ -> false

  let type_name = function
    | Ast.Type_Decl.Decl_Enum e -> e.enum_name
    | Ast.Type_Decl.Decl_Struct s -> s.struct_name

  let generics = function
    | Ast.Type_Decl.Decl_Enum e -> e.generics
    | Ast.Type_Decl.Decl_Struct s -> s.generics

  let is_ktype_generic kt = function
    | Ast.Type_Decl.Decl_Enum e -> e |> Enum.is_type_generic kt
    | Ast.Type_Decl.Decl_Struct s -> s |> Struct.is_type_generic kt

  let is_ktype_generic_level_zero kt = function
    | Ast.Type_Decl.Decl_Enum e -> e |> Enum.is_ktype_generic_level_zero kt
    | Ast.Type_Decl.Decl_Struct s -> s |> Struct.is_ktype_generic_level_zero kt

  let remove_level_zero_genenics kts = function
    | Ast.Type_Decl.Decl_Enum e -> e |> Enum.remove_level_zero_genenics kts
    | Ast.Type_Decl.Decl_Struct s -> s |> Struct.remove_level_zero_genenics kts

  let are_same_type_decl lhs rhs =
    lhs |> type_name |> Position.value = (rhs |> type_name |> Position.value)
end

module ExternalFunc = struct
  type t = external_func_decl

  let rename_parameter_explicit_module current_module (external_func : t) =
    {
      external_func with
      fn_parameters =
        external_func.fn_parameters
        |> List.map (Position.map (Ast.Type.set_module_path [] current_module));
      r_type =
        external_func.r_type
        |> Position.map (Ast.Type.set_module_path [] current_module);
    }
end

module Syscall = struct
  type t = syscall_decl

  let rename_parameter_explicit_module current_module (syscall_decl : t) =
    {
      syscall_decl with
      parameters =
        syscall_decl.parameters
        |> List.map (Position.map (Ast.Type.set_module_path [] current_module));
      return_type =
        syscall_decl.return_type
        |> Position.map (Ast.Type.set_module_path [] current_module);
    }
end

module Builtin_Function = struct
  type t = Ast.Builtin_Function.functions

  let fn_name_of_built_in_fn =
    let open Ast.Builtin_Function in
    function
    | Tos8 -> "tos8"
    | Tou8 -> "tou8"
    | Tos16 -> "tos16"
    | Tou16 -> "tou16"
    | Tof32 -> "tof32"
    | Tos32 -> "tos32"
    | Tou32 -> "tou32"
    | Tos64 -> "tos64"
    | Tou64 -> "tou64"
    | Tof64 -> "tof64"
    | Stringl_ptr -> "stringlptr"

  let builtin_fn_of_fn_name =
    let open Ast.Builtin_Function in
    function
    | { v = "tos8"; _ } -> Tos8 |> Result.ok
    | { v = "tou8"; _ } -> Tou8 |> Result.ok
    | { v = "tos16"; _ } -> Tos16 |> Result.ok
    | { v = "tou16"; _ } -> Tou16 |> Result.ok
    | { v = "tos32"; _ } -> Tos32 |> Result.ok
    | { v = "tou32"; _ } -> Tou32 |> Result.ok
    | { v = "tos64"; _ } -> Tos64 |> Result.ok
    | { v = "tou64"; _ } -> Tou64 |> Result.ok
    | { v = "tof64"; _ } -> Tof64 |> Result.ok
    | { v = "tof32"; _ } -> Tof32 |> Result.ok
    | { v = "stringlptr"; _ } -> Stringl_ptr |> Result.ok
    | _ as fn_name -> Ast.Error.Unknow_built_function fn_name |> Result.error

  let is_valide_parameters_type fn_location parameters =
    let open Ast.Builtin_Function in
    function
    | ( Tos8 | Tou8 | Tos16 | Tou16 | Tos32 | Tou32 | Tos64 | Tou64 | Tof32
      | Tof64 ) as fn -> (
        match parameters with
        | [ t ] ->
            let param_type = Position.value t in
            if fn = Tou8 && param_type = TChar then Result.ok fn
            else if
              Type.is_any_integer param_type || Type.is_any_float param_type
            then Result.ok fn
            else
              Ast.Error.Found_no_Integer { fn_name = fn_location.v; found = t }
              |> Result.error
        | list ->
            Ast.Error.Mismatched_Parameters_Length
              {
                fn_name = fn_location;
                expected = 1;
                found = list |> List.length;
              }
            |> Result.error)
    | Stringl_ptr -> (
        match parameters with
        | [ t ] ->
            if t |> Position.value |> Type.is_string_litteral then
              Result.ok Stringl_ptr
            else
              Ast.Error.Wrong_parameters
                { fn_name = fn_location.v; expected = TString_lit; found = t }
              |> Result.error
        | list ->
            Ast.Error.Mismatched_Parameters_Length
              {
                fn_name = fn_location;
                expected = 1;
                found = list |> List.length;
              }
            |> Result.error)

  let builtin_return_type =
    let open Ast.Builtin_Function in
    function
    | Stringl_ptr ->
        TPointer { v = TInteger (Signed, I8); position = Position.dummy }
    | Tos8 -> TInteger (Signed, I8)
    | Tou8 -> TInteger (Unsigned, I8)
    | Tos16 -> TInteger (Signed, I16)
    | Tou16 -> TInteger (Unsigned, I16)
    | Tos32 -> TInteger (Signed, I32)
    | Tou32 -> TInteger (Unsigned, I32)
    | Tos64 -> TInteger (Signed, I64)
    | Tou64 -> TInteger (Unsigned, I64)
    | Tof32 -> TFloat F32
    | Tof64 -> TFloat F64
end

module Function = struct
  type t = function_decl

  let rename_parameter_explicit_module current_module (function_decl : t) =
    {
      function_decl with
      parameters =
        function_decl.parameters
        |> List.map (fun (field, ktype) ->
               ( field,
                 ktype
                 |> Position.map
                      (Ast.Type.set_module_path function_decl.generics
                         current_module) ));
      return_type =
        function_decl.return_type
        |> Position.map
             (Ast.Type.set_module_path function_decl.generics current_module);
      body =
        Kbody.remap_body_explicit_type function_decl.generics current_module
          function_decl.body;
    }

  let rec is_ktype_generic ktype (fn_decl : t) =
    match ktype with
    | TParametric_identifier { module_path = _; parametrics_type; name = _ } ->
        parametrics_type
        |> List.exists (fun kt -> is_ktype_generic kt.v fn_decl)
    | TType_Identifier { module_path = { v = ""; _ }; name } ->
        fn_decl.generics |> List.map Position.value |> List.mem name.v
    | TPointer ktl -> is_ktype_generic ktl.v fn_decl
    | TTuple ktls ->
        ktls |> List.exists (fun ktl -> is_ktype_generic ktl.v fn_decl)
    | _ -> false

  (**
    @return true if the generics is the immediat type and not nested into a parametric type
  *)
  let is_ktype_generic_level_zero ktype (fn_decl : t) =
    match ktype with
    | TType_Identifier { module_path = { v = ""; _ }; name } ->
        fn_decl.generics |> List.map Position.value |> List.mem name.v
    | _ -> false

  let is_generic_used_in_parameters (fn_decl : t) =
    fn_decl.parameters
    |> List.exists (fun (_, lkt) -> is_ktype_generic lkt.v fn_decl)

  let does_need_generic_resolver (function_decl : t) =
    if function_decl.generics = [] then false
    else if function_decl.parameters |> List.length = 0 then true
    else function_decl |> is_generic_used_in_parameters |> not
  (* else
     function_decl |> is_ktype_generic_level_zero function_decl.return_type.v *)

  (* (**
       @return : Returns [Some name] if the associated type with the field is generics else [None] if not or the field name doesn't exist
     *)
     let assoc_generics_name_of_field field (fn_decl: function_decl) =
       let (>>:) = Option.bind in
       fn_decl.parameters
       |> List.find_map (fun (field_name, ktype ) -> if field_name = field then Some ktype else None)
       >>: (Type.type_name_opt)
       >>: (fun s -> if fn_decl.generics |> List.map Position.value |> List.mem s then Some s else None) *)

  let rec is_type_compatible_hashgen generic_table (init_type : ktype)
      (expected_type : ktype) (function_decl : t) =
    (* let () = Printf.printf "fn_name = %s : init_type = %s, expected_type = %s\n" (function_decl.fn_name.v) (Pprint.string_of_ktype init_type) (Pprint.string_of_ktype expected_type) in
       let () = Printf.printf "TIdentifier = %b\n\n" (match expected_type with TType_Identifier _ -> true | _ -> false) in *)
    let open Ast.Type in
    match (init_type, expected_type) with
    | kt, TType_Identifier { module_path = { v = ""; _ }; name }
      when match Hashtbl.find_opt generic_table name.v with
           | None ->
               if
                 function_decl.generics |> List.map Position.value
                 |> List.mem name.v
               then
                 let () =
                   Hashtbl.replace generic_table name.v
                     ( function_decl.generics
                       |> Util.ListHelper.index_of (fun g -> g.v = name.v),
                       kt )
                 in
                 true
               else false
           | Some (_, find_kt) -> are_compatible_type find_kt kt ->
        true
    | ( TType_Identifier { module_path = init_path; name = init_name },
        TType_Identifier { module_path = exp_path; name = exp_name } ) ->
        function_decl.generics |> List.map Position.value |> List.mem exp_name.v
        || (init_path.v = exp_path.v && init_name.v = exp_name.v)
    | ( TParametric_identifier
          {
            module_path = init_path;
            parametrics_type = init_pt;
            name = init_name;
          },
        TParametric_identifier
          { module_path = exp_path; parametrics_type = exp_pt; name = exp_name }
      ) ->
        if
          init_path.v <> exp_path.v || init_name.v <> exp_name.v
          || List.compare_lengths init_pt exp_pt <> 0
        then false
        else
          List.combine init_pt exp_pt
          |> List.for_all (fun (i, e) ->
                 is_type_compatible_hashgen generic_table i.v e.v function_decl)
    | TUnknow, _ -> true
    | TPointer _, TPointer { v = TUnknow; _ } -> true
    | TPointer lhs, TPointer rhs ->
        is_type_compatible_hashgen generic_table lhs.v rhs.v function_decl
    | TTuple lhs, TTuple rhs ->
        Util.are_same_lenght lhs rhs
        && List.for_all2
             (fun lkt rkt ->
               is_type_compatible_hashgen generic_table lkt.v rkt.v
                 function_decl)
             lhs rhs
    | lhs, rhs -> lhs === rhs

  let to_return_ktype_hashtab ~current_module ~module_type_path generic_table
      (function_decl : t) =
    if
      function_decl.generics = []
      || function_decl |> is_ktype_generic function_decl.return_type.v |> not
    then
      Ast.Type.module_path_return_type ~current_module ~module_type_path
        function_decl.return_type.v
    else
      Ast.Type.remap_generic_ktype ~current_module generic_table
        function_decl.return_type.v

  let iter_statement fn (function_decl : t) =
    let statements, _ = function_decl.body in
    statements |> List.iter fn
end

module ParserOperator = struct
  let string_of_parser_unary = function PNot -> "(!)" | PUMinus -> "(.-)"

  let string_name_of_parser_unary = function
    | PNot -> "unot"
    | PUMinus -> "uminus"

  let string_of_parser_binary = function
    | Add -> "+"
    | Minus -> "-"
    | Mult -> "*"
    | Div -> "/"
    | Modulo -> "%"
    | BitwiseAnd -> "&"
    | BitwiseOr -> "|"
    | BitwiseXor -> "^"
    | ShiftLeft -> "<<"
    | ShiftRight -> ">>"
    | Spaceship -> "<=>"
    | Equal -> "=="

  let string_name_of_parser_binary = function
    | Add -> "add"
    | Minus -> "sub"
    | Mult -> "mul"
    | Div -> "div"
    | Modulo -> "mod"
    | BitwiseAnd -> "bwa"
    | BitwiseOr -> "bwo"
    | BitwiseXor -> "bwx"
    | ShiftLeft -> "sfl"
    | ShiftRight -> "sfr"
    | Spaceship -> "shp"
    | Equal -> "eql"

  let parameters = function
    | Unary u -> u.field |> snd |> fun k -> k :: []
    | Binary b ->
        let lhs, rhs = (b.fields |> fst |> snd, b.fields |> snd |> snd) in
        [ lhs; rhs ]

  let string_of_parser_operator = function
    | `unary u -> string_of_parser_unary u
    | `binary b -> string_of_parser_binary b

  let first_parameter_ktype operator_decl =
    match operator_decl with
    | Unary u ->
        let _, kt1 = u.field in
        kt1
    | Binary b ->
        let (_, kt1), (_, _) = b.fields in
        kt1

  let second_parameter_ktype operator_decl =
    match operator_decl with
    | Unary _ -> None
    | Binary b ->
        let (_, _), (_, kt2) = b.fields in
        Some kt2

  let operator = function
    | Unary u -> u.op |> Position.map string_of_parser_unary
    | Binary b -> b.op |> Position.map string_of_parser_binary

  let backticked_operator op =
    op |> operator |> Position.map (Printf.sprintf "`%s`")

  let return_ktype operator_decl =
    match operator_decl with
    | Unary u -> u.return_type
    | Binary b -> b.return_type

  let expected_unary_return_type (ktype : ktype) op =
    match op with Ast.PNot | Ast.PUMinus -> ktype

  let kbody = function
    | Unary { op = _; field = _; return_type = _; kbody }
    | Binary { op = _; fields = _; return_type = _; kbody } ->
        kbody

  let expected_binary_return_type (ktype : ktype) op =
    match op with
    | Add | Minus | Mult | Div | Modulo | BitwiseOr | BitwiseAnd | BitwiseXor
    | ShiftLeft | ShiftRight ->
        ktype
    | Equal -> TBool
    | Spaceship -> TOredered

  let expected_op_return_type ktype op =
    match op with
    | `unary u -> u |> expected_unary_return_type ktype
    | `binary b -> b |> expected_binary_return_type ktype

  let signature = function
    | Unary { op; field; return_type; kbody = _ } ->
        (`unary op, field :: [], return_type)
    | Binary { op; fields = t1, t2; return_type; kbody = _ } ->
        (`binary op, [ t1; t2 ], return_type)

  let rename_parameter_explicit_module new_module_path = function
    | Unary unary ->
        let p1, k1 = unary.field in
        Unary
          {
            unary with
            field =
              (p1, k1 |> Position.map (Type.set_module_path [] new_module_path));
            return_type =
              unary.return_type
              |> Position.map (Type.set_module_path [] new_module_path);
            kbody =
              Kbody.remap_body_explicit_type [] new_module_path unary.kbody;
          }
    | Binary binary ->
        let (p1, k1), (p2, k2) = binary.fields in
        Binary
          {
            binary with
            fields =
              ( ( p1,
                  k1 |> Position.map (Type.set_module_path [] new_module_path)
                ),
                ( p2,
                  k2 |> Position.map (Type.set_module_path [] new_module_path)
                ) );
            return_type =
              binary.return_type
              |> Position.map (Type.set_module_path [] new_module_path);
            kbody =
              Kbody.remap_body_explicit_type [] new_module_path binary.kbody;
          }
end

module AstModif = struct
  let module_node_remove_implicit_type_path new_module_path = function
    | NExternFunc exf ->
        NExternFunc
          (ExternalFunc.rename_parameter_explicit_module new_module_path exf)
    | NSyscall syscall_decl ->
        NSyscall
          (Syscall.rename_parameter_explicit_module new_module_path syscall_decl)
    | NFunction function_decl ->
        NFunction
          (Function.rename_parameter_explicit_module new_module_path
             function_decl)
    | NOperator operator_decl ->
        NOperator
          (ParserOperator.rename_parameter_explicit_module new_module_path
             operator_decl)
    | NEnum enum_decl ->
        NEnum (Enum.rename_parameter_explicit_module new_module_path enum_decl)
    | NStruct struct_decl ->
        NStruct
          (Struct.rename_parameter_explicit_module new_module_path struct_decl)
    | NConst c -> NConst c
end

module Affected_Value = struct
  let rec resolve_fields_access_gen (parametrics_types : ktype list)
      (fields : string location list) (type_decl : Ast.Type_Decl.type_decl)
      (current_mod_name : string) (program : module_path list) =
    let open Ast.Type_Decl in
    let open Ast.Error in
    match fields with
    | [] -> failwith "Unreachable: Empty field access"
    | [ t ] -> (
        match type_decl with
        | Decl_Enum enum_decl ->
            Enum_Access_field { field = t; enum_decl } |> ast_error |> raise
        | Decl_Struct struct_decl -> (
            match
              Struct.ktype_of_field_gen ~current_module:current_mod_name
                parametrics_types t.v struct_decl
            with
            | None ->
                Impossible_field_Access { field = t; struct_decl }
                |> ast_error |> raise
            | Some kt -> kt))
    | t :: q -> (
        match type_decl with
        | Decl_Enum enum_decl ->
            Enum_Access_field { field = t; enum_decl } |> ast_error |> raise
        | Decl_Struct struct_decl -> (
            match
              Struct.ktype_of_field_gen ~current_module:current_mod_name
                parametrics_types t.v struct_decl
            with
            | None ->
                Impossible_field_Access { field = t; struct_decl }
                |> ast_error |> raise
            | Some kt ->
                let parametrics_types_two = Type.extract_parametrics_ktype kt in
                let ktype_def_path = Type.module_path_opt kt |> Option.get in
                let ktype_name = Type.type_name_opt kt |> Option.get in
                let type_decl_two =
                  match
                    Program.find_type_decl_from_ktype ~ktype_def_path
                      ~ktype_name ~current_module:current_mod_name program
                  with
                  | Error e -> e |> Ast.Error.ast_error |> raise
                  | Ok type_decl -> type_decl
                in
                resolve_fields_access_gen
                  (parametrics_types_two |> List.map Position.value)
                  q type_decl_two current_mod_name program))

  (**
      
    @raise ast_error if ktype is not a declared type or if try to access field of enum
    @return ktype of the field
  *)
  let field_type ~variable ktype current_mod_name program fields =
    match Ast.Type.module_path_of_ktype_opt ktype with
    | None ->
        No_struct_field_acc { variable; ktype } |> Ast.Error.ast_error |> raise
    | Some (module_path, typename) ->
        let generics = Ast.Type.extract_parametrics_ktype ktype in
        let type_decl =
          match
            Program.find_type_decl_from_ktype ~ktype_def_path:module_path
              ~ktype_name:typename ~current_module:current_mod_name program
          with
          | Error ast_e -> ast_e |> Ast.Error.ast_error |> raise
          | Ok type_decl -> type_decl
        in
        let field_type =
          resolve_fields_access_gen
            (generics |> List.map Position.value)
            fields type_decl current_mod_name program
        in
        field_type
end
