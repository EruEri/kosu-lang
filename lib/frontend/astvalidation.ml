open Ast
open Asthelper

module Error = struct

  type external_func_error = 
  | Unit_parameter of external_func_decl
  | Not_C_compatible_type of external_func_decl * ktype
  | Too_much_parameters of { limit: int; found: int}


  type syscall_error =
  | Syscall_Unit_parameter of syscall_decl
  | Syscall_Not_C_compatible_type of syscall_decl * ktype
  | Syscall_Too_much_parameters of { limit: int; found: int} 

  type struct_error = 
  | Unknown_type_for_field of string*ktype
  | SCyclic_Declaration of struct_decl
  | SDuplicated_field of struct_decl

  type enum_error =
  | ECyclic_Declaration of enum_decl
  | EDuplicated_variant_name of enum_decl

  type validation_error = 
  | External_Func_Error of external_func_error
  | Syscall_Error of syscall_error
  | Struct_Error of struct_error
  | Enum_Error of enum_error
  | No_Type_decl_found of ktype
  | Too_many_type_decl of ktype

  let external_func_error e = External_Func_Error e
  let syscall_error e = Syscall_Error e
  let struct_error e = Struct_Error e
  let enum_error e = Enum_Error e

  exception Validation_error of validation_error

  let string_of_external_func_error = let open Printf in function
  | Unit_parameter external_func_decl -> sprintf "Unit parameter in %s" external_func_decl.sig_name
  | Not_C_compatible_type (external_func_decl, ktype) -> sprintf "Not_C_compatible_type in %s -- %s --" (external_func_decl.sig_name) (string_of_ktype ktype)
  | Too_much_parameters record -> sprintf "Too_much_parameters -- limit: %d, found: %d --" record.limit record.found

  let string_of_sycall_error = let open Printf in function
  | Syscall_Unit_parameter syscall_decl -> sprintf "Unit parameter in %s" syscall_decl.syscall_name
  | Syscall_Not_C_compatible_type (syscall_decl, ktype) -> sprintf "Not_C_compatible_type in %s -- %s --" (syscall_decl.syscall_name) (string_of_ktype ktype)
  | Syscall_Too_much_parameters record -> sprintf "Too_much_parameters -- limit: %d, found: %d --" record.limit record.found

  let string_of_struct_error = let open Printf in function
  | Unknown_type_for_field (field, ktype) -> sprintf "Unknown_type_for_field : %s -> %s" field (string_of_ktype ktype)
  | SCyclic_Declaration struct_decl -> sprintf "Struct Cyclic_Declaration for %s" struct_decl.struct_name
  | SDuplicated_field struct_decl -> sprintf "Struct Duplicated_field for %s" struct_decl.struct_name

  let string_of_enum_error = let open Printf in function
  | ECyclic_Declaration enum_decl -> sprintf " Enum_Cyclic_Declaration for %s" enum_decl.enum_name
  | EDuplicated_variant_name enum_decl -> sprintf "Enum_Duplicated_variant_name for %s" enum_decl.enum_name

  let string_of_validation_error = let open Printf in function
  | External_Func_Error e -> string_of_external_func_error e
  | Syscall_Error e -> string_of_sycall_error e
  | Struct_Error e -> string_of_struct_error e
  | Enum_Error e -> string_of_enum_error e
  | No_Type_decl_found kt -> sprintf "No_Type_decl_found : %s" (string_of_ktype kt)
  | Too_many_type_decl kt -> sprintf "Too_many_type_decl : %s" (string_of_ktype kt)
end

module Help = struct

  let is_ktype_exist current_module program ktype = 
    match (Ast.Type.module_path_opt ktype , Ast.Type.type_name_opt ktype) with
    | Some ktype_def_path, Some ktype_name -> (
      try
        let _ =  Asthelper.Program.find_type_decl_from_ktype ~ktype_def_path ~ktype_name ~current_module program in
        Ok ()
       with 
         Util.Occurence.No_Occurence -> Error.No_Type_decl_found ktype |> Result.error
       | Util.Occurence.Too_Many_Occurence -> Error.Too_many_type_decl ktype |> Result.error
    )
    | _, _ -> Ok ()         
    
  let rec does_ktype_contains_type_decl current_module program ktype type_decl_to_check = 
      match ktype with
      | TType_Identifier {module_path = ktype_def_path; name = ktype_name} | TParametric_identifier {module_path = ktype_def_path; parametrics_type = _; name = ktype_name} -> (
        match Asthelper.Program.find_type_decl_from_ktype ~ktype_def_path ~ktype_name ~current_module program with
        | Ast.Type_Decl.Decl_Struct struct_decl -> does_contains_type_decl_struct current_module program struct_decl type_decl_to_check
        | Ast.Type_Decl.Decl_Enum enum_decl -> does_contains_type_decl_enum current_module program enum_decl type_decl_to_check
      )
      | TTuple kts -> kts |> List.for_all (fun kt -> does_ktype_contains_type_decl current_module program kt type_decl_to_check)
      | _ -> false
  and does_contains_type_decl_struct current_module program struct_decl type_decl_to_check  = let open Asthelper in
  Type_Decl.decl_struct struct_decl = type_decl_to_check || struct_decl.fields
    |> List.exists (fun (_, kt) -> 
      match kt with
      | TType_Identifier _ when (Struct.is_type_generic kt struct_decl) -> false 
      | TType_Identifier {module_path = ktype_def_path; name = ktype_name} -> (
        let t_decl = Program.find_type_decl_from_ktype ~ktype_def_path ~ktype_name ~current_module program in
        t_decl = type_decl_to_check || t_decl = (Type_Decl.decl_struct struct_decl) || does_ktype_contains_type_decl current_module program kt type_decl_to_check
      )
      | TParametric_identifier {module_path = ktype_def_path; parametrics_type; name = ktype_name} -> (
        let t_decl = Program.find_type_decl_from_ktype ~ktype_def_path ~ktype_name ~current_module program in
        t_decl = type_decl_to_check 
        || parametrics_type |> List.exists (fun ikt -> 
          if struct_decl |> Struct.is_type_generic ikt then false 
          else does_ktype_contains_type_decl current_module program ikt type_decl_to_check 
          )
        || does_ktype_contains_type_decl current_module program kt type_decl_to_check
      )
      | _ -> does_ktype_contains_type_decl current_module program kt type_decl_to_check
      )
  and does_contains_type_decl_enum current_module program (enum_decl: Ast.enum_decl) type_decl_to_check = let open Asthelper in
  (Type_Decl.decl_enum enum_decl) = type_decl_to_check || enum_decl.variants
  |> List.exists (fun (_, kts ) -> 
      kts |> List.exists (fun (kt) -> 
        match kt with
        | TType_Identifier _ when (Enum.is_type_generic kt enum_decl) -> false 
        | TParametric_identifier {module_path = ktype_def_path; parametrics_type; name = ktype_name} -> (
          let t_decl = Program.find_type_decl_from_ktype ~ktype_def_path ~ktype_name ~current_module program in
          t_decl = type_decl_to_check || t_decl = (Type_Decl.decl_enum enum_decl)
          || parametrics_type |> List.exists (fun ikt -> 
            if enum_decl |> Enum.is_type_generic ikt then false 
            else does_ktype_contains_type_decl current_module program ikt type_decl_to_check 
            )
          || does_ktype_contains_type_decl current_module program kt type_decl_to_check
        )
        | TType_Identifier {module_path = ktype_def_path; name = ktype_name} -> (
          let t_decl = Program.find_type_decl_from_ktype ~ktype_def_path ~ktype_name ~current_module program in
          t_decl = type_decl_to_check || t_decl = (Type_Decl.decl_enum enum_decl) || does_ktype_contains_type_decl current_module program kt type_decl_to_check
        )
        | _ -> does_ktype_contains_type_decl current_module program kt type_decl_to_check
    )
  )
  and is_cyclic_struct current_module program struct_decl = 
    struct_decl.fields
    |> List.exists (fun (_, kt) -> not (Asthelper.Struct.is_type_generic kt struct_decl) && does_ktype_contains_type_decl current_module program kt (Type_Decl.decl_struct struct_decl) )
  and is_cyclic_enum current_module program enum_decl = 
    enum_decl.variants
    |> List.map (fun (_, kts) -> kts)
    |> List.flatten
    |> List.exists (fun kt -> not (Asthelper.Enum.is_type_generic kt enum_decl) && does_ktype_contains_type_decl current_module program kt (Type_Decl.decl_enum enum_decl))

end

module ExternalFunction = struct
  let does_parameters_contains_unit (external_func_decl: external_func_decl) = 
    if external_func_decl.fn_parameters |> List.mem TUnit then Error.Unit_parameter external_func_decl |> Error.external_func_error |> Result.error else Ok ()
  
  let does_contains_not_c_compatible_type program current_module (external_func_decl: external_func_decl) = 
    external_func_decl.fn_parameters 
    |> List.filter_map ( fun kt -> 
      try
       if Asthelper.Program.is_c_type_from_ktype current_module kt program |> not then Error.External_Func_Error (Error.Not_C_compatible_type (external_func_decl, kt))  |> Option.some else None
      with 
        Util.Occurence.No_Occurence -> Error.No_Type_decl_found kt  |> Option.some  
      | Util.Occurence.Too_Many_Occurence -> Error.Too_many_type_decl kt |> Option.some
    )
    |> function [] -> Ok () | t::_ -> t |> Result.error
  
  let does_contains_too_much_parameters (external_func_decl: external_func_decl) = 
    let length = external_func_decl.fn_parameters |> List.length in
    if length > 15 then Error.Too_much_parameters {limit = 15; found = length } |> Result.error else Ok ()

  let is_valid_external_function_declaration (program: Ast.program) (current_module_name: string) (external_func_decl: external_func_decl) =
    let (>>=) = Result.bind in
    does_parameters_contains_unit external_func_decl
    >>= (fun () ->
      does_contains_not_c_compatible_type program current_module_name external_func_decl
      )
    >>= (fun () -> 
      does_parameters_contains_unit external_func_decl
      )
end

module Syscall = struct
  let does_parameters_contains_unit (syscall_decl: syscall_decl) = 
    if syscall_decl.parameters |> List.mem TUnit then Error.Syscall_Unit_parameter syscall_decl |> Error.syscall_error |> Result.error else Ok ()
  
    let does_contains_not_c_compatible_type program current_module (syscall_decl: syscall_decl) = 
      syscall_decl.parameters 
      |> List.filter_map ( fun kt -> 
        try
         if Asthelper.Program.is_c_type_from_ktype current_module kt program |> not then Error.Syscall_Not_C_compatible_type (syscall_decl, kt) |> Error.syscall_error |> Option.some else None
        with 
          Util.Occurence.No_Occurence -> Error.No_Type_decl_found kt |> Option.some  
        | Util.Occurence.Too_Many_Occurence -> Error.Too_many_type_decl kt |> Option.some
      )
      |> function [] -> Ok () | t::_ -> t |> Result.error
  
  let does_contains_too_much_parameters (syscall_decl: syscall_decl) = 
    let length = syscall_decl.parameters |> List.length in
    if length > 15 then Error.Syscall_Too_much_parameters {limit = 15; found = length } |> Result.error else Ok ()

  let is_valid_syscall_declaration  (program: Ast.program) (current_module_name: string) (syscall_decl: syscall_decl) =
    let (>>=) = Result.bind in
    does_parameters_contains_unit syscall_decl
    >>= (fun () ->
      does_contains_not_c_compatible_type program current_module_name syscall_decl
      )
    >>= (fun () -> 
      does_parameters_contains_unit syscall_decl
      )
end

module Struct = struct

  let is_all_type_exist current_module program struct_decl =
    struct_decl.fields
    |> List.find_map (fun (_, kt) -> 
      if Asthelper.Struct.is_type_generic kt struct_decl then None 
      else match Help.is_ktype_exist current_module program kt with
      | Ok () -> None
      | Error e -> Some e
    )

  let is_field_duplicate struct_decl = struct_decl.fields 
  |> List.map (fun (field, _) -> field) 
  |> Util.ListHelper.duplicate
  |> Util.are_diff_lenght []

  let is_valid_struct_decl (program: Ast.program) (current_module_name: string) (struct_decl: struct_decl) = 
    let (>>=) = Result.bind in
    is_all_type_exist current_module_name program struct_decl 
    |> (function None -> Ok () | Some e -> Error e)
    >>= (fun () ->     
      if Help.is_cyclic_struct current_module_name program struct_decl then Error.SCyclic_Declaration struct_decl |> Error.struct_error |> Result.error
      else (Ok ()) 
    )
    >>= (fun () -> 
      if is_field_duplicate struct_decl then Error.SDuplicated_field struct_decl |> Error.struct_error |> Result.error
      else Ok ()
    )
end

module Enum = struct
  let is_all_type_exist current_module program (enum_decl: enum_decl) =
    enum_decl.variants 
    |> List.map (fun (_, kts) -> kts) 
    |> List.flatten
    |> List.find_map (fun (kt) -> 
      if Asthelper.Enum.is_type_generic kt enum_decl then None 
      else match Help.is_ktype_exist current_module program kt with
      | Ok () -> None
      | Error e -> Some e
    )

  let is_variant_duplicate enum_decl = 
    enum_decl.variants
    |> List.map (fun (s, _) -> s)
    |> Util.ListHelper.duplicate
    |> Util.are_diff_lenght []



    let is_valid_enum_decl (program: Ast.program) (current_module_name: string) (enum_decl: enum_decl) = 
      let (>>=) = Result.bind in
      is_all_type_exist current_module_name program enum_decl 
      |> (function None -> Ok () | Some e -> Error e)
      >>= (fun () ->     
        if Help.is_cyclic_enum current_module_name program (enum_decl) then Error.ECyclic_Declaration enum_decl |> Error.enum_error |> Result.error
        else (Ok ()) 
      )
      >>= (fun () -> 
        if is_variant_duplicate enum_decl then Error.EDuplicated_variant_name enum_decl |> Error.enum_error |> Result.error
        else Ok ()
      )
end
let validate_module_node (program: Ast.program) (current_module_name: string) (node: Ast.module_node) = 
  match node with
  | NConst _ -> Ok ()
  | NExternFunc external_func_decl -> 
    ExternalFunction.is_valid_external_function_declaration program current_module_name external_func_decl 
  | NSyscall syscall_decl -> Syscall.is_valid_syscall_declaration program current_module_name syscall_decl
  | NStruct struct_decl -> Struct.is_valid_struct_decl program current_module_name struct_decl
  | NEnum enum_decl -> Enum.is_valid_enum_decl program current_module_name enum_decl
  | _ -> failwith "Not implemented yet ..."
  

let validate_module (program: Ast.program) {path; _module = Mod (_module)} = 
  _module
  |> List.fold_left (fun acc value -> if acc |> Result.is_error then acc else validate_module_node program path value) (Ok ())

let valide_program (program: program) = 
  program
  |> List.fold_left (fun acc value -> if acc |> Result.is_error then acc else validate_module program value) (Ok ())
  