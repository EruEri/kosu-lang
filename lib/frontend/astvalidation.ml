open Ast



module Error = struct

  type external_func_error = 
  | Unit_parameter of external_func_decl
  | Not_C_compatible_type of external_func_decl * ktype
  | Too_much_parameters of { limit: int; found: int}

  type syscall_error =
  | Syscall_Unit_parameter of syscall_decl
  | Syscall_Not_C_compatible_type of syscall_decl * ktype
  | Syscall_Too_much_parameters of { limit: int; found: int} 

  type validation_error = 
  | External_Func_Error of external_func_error
  | Syscall_Error of syscall_error
end

module ExternalFunction = struct
  let does_parameters_contains_unit (external_func_decl: external_func_decl) = 
    if external_func_decl.fn_parameters |> List.mem TUnit then Error.Unit_parameter external_func_decl |> Result.error else Ok ()
  
  let does_contains_not_c_compatible_type program current_module (external_func_decl: external_func_decl) = 
    external_func_decl.fn_parameters 
    |> List.filter ( fun kt -> 
      Asthelper.Program.is_c_type_from_ktype current_module kt program |> not
    )
    |> function  [] -> Ok () | t::_ -> Error.Not_C_compatible_type (external_func_decl, t) |> Result.error
  
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
    if syscall_decl.parameters |> List.mem TUnit then Error.Syscall_Unit_parameter syscall_decl |> Result.error else Ok ()
  
  let does_contains_not_c_compatible_type program current_module (syscall_decl: syscall_decl) = 
    syscall_decl.parameters 
    |> List.filter ( fun kt -> 
      Asthelper.Program.is_c_type_from_ktype current_module kt program |> not
    )
    |> function  [] -> Ok () | t::_ -> Error.Syscall_Not_C_compatible_type (syscall_decl, t) |> Result.error
  
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
let validate_module_node (program: Ast.program) (current_module_name: string) (node: Ast.module_node) = 
  match node with
  | NConst _ -> Ok ()
  | NExternFunc external_func_decl -> 
    ExternalFunction.is_valid_external_function_declaration program current_module_name external_func_decl |> Result.map_error (fun e -> Error.External_Func_Error e)
  | NSyscall syscall_decl -> Syscall.is_valid_syscall_declaration program current_module_name syscall_decl |> Result.map_error (fun e -> Error.Syscall_Error e)
  | _ -> failwith ""
  

let validate_module (program: Ast.program) {path; _module: Ast._module} = failwith ""

let valide_program (_program: program) = failwith ""
  