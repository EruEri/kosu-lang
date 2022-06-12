module Enum = struct
  include Astpp.PPEnum
  type t = Astpp.PPEnum.t

  let is_simple_enum (enum_decl: t) = 
    enum_decl.variants |> List.for_all (fun (_, assoc) -> assoc = [])
  let is_tagged_union (enum_decl: t) = 
    enum_decl.variants |> List.exists (fun (_, assoc_type) -> assoc_type <> [] )
  let contains_generics (enum_decl: t) = enum_decl.generics = []
end

module Struct = struct
  include Astpp.PPStruct
  type t = Astpp.PPStruct.t
  let contains_generics (struct_decl: t) = struct_decl.generics = []

end


module Module = struct

  let retrieve_enum_decl = function
  | Ast.Mod nodes -> nodes |> List.filter_map (fun node -> match node with Ast.NEnum e -> Some e | _ -> None)

  let retrieve_struct_decl = function
  | Ast.Mod nodes -> nodes |> List.filter_map (fun node -> match node with Ast.NStruct s -> Some s | _ -> None)

  let retrieve_external_func_decl = function
  | Ast.Mod nodes -> nodes |> List.filter_map (fun node -> match node with Ast.NExternFunc s -> Some s | _ -> None)

  let retrieve_func_decl = function
  | Ast.Mod nodes -> nodes |> List.filter_map (fun node -> match node with Ast.NFunction s -> Some s | _ -> None)

  let retrieve_const_decl = function
  | Ast.Mod nodes -> nodes |> List.filter_map (fun node -> match node with Ast.NConst s -> Some s | _ -> None)

  let retrieve_sig_decl = function
  | Ast.Mod nodes -> nodes |> List.filter_map (fun node -> match node with Ast.NSigFun s -> Some s | _ -> None)
end

module Program = struct
  type t = Ast.program

  let module_of_string_opt mod_name (program: t) = 
    program 
    |> List.filter_map ( fun (mp: Ast.module_path) -> if mod_name = mp.path then Some mp._module else None) 
    |> (fun l -> match l with [] -> None | t::_ -> Some t)
  let module_of_string mod_name (program: t) = 
    program 
    |> List.filter_map ( fun (mp: Ast.module_path) -> if mod_name = mp.path then Some mp._module else None) 
    |> List.hd
end