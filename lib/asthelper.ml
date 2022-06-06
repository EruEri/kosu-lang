module Enum = struct
  include Astpp.PPEnum
  type t = Astpp.PPEnum.t

  let is_simple_enum (enum_decl: t) = 
    enum_decl.variants |> List.for_all (fun (_, assoc) -> assoc = [])
  let is_tagged_union (enum_decl: t) = 
    enum_decl.variants |> List.exists (fun (_, assoc_type) -> assoc_type <> [] )
end


module Program = struct

  let retrieve_enum_decl = function
  | Ast.Prog nodes -> nodes |> List.filter_map (fun node -> match node with Ast.NEnum e -> Some e | _ -> None)

  let retrieve_struct_decl = function
  | Ast.Prog nodes -> nodes |> List.filter_map (fun node -> match node with Ast.NStruct s -> Some s | _ -> None)

  let retrieve_external_func_decl = function
  | Ast.Prog nodes -> nodes |> List.filter_map (fun node -> match node with Ast.NExternFunc s -> Some s | _ -> None)
end