open Ast;;
open Printf;;

let char_of_signedness = function
| Signed -> 's'
| Unsigned -> 'u'
;;

let string_of_isize = function
| I8 -> "8"
| I16 -> "16"
| I32 -> "32"
| I64 -> "64"

let rec string_of_ktype = function
| TParametric_identifier {module_path; parametrics_type; type_name} -> sprintf "(%s) %s %s" (parametrics_type |> List.map string_of_ktype |> String.concat ", ") (module_path) (type_name)
| TType_Identifier {module_path; name} -> sprintf "%s%s" (if module_path = "" then "" else sprintf "%s::" module_path) name
| TInteger (sign, size) -> sprintf "%c%s" (char_of_signedness sign) (string_of_isize size)
| TPointer ktype -> sprintf "*%s" (string_of_ktype ktype)
| TTuple ktypes -> sprintf "(%s)" (ktypes |> List.map string_of_ktype |> String.concat ", ")
| TFunction (parameters, r_type) -> sprintf "(%s) -> %s" (parameters |> List.map string_of_ktype |> String.concat ", ") (string_of_ktype r_type)
| TString_lit -> "stringl"
| TBool -> "bool"
| TUnit -> "unit"
| TUnknow -> "unknow"
| TFloat -> "f64"


module Enum = struct
  type t = enum_decl

  let string_of_enum_variant (variant: (string * Ast.ktype list)) = 
    let name, types = variant in
    sprintf "%s (%s)" name (types |> List.map string_of_ktype |> String.concat ", ")

    let string_of_enum_decl (enum_decl: t) = 
      sprintf "enum (%s) %s := { %s }"
      (enum_decl.generics |> String.concat ", ")
      (enum_decl.enum_name)
      (enum_decl.variants |> List.map string_of_enum_variant |> String.concat ", ")

  let is_simple_enum (enum_decl: t) = 
    enum_decl.variants |> List.for_all (fun (_, assoc) -> assoc = [])
  let is_tagged_union (enum_decl: t) = 
    enum_decl.variants |> List.exists (fun (_, assoc_type) -> assoc_type <> [] )
  let contains_generics (enum_decl: t) = enum_decl.generics = []
end

module Struct = struct
  type t = struct_decl

  let string_of_struct_decl (struct_decl: t) = 
    sprintf "struct (%s) %s := { %s }" 
    (struct_decl.generics |> String.concat ", ")
    (struct_decl.struct_name)
    (struct_decl.fields |> List.map (fun (field, t) -> sprintf "%s : %s" (field) (string_of_ktype t)) |> String.concat ", " )
  let contains_generics (struct_decl: t) = struct_decl.generics = []

  let rec resolve_single_generics (generic_name: string) (mapped_type: ktype) (expected_type: ktype) =
    match expected_type with
    | TType_Identifier { module_path = _ ; name} -> if name = generic_name then mapped_type else expected_type
    | TParametric_identifier { module_path; parametrics_type; type_name } -> begin 
      TParametric_identifier { 
        module_path; 
        parametrics_type = parametrics_type |> List.map (resolve_single_generics generic_name mapped_type); 
        type_name 
      }
    end
    | t -> t
  let rec is_type_generic ktype (struct_decl: t) = 
    match ktype with
    | TType_Identifier { module_path = _; name} -> struct_decl.generics |> List.mem name
    | TParametric_identifier { module_path = _; parametrics_type ; type_name = _ } -> parametrics_type |> List.exists ( fun kt -> is_type_generic kt struct_decl)
    | _ -> false
  let is_field_generic field (struct_decl : t) =
    struct_decl.fields |> List.assoc_opt field |> Option.map ( fun kt -> is_type_generic kt struct_decl)
  let bind_generic (generic_name) (new_type: ktype) (struct_decl: t) = 
    {
      struct_name = struct_decl.struct_name;
      generics = struct_decl.generics |> List.filter ( (<>) generic_name ) ;
      fields = struct_decl.fields |> List.map ( fun (f,t) -> (f, resolve_single_generics generic_name new_type t))    ;
    }
  let rec is_type_compatible (init_type: ktype) (expected_type: ktype) (struct_decl: t) = 
    match init_type, expected_type with
    | TType_Identifier {module_path = init_path; name = init_name}, TType_Identifier {module_path = exp_path; name = exp_name } -> begin
      struct_decl.generics |> List.mem exp_name || (init_path = exp_path && init_name = exp_name)
    end
    | TParametric_identifier {module_path = init_path; parametrics_type = init_pt; type_name = init_name}, 
      TParametric_identifier {module_path = exp_path; parametrics_type = exp_pt; type_name = exp_name} -> 
        if init_path <> exp_path || init_name <> exp_name || List.compare_lengths init_pt exp_pt <> 0 then false
        else begin
          List.combine init_pt exp_pt |> List.for_all (fun (i,e) -> is_type_compatible i e struct_decl)
        end
    | lhs, rhs -> lhs = rhs
end

module ExternalFunc = struct 
  type t = external_func_decl

  let string_of_external_func_decl (efucn_decl: t) = 
    sprintf "external %s(%s%s) %s %s"
    (efucn_decl.sig_name)
    (efucn_decl.fn_parameters |> List.map string_of_ktype |> String.concat ",")
    (if efucn_decl.is_variadic then ";..." else "")
    (efucn_decl.r_type |> string_of_ktype)
    (efucn_decl.c_name |> Option.map (fun s -> sprintf " = %s" s) |> Option.value ~default: "")
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
    |> function [] -> None | t::_ -> Some t
  let module_of_string mod_name (program: t) = 
    program 
    |> List.filter_map ( fun (mp: Ast.module_path) -> if mod_name = mp.path then Some mp._module else None) 
    |> List.hd
end