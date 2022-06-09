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
| TParametric_identifier (name, parameters) -> sprintf "(%s) %s" (parameters |> List.map string_of_ktype |> String.concat ", ") (name)
| TType_Identifier s -> s
| TInteger (sign, size) -> sprintf "%c%s" (char_of_signedness sign) (string_of_isize size)
| TPointer ktype -> sprintf "*%s" (string_of_ktype ktype)
| TTuple ktypes -> sprintf "(%s)" (ktypes |> List.map string_of_ktype |> String.concat ", ")
| TFunction (parameters, r_type) -> sprintf "(%s) -> %s" (parameters |> List.map string_of_ktype |> String.concat ", ") (string_of_ktype r_type)
| TBool -> "bool"
| TUnit -> "unit"
| TUnknow -> "unknow"
| TFloat -> "float"


module PPEnum = struct
  type t = enum_decl

  let string_of_enum_variant (variant: (string * Ast.ktype list)) = 
    let name, types = variant in
    sprintf "%s (%s)" name (types |> List.map string_of_ktype |> String.concat ", ")

    let string_of_enum_decl (enum_decl: t) = 
      sprintf "enum (%s) %s := { %s }"
      (enum_decl.generics |> String.concat ", ")
      (enum_decl.enum_name)
      (enum_decl.variants |> List.map string_of_enum_variant |> String.concat ", ")
end

module PPStruct = struct
  type t = struct_decl

  let string_of_struct_decl (struct_decl: t) = 
    sprintf "struct (%s) %s := { %s }" 
    (struct_decl.generics |> String.concat ", ")
    (struct_decl.struct_name)
    (struct_decl.fields |> List.map (fun (field, t) -> sprintf "%s : %s" (field) (string_of_ktype t)) |> String.concat ", " )
end

module PPExternalFunc = struct
  type t = external_func_decl

  let string_of_external_func_decl (efucn_decl: t) = 
    sprintf "external %s(%s %s) %s %s"
    (efucn_decl.sig_name)
    (efucn_decl.fn_parameters |> List.map string_of_ktype |> String.concat ",")
    (if efucn_decl.is_variadic then ";..." else "")
    (efucn_decl.r_type |> string_of_ktype)
    (efucn_decl.c_name |> Option.map (fun s -> sprintf " = %s" s) |> Option.value ~default: "")
end