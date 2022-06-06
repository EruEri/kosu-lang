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
      sprintf "(%s) %s := { %s }"
      (enum_decl.generics |> String.concat ", ")
      (enum_decl.enum_name)
      (enum_decl.variants |> List.map string_of_enum_variant |> String.concat ", ")
end


