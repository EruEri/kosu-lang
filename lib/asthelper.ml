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

let f = sprintf "%Ld"
let rec string_of_ktype = function
| TParametric_identifier {module_path; parametrics_type; name} -> sprintf "(%s) %s %s" (parametrics_type |> List.map string_of_ktype |> String.concat ", ") (module_path) (name)
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

module Statement = struct

  let rec string_of_kbody = function
  | (statements: kstatement list), (expr: kexpression) -> begin 
  sprintf "{ %s\n  %s}"
  (statements |> List.map string_of_kstatement |> String.concat "\n  ")
  (string_of_kexpression expr)
  end
  and string_of_kstatement = function
  | SDeclaration { is_const; variable_name; expression } -> 
    sprintf "%s %s = %s;"
    (if is_const then "const" else "var")
    (variable_name)
    (expression |> string_of_kexpression)
  | SAffection (id, expression) -> sprintf "%s = %s" (id) (expression |> string_of_kexpression)
  | SDiscard (expr) -> sprintf "discard %s" (string_of_kexpression expr)
  and string_of_kexpression = function
  | Empty -> "empty"
  | True -> "true"
  | False -> "false"
  | EInteger (sign, _, value) -> (match sign with Signed -> sprintf "%Ld" value | Unsigned -> sprintf "%Lu" value)
  | EFloat f -> string_of_float f
  | EBin_op bin -> string_of_kbin_op bin
  | EUn_op un -> string_of_kunary_op un
  | ESizeof e -> begin match e with Either.Left t -> string_of_ktype t | Either.Right expr -> string_of_kexpression expr end
  | EString s -> s
  | EAdress x -> sprintf "&%s" x
  | EDeference (indirection, id) -> sprintf "%s%s" (Util.string_of_chars indirection '*') id
  | EIdentifier { modules_path; identifier } | EConst_Identifier {modules_path; identifier}  -> sprintf "%s%s" (Util.string_of_module_path modules_path) identifier
  | EFieldAcces {first_expr; fields} -> sprintf "(%s)->%s" (string_of_kexpression first_expr) (fields |> String.concat "->")
  | EStruct { modules_path; struct_name; fields } -> 
    sprintf "%s%s { %s }" 
    (if modules_path = "" then "" else sprintf "%s::" modules_path) 
    struct_name 
    (fields |> List.map (fun (id, expr) -> sprintf "%s: %s" (id) (string_of_kexpression expr)) |> String.concat ",")
  | EEnum {modules_path; enum_name; variant; assoc_exprs} -> 
    sprintf "%s%s.%s%s"
    (Util.string_of_module_path modules_path)
    (enum_name |> Option.value ~default: " ")
    (variant)
    (if assoc_exprs = [] then "" else sprintf "(%s)" (assoc_exprs |> List.map string_of_kexpression |> String.concat ", "))
  | ETuple exprs -> sprintf "(%s)" (exprs |> List.map string_of_kexpression |> String.concat ", ")
  | EFunction_call {modules_path; generics_resolver; fn_name; parameters} -> 
    sprintf "%s%s%s(%s)"
    (Util.string_of_module_path modules_path)
    (fn_name)
    (generics_resolver |> Option.map (fun kts -> sprintf "::<%s>" (kts |> List.map string_of_ktype |> String.concat ", ")) |> Option.value ~default: "" )
    (parameters |> List.map string_of_kexpression |> String.concat ", ")
  | EIf (expression, if_body, else_body) -> 
    sprintf "if %s %s else %s"
    (string_of_kexpression expression)
    (string_of_kbody if_body)
    (string_of_kbody else_body)  
  | ECases { cases; else_case } -> 
    sprintf "cases {\n %s else => %s}"
    ( cases |> List.map (fun (exprs, kbody) -> sprintf "%s => %s" (exprs |> List.map string_of_kexpression |> String.concat ", ") (string_of_kbody kbody)) |> String.concat "\n")
    (string_of_kbody else_case)
  | ESwitch { expression; cases; wildcard_case } -> 
    sprintf "switch %s {%s\n%s}"
    (string_of_kexpression expression)
    (cases |> List.map (fun (sc, kbody) -> sprintf "%s => %s" (sc |> List.map string_of_switch_case |> String.concat ", ") (string_of_kbody kbody)) |> String.concat "\n")
    (match wildcard_case with None -> "" | Some kbody -> sprintf "_ => %s" (string_of_kbody kbody))
  and string_of_kbin_op = function
  | BAdd (lhs, rhs) -> sprintf "(%s + %s)" (string_of_kexpression lhs) (string_of_kexpression rhs)
  | BMinus (lhs, rhs) -> sprintf "(%s - %s)" (string_of_kexpression lhs) (string_of_kexpression rhs)
  | BMult (lhs, rhs) -> sprintf "(%s * %s)" (string_of_kexpression lhs) (string_of_kexpression rhs)
  | BDiv (lhs, rhs) -> sprintf "(%s / %s)" (string_of_kexpression lhs) (string_of_kexpression rhs)
  | BMod (lhs, rhs) -> sprintf "(%s %% %s)" (string_of_kexpression lhs) (string_of_kexpression rhs)
  | BBitwiseOr (lhs, rhs) -> sprintf "(%s | %s)" (string_of_kexpression lhs) (string_of_kexpression rhs)
  | BBitwiseXor (lhs, rhs) -> sprintf "(%s ^ %s)" (string_of_kexpression lhs) (string_of_kexpression rhs)
  | BBitwiseAnd (lhs, rhs) -> sprintf "(%s & %s)" (string_of_kexpression lhs) (string_of_kexpression rhs)
  | BShiftLeft (lhs, rhs) -> sprintf "(%s << %s)" (string_of_kexpression lhs) (string_of_kexpression rhs)
  | BShiftRight (lhs, rhs) -> sprintf "(%s >> %s)" (string_of_kexpression lhs) (string_of_kexpression rhs)
  | BAnd (lhs, rhs) -> sprintf "(%s && %s)" (string_of_kexpression lhs) (string_of_kexpression rhs)
  | BOr (lhs, rhs) -> sprintf "(%s || %s)" (string_of_kexpression lhs) (string_of_kexpression rhs)
  | BInf (lhs, rhs) -> sprintf "(%s < %s)" (string_of_kexpression lhs) (string_of_kexpression rhs)
  | BInfEq (lhs, rhs) -> sprintf "(%s <= %s)" (string_of_kexpression lhs) (string_of_kexpression rhs)
  | BSup (lhs, rhs) -> sprintf "(%s > %s)" (string_of_kexpression lhs) (string_of_kexpression rhs)
  | BSupEq (lhs, rhs) -> sprintf "(%s >= %s)" (string_of_kexpression lhs) (string_of_kexpression rhs)
  | BEqual (lhs, rhs) -> sprintf "(%s == %s)" (string_of_kexpression lhs) (string_of_kexpression rhs)
  | BDif (lhs, rhs) -> sprintf "(%s != %s)" (string_of_kexpression lhs) (string_of_kexpression rhs)
  and string_of_kunary_op = function
  | UMinus expr -> sprintf "-(%s)" (string_of_kexpression expr)
  | UNot expr -> sprintf "!(%s)" (string_of_kexpression expr)
  and string_of_switch_case = function
  | SC_Identifier s -> s
  | SC_Integer_Literal (sign, size, value) -> string_of_kexpression (EInteger (sign, size, value))
  | SC_Enum_Identifier { variant } -> "."^variant
  | SC_Enum_Identifier_Assoc { variant; assoc_ids } -> 
    sprintf "%s(%s)"
    variant
    (assoc_ids |> List.map (Option.value ~default: "_") |> String.concat "," )
end 
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

  let find_field_type_opt (field: string) (struct_decl: t) = struct_decl.fields |> List.assoc_opt field

  (** 
  @raise Not_found    
  *)
  let find_field_type (field: string) (struct_decl: t) = struct_decl.fields |> List.assoc field

  let rec resolve_single_generics (generic_name: string) (mapped_type: ktype) (expected_type: ktype) =
    match expected_type with
    | TType_Identifier { module_path = _ ; name} -> if name = generic_name then mapped_type else expected_type
    | TParametric_identifier { module_path; parametrics_type; name } -> begin 
      TParametric_identifier { 
        module_path; 
        parametrics_type = parametrics_type |> List.map (resolve_single_generics generic_name mapped_type); 
        name 
      }
    end
    | t -> t
  let rec is_type_generic ktype (struct_decl: t) = 
    match ktype with
    | TType_Identifier { module_path ; name} ->  module_path = "" && struct_decl.generics |> List.mem name
    | TParametric_identifier { module_path = _; parametrics_type ; name = _ } -> parametrics_type |> List.exists ( fun kt -> is_type_generic kt struct_decl)
    | _ -> false
  let is_field_generic_opt field (struct_decl : t) =
    struct_decl.fields |> List.assoc_opt field |> Option.map ( fun kt -> is_type_generic kt struct_decl)

  (**
  @raise Not_found 
  *)
  let is_field_generic field (struct_decl : t) =
    struct_decl.fields |> List.assoc field |> fun kt -> is_type_generic kt struct_decl
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
    | TParametric_identifier {module_path = init_path; parametrics_type = init_pt; name = init_name}, 
      TParametric_identifier {module_path = exp_path; parametrics_type = exp_pt; name = exp_name} -> 
        if init_path <> exp_path || init_name <> exp_name || List.compare_lengths init_pt exp_pt <> 0 then false
        else begin
          List.combine init_pt exp_pt |> List.for_all (fun (i,e) -> is_type_compatible i e struct_decl)
        end
    | TUnknow, _ -> true
    | lhs, rhs -> lhs = rhs
  let rec find_generic_name_from_ktype (ktype: ktype) (struct_decl: t) = 
    match ktype with
    | TType_Identifier { module_path = _ ; name } as tti -> if struct_decl |> is_type_generic tti then Some name else None
    | TParametric_identifier { module_path = _; parametrics_type; name = _ } -> begin 
      parametrics_type |> List.filter_map (fun k -> find_generic_name_from_ktype k struct_decl) |> function [] -> None | t::_ -> Some t
    end
    | _ -> None
  let retrieve_generic_name_from_field_opt (field: string) (struct_decl: t) =
    match struct_decl.fields |> List.assoc_opt field with None -> None | Some kt -> find_generic_name_from_ktype kt struct_decl
  let to_ktype module_def_path (struct_decl: t) = 
    if not (struct_decl |> contains_generics) then TType_Identifier { module_path = module_def_path; name = struct_decl.struct_name }
    else begin 
    TParametric_identifier {
      module_path = module_def_path;
      parametrics_type = struct_decl.generics |> List.map (fun _ -> TUnknow);
      name = struct_decl.struct_name
    }
    end
  let rec is_cyclic_aux ktype_to_test module_def_path (struct_decl: t) =
    match ktype_to_test with
    | TType_Identifier _ as tti -> tti = (struct_decl |> to_ktype module_def_path)
    | TParametric_identifier { module_path; parametrics_type; name } -> (module_path = module_def_path && name = struct_decl.struct_name) || parametrics_type |> List.exists (fun k -> is_cyclic_aux k module_def_path struct_decl)
    | _ -> false
  let is_cyclic module_def_path (struct_decl: t) = struct_decl.fields |> List.exists ( fun (_, ktype) -> is_cyclic_aux ktype module_def_path struct_decl )
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