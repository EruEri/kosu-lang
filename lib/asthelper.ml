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

  let retrieve_type_decl = function
  | Ast.Mod nodes -> nodes |> List.filter_map (fun node -> match node with Ast.NEnum e -> Some (Ast.Type_Decl.decl_enum e) | Ast.NStruct s -> Some (Ast.Type_Decl.decl_struct s) | _ -> None)

  let retrieve_functions_decl = function
  | Ast.Mod nodes -> nodes |> List.filter_map (fun node -> match node with Ast.NExternFunc e -> Some (Ast.Function_Decl.Decl_External e) | Ast.NFunction e -> Some (Ast.Function_Decl.decl_kosu_function e) | _ -> None)

  let type_decl_occurence (type_name: string) (module_definition: Ast._module) = 
    module_definition |> retrieve_type_decl |> Util.Occurence.find_occurence (function
    | Ast.Type_Decl.Decl_Enum e -> e.enum_name = type_name
    | Ast.Type_Decl.Decl_Struct s -> s.struct_name = type_name
    )

  let function_decl_occurence (fn_name: string) (module_definition: Ast._module) = 
    module_definition |> retrieve_functions_decl |> Util.Occurence.find_occurence (function
    | Ast.Function_Decl.Decl_External e -> fn_name = e.sig_name
    | Ast.Function_Decl.Decl_Kosu_Function e -> fn_name = e.fn_name
    )
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

  let find_struct_decl_opt (current_module_name: string) (module_path: string) (struct_name: string) (program: t) = 
    Result.bind (
    (if module_path = "" then Some (program |> module_of_string current_module_name) else program |> module_of_string_opt current_module_name)
    |> Option.map Module.retrieve_struct_decl 
    |> Option.to_result ~none:(Ast.Error.Unbound_Module module_path)
    )
    (fun structs -> 
      structs |> List.find_opt (fun s -> s.struct_name = struct_name) |> Option.to_result ~none:(Ast.Error.Undefined_Struct struct_name)
    )
  
  let find_enum_decl_opt (current_module_name) (module_enum_path) (enum_name_opt: string option) (variant: string) (assoc_exprs: kexpression list) (program: t) =
    let () = Printf.printf "current module : %s\n" current_module_name in
    match (if module_enum_path = "" then Some (program |> module_of_string current_module_name) else program |> module_of_string_opt current_module_name) with
    | None -> (Ast.Error.Unbound_Module module_enum_path) |> Either.left |> Result.error
    | Some _module -> 
      _module 
      |> Module.retrieve_enum_decl
      |> Util.Occurence.find_occurence (fun enum_decl -> 
        match enum_name_opt with
        | None -> enum_decl.variants |> List.exists (fun (variant_decl, assoc_type) -> variant_decl = variant && Util.are_same_lenght assoc_exprs assoc_type)
        | Some enum_name -> enum_name = enum_decl.enum_name && enum_decl.variants |> List.exists (fun (variant_decl, assoc_type) -> variant_decl = variant && Util.are_same_lenght assoc_exprs assoc_type)
        )
      |> function
      | Empty -> Not_found |> Either.right |> Result.error
      | Util.Occurence.Multiple _ -> Util.Occurence.Too_Many_Occurence |> Either.right |> Result.error
      | One enum_decl -> enum_decl |> Result.ok

    let find_module_of_ktype ktype_def_path current_module program = 
      match ktype_def_path with
      | "" -> program |> Util.Occurence.find_occurence (fun module_path -> module_path.path = current_module)
      | s  -> program |> Util.Occurence.find_occurence (fun module_path -> module_path.path = s)

    let find_module_of_function fn_def_path current_module program = 
      match fn_def_path with
      | "" -> program |> Util.Occurence.find_occurence (fun module_path -> module_path.path = current_module)
      | s  -> program |> Util.Occurence.find_occurence (fun module_path -> module_path.path = s)

    (**
    Find type declaration from ktype
    @raise Not_found : if no type declaration was found
    @raise Too_Many_Occurence: if several type declaration matching was found
    *)
    let find_type_decl_from_ktype ktype_def_path ktype_name current_module program = 
      find_module_of_ktype ktype_def_path current_module program
      |> Util.Occurence.one 
      |> fun m -> m._module
      |> Module.type_decl_occurence ktype_name
      |> Util.Occurence.one

    (**
    Find function declaration from function name
    @raise Not_found : if no function declaration was found
    @raise Too_Many_Occurence: if several function declaration matching was found
    *)
    let find_function_decl_from_fn_name fn_def_path fn_name current_module program =
      program 
      |> find_module_of_function fn_def_path current_module
      |> Util.Occurence.one
      |> fun m -> m._module
      |> Module.function_decl_occurence fn_name
      |> Util.Occurence.one

    let rec is_c_type (current_mod_name) (type_decl: Ast.Type_Decl.type_decl) program = 
      match type_decl with
      | Decl_Enum e -> e.generics = [] && e.variants |> List.for_all (fun (_, assoc) -> assoc = [] )
      | Decl_Struct s -> 
        if s.generics = [] then false 
        else s.fields |> List.for_all (fun (_, ktype) -> 
          match ktype with
          | TParametric_identifier _ -> false
          | TType_Identifier { module_path; name } -> begin
            try 
              let type_decl =  (find_type_decl_from_ktype module_path name current_mod_name program) in 
              is_c_type current_mod_name type_decl program
            with _ -> false
          end
          | TFunction _ -> false
          | TTuple _ -> false
          | _ -> true
        )
  
    let is_c_type_from_ktype (current_mod_name) (ktype: ktype) program = 
      match ktype with
      | TParametric_identifier _ -> false
      | TType_Identifier { module_path; name } -> begin
        try 
          let type_decl =  (find_type_decl_from_ktype module_path name current_mod_name program) in 
          is_c_type current_mod_name type_decl program
        with _ -> false
      end
      | TFunction _ -> false
      | TTuple _ -> false
      | _ -> true
      
end

module Statement = struct

  let rec string_of_kbody = function
  | (statements: kstatement list), (expr: kexpression) -> begin 
  sprintf "{\n  %s  %s\n}"
  (statements |> List.map string_of_kstatement |> String.concat "\n  ")
  (string_of_kexpression expr)
  end
  and string_of_kstatement = function
  | SDeclaration { is_const; variable_name; explicit_type ; expression } -> 
    sprintf "%s %s : %s = %s;"
    (if is_const then "const" else "var")
    (variable_name)
    (string_of_ktype explicit_type)
    (expression |> string_of_kexpression)
  | SAffection (id, expression) -> sprintf "%s = %s;" (id) (expression |> string_of_kexpression)
  | SDiscard (expr) -> sprintf "discard %s;" (string_of_kexpression expr)
  and string_of_kexpression = function
  | Empty -> "empty"
  | True -> "true"
  | False -> "false"
  | EInteger (sign, _, value) -> (match sign with Signed -> sprintf "%Ld" value | Unsigned -> sprintf "%Lu" value)
  | EFloat f -> string_of_float f
  | EBin_op bin -> string_of_kbin_op bin
  | EUn_op un -> string_of_kunary_op un
  | ESizeof e -> let s = begin match e with Either.Left t -> string_of_ktype t | Either.Right expr -> string_of_kexpression expr end in
  sprintf "sizeof(%s)" s
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
  | ECases { cases ; else_case } -> 
    sprintf "cases {\n %s else => %s}"
    ( cases |> List.map (fun (expr, kbody) -> sprintf "%s => %s" (expr |> string_of_kexpression) (string_of_kbody kbody)) |> String.concat "\n")
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

  module BinOp = struct
    type t = kbin_op
    let is_boolean = function
    | BAnd _  | BOr _ | BInf _ | BInfEq _ | BSup _ | BSupEq _ | BEqual _ | BDif _ -> true
    | _ -> false

    let is_arithmetic bin = bin |> is_boolean |> not
  end
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

  let rec is_assoc_type_contains_generic (ktype: ktype) (enum_decl: t) = 
    enum_decl.generics |> List.exists (fun g -> match ktype with
    | TType_Identifier { module_path; name } -> module_path = "" && name = g
    | TParametric_identifier { module_path = _ ; parametrics_type; name = _ } -> parametrics_type |> List.exists (fun kt -> is_assoc_type_contains_generic kt enum_decl)
    | _ -> false
    )
  let rec are_type_compatible (init_type: ktype) (expected_type: ktype) (enum_decl: t) = 
    (* let () = Printf.printf "expected : %s :: found : %s\n" (string_of_ktype expected_type) (string_of_ktype init_type) in *)
    match init_type, expected_type with
    | TType_Identifier {module_path = init_path; name = init_name}, TType_Identifier {module_path = exp_path; name = exp_name } -> begin
      enum_decl.generics |> List.mem exp_name || (init_path = exp_path && init_name = exp_name)
    end
    | TParametric_identifier {module_path = init_path; parametrics_type = init_pt; name = init_name}, 
      TParametric_identifier {module_path = exp_path; parametrics_type = exp_pt; name = exp_name} -> 
        if init_path <> exp_path || init_name <> exp_name || List.compare_lengths init_pt exp_pt <> 0 then false
        else begin
          List.combine init_pt exp_pt |> List.for_all (fun (i,e) -> are_type_compatible i e enum_decl)
        end
    | TUnknow, _ -> true
    | _ , TType_Identifier { module_path; name } when module_path = "" && enum_decl.generics |> List.mem name ->  true
    | lhs, rhs -> lhs = rhs

  let rec bind_generic_ktype generic_name new_type expected_type (enum_decl: t) = 
    match expected_type with
    | TType_Identifier { module_path; name } -> if module_path = "" && generic_name = name && enum_decl.generics |> List.mem generic_name then new_type else expected_type
    | TParametric_identifier { module_path; parametrics_type; name } -> TParametric_identifier {
      module_path;
      parametrics_type = parametrics_type |> List.map (fun kt -> bind_generic_ktype generic_name new_type kt enum_decl);
      name
    }
    | _ as t -> t

  let bind_single_generic (generic_name) (new_type) (enum_decl: t) = 
    {
      enum_name = enum_decl.enum_name;
      generics = enum_decl.generics |> List.filter ((<>) generic_name);
      variants = enum_decl.variants |> List.map (fun (variant, assoc_type) -> 
        (variant, 
        assoc_type |> List.map (fun kt -> bind_generic_ktype generic_name new_type kt enum_decl)
        )
      )
    }

  let rec find_generic_name_from_ktype ~generics_result ktype (enum_decl: t) =
    match ktype with
    | TType_Identifier { module_path; name } -> if module_path = "" && enum_decl.generics |> List.mem name then name::generics_result else generics_result
    | TParametric_identifier { module_path = _; parametrics_type; name = _ } -> 
      parametrics_type 
      |> List.map (fun kt -> find_generic_name_from_ktype ~generics_result kt enum_decl)
      |> List.concat
      |> List.fold_left (fun acc value -> if acc |> List.mem value then acc else value::acc) []
    | _ -> generics_result

  (**
    Returns the generic name for the associated type of an variant at a given position
    Returns [None] if [variant_name] not in the list of enum variant or [assoc_position > lenght of variant assoc type] or type found isn't generic
  *)
  let find_generic_name_from_positon ~assoc_position (variant_name: string) (enum_decl: t) = 
    enum_decl.variants
    |> List.find_map (fun (v, assoc_types) -> if v = variant_name then Some assoc_types else None)
    |> function
    | None -> None
    | Some assoc_types -> assoc_position |> List.nth_opt assoc_types
    |> function
    | None -> None
    | Some kt -> Some (find_generic_name_from_ktype ~generics_result:[] kt enum_decl) 
  
  let rec infer_generics ~assoc_position (variant_zipped: (ktype * ktype) list) (generics: (ktype * bool) list) enum_decl = 
    match variant_zipped with
    | [] -> generics
    | (init_ktype, expected_ktype)::q -> 
      if is_assoc_type_contains_generic expected_ktype enum_decl then 
        if are_type_compatible init_ktype expected_ktype enum_decl then
        enum_decl
        |> infer_generics ~assoc_position:(assoc_position + 1) q ( generics |> List.map (fun (kt, true_type) -> 
          match kt with
          | TType_Identifier { module_path = _; name } -> if enum_decl.generics |> List.mem name && not true_type
            then (bind_generic_ktype name init_ktype expected_ktype enum_decl, true) 
            else (kt, true_type)
          | _ -> raise (Failure "Generics cannot have a different type than TIdentifier" )
          ))
        else failwith ""
      else
      infer_generics ~assoc_position:(assoc_position + 1) q generics enum_decl

  let is_valide_assoc_type_init ~init_types ~expected_types enum_decl = 
    Util.are_same_lenght init_types expected_types 
    && (List.combine init_types expected_types |> List.for_all (fun (init_type, expected_type) -> are_type_compatible init_type expected_type enum_decl))

  let is_simple_enum (enum_decl: t) = 
    enum_decl.variants |> List.for_all (fun (_, assoc) -> assoc = [])
  let is_tagged_union (enum_decl: t) = 
    enum_decl.variants |> List.exists (fun (_, assoc_type) -> assoc_type <> [] )
  let contains_generics (enum_decl: t) = enum_decl.generics <> []
end

module Struct = struct
  type t = struct_decl

  let find_struct_decl_from_name (current_module_name: string) (prog : program) (module_path: string) (struct_name: string) = 
    let structs_opt = (if module_path = "" then Some (prog |> Program.module_of_string current_module_name) else prog |> Program.module_of_string_opt current_module_name)
    |> Option.map Module.retrieve_struct_decl in
    
    match structs_opt with
    | None -> Error (Ast.Error.Unbound_Module module_path)
    | Some structs ->
      structs |> List.find_opt (fun s -> s.struct_name = struct_name) |> Option.to_result ~none:(Ast.Error.Undefined_Struct struct_name)
  ;;

  let string_of_struct_decl (struct_decl: t) = 
    sprintf "struct (%s) %s := { %s }" 
    (struct_decl.generics |> String.concat ", ")
    (struct_decl.struct_name)
    (struct_decl.fields |> List.map (fun (field, t) -> sprintf "%s : %s" (field) (string_of_ktype t)) |> String.concat ", " )
  let contains_generics (struct_decl: t) = struct_decl.generics <> []

  (**
  @raise Type_Error: 
  @raise Not_found: raised if field or struct declaration not found
  *)
  let rec resolve_fields_access (current_mod_name: string) (program: program) (fields: string list) (ktype: ktype) = 
    match fields with
    | [] -> failwith "Not supposed to be reached"
    | t::[] -> begin 
      match ktype with
      | TType_Identifier { module_path; name } -> (
        match find_struct_decl_from_name current_mod_name program module_path name with
        | Error e -> e |> Error.ast_error |> raise
        | Ok _struct -> _struct.fields |> List.assoc t
        )
      | _ -> raise (Ast.Error.ast_error (Impossible_field_Access ktype))
    end
    | t::q -> begin 
      match ktype with
      | TType_Identifier { module_path; name } ->  begin
        match find_struct_decl_from_name current_mod_name program module_path name with
        | Error e -> e |> Error.ast_error |> raise
        | Ok _struct -> resolve_fields_access current_mod_name program q (_struct.fields |> List.assoc t)
      end
      | _ -> raise (Ast.Error.ast_error (Impossible_field_Access ktype))
    end

  
  

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
  let is_cyclic module_def_path (struct_decl: t) = 
    struct_decl.fields |> List.exists ( fun (_, ktype) -> is_cyclic_aux ktype module_def_path struct_decl )
  
  (**
  @raise Failure: if type in last parameters aren't TType_Identfier
  *)
  let rec to_ktype_help_aux zip_new_old_fields old_struct_decl (generics: (ktype * bool) list) = 
    match zip_new_old_fields with
    | [] -> generics
    | ((_, new_type), (old_field, old_type))::q -> 
      match old_struct_decl |> retrieve_generic_name_from_field_opt old_field with
      | None -> to_ktype_help_aux q old_struct_decl generics
      | Some generic_name -> to_ktype_help_aux q old_struct_decl (generics |> List.map (fun (ktype, true_type) ->
        match ktype with
        | TType_Identifier { module_path = _; name } ->
          if not (Ast.Type.are_compatible_type old_type new_type) then (Ast.Error.Uncompatible_type { expected = old_type; found = new_type } |> Ast.Error.ast_error |> raise) 
          else
          if name = generic_name then (new_type, true) else (ktype, true_type)
        | _ -> raise (Failure "Generics cannot have a different type than TIdentifier" )
        )
      )

  let to_ktype_help module_def_path ~new_struct_decl old_struct_decl = 
    if old_struct_decl |> contains_generics |> not then TType_Identifier { module_path = module_def_path; name = old_struct_decl.struct_name }
    else TParametric_identifier {
      module_path = module_def_path;
      parametrics_type = 
      (to_ktype_help_aux 
        (List.combine new_struct_decl.fields old_struct_decl.fields) 
        old_struct_decl 
        (old_struct_decl.generics |> List.map (fun kt -> (TType_Identifier { module_path = ""; name = kt }, false)))) 
      |> List.map (fun (kt, true_type) -> if true_type then kt else TUnknow);
      name = old_struct_decl.struct_name
    } 
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

module Function = struct
  type t = function_decl

  let string_of_func_decl (function_decl: t) = 
    sprintf "fn %s%s(%s)%s%s"
    (function_decl.fn_name)
    (if function_decl.generics = [] then "" else sprintf "<%s>" (function_decl.generics |> String.concat ", "))
    (function_decl.parameters |> List.map (fun (id, ktype) -> sprintf "%s: %s" (id) (string_of_ktype ktype)) |> String.concat ", ")
    (function_decl.return_type |> string_of_ktype)
    (function_decl.body |> Statement.string_of_kbody)

  let rec is_ktype_generic ktype (fn_decl: t) = 
    match ktype with
    | TParametric_identifier { module_path = _; parametrics_type ; name = _ } -> parametrics_type |> List.exists (fun kt -> is_ktype_generic kt fn_decl)
    | TType_Identifier { module_path = _; name } -> fn_decl.generics |> List.mem name
    | _ -> false

  (**
    @return true if parameter contains generics, false if not and None if paramater name doesn't exist
  *)
  let is_parameter_generic (para_name: string) (fn_decl: t) = 
    fn_decl.parameters
    |> List.assoc_opt para_name
    |> Option.map ( fun kt -> is_ktype_generic kt fn_decl)

  let is_no_nested_generic ktype (fn_decl: t) = 
    match ktype with
    | TType_Identifier { module_path = _; name } -> fn_decl.generics |> List.mem name
    | _ -> false

  let rec are_ktypes_compatible ~para_type ~init_type (fn_decl: t) =
    if is_no_nested_generic para_type fn_decl then true 
    else match para_type, init_type with 
    | TType_Identifier {module_path = l_module_path; name = lname}, TType_Identifier {module_path = r_module_path; name = r_name} -> l_module_path = r_module_path && lname = r_name
    | TParametric_identifier { module_path = lmp; parametrics_type = lpt ; name = ln }, TParametric_identifier { module_path = rmp ; parametrics_type = rpt; name = rn } -> 
      rn = ln && lmp = rmp && (Util.are_same_lenght lpt rpt) && (List.combine lpt rpt |> List.for_all (fun (l, r) -> are_ktypes_compatible ~para_type: l ~init_type: r fn_decl))
    | lhs, rhs -> lhs = rhs


  let iter_statement fn (function_decl: t) = let statements, _  = function_decl.body in statements |> List.iter fn
end

let string_of_found_expected = function
| `int(expected, found ) -> Printf.sprintf "-- expected : %d, found : %d --" expected found
| `str(expected, found) -> Printf.sprintf "-- expected : %s, found : %s --" expected found
| `ktype(expected, found ) -> Printf.sprintf "-- expected : %s, found : %s --" (expected |> string_of_ktype) (found  |> string_of_ktype)

let string_of_struct_error = let open Ast.Error in let open Printf in function
| Unexpected_field { expected; found } -> sprintf "Unexpected_field -- expected : %s, found : %s --" expected found
| Unexisting_field s -> sprintf "Unexisting_field %s" s
| Wrong_field_count record -> sprintf "Wrong_field_count -- expected : %d, found : %d" record.expected record.found

let string_of_enum_error = let open Ast.Error in let open Printf in function
| Wrong_length_assoc_type record -> sprintf "Wrong_length_assoc_type -- expected : %d, found : %d --" record.expected record.found
| Uncompatible_type_in_variant { variant_name } -> sprintf "Uncompatible_type_in_variant : %s" variant_name

let string_of_statement_error = let open Ast.Error in let open Printf in function
| Undefine_Identifier s -> sprintf "Undefine_Identifier : %s" s.name
| Already_Define_Identifier s -> sprintf "Already_Define_Identifier : %s" s.name
| Reassign_Constante s -> sprintf "Reassign_Constante : %s" s.name
| Uncompatible_type_Assign s -> sprintf "Uncompatible_type_Assign -- expected: %s, found: %s --" (s.expected |> string_of_ktype)  (s.found |> string_of_ktype) 

let string_of_function_error = let open Ast.Error in let open Printf in function
| Unmatched_Parameters_length record -> sprintf "Unmatched_Parameters_length %s " (string_of_found_expected (`int(record.expected, record.found)))
| Unmatched_Generics_Resolver_length record -> sprintf "Unmatched_Generics_Resolver_length : %s" (string_of_found_expected (`int(record.expected, record.expected)) )
| Uncompatible_type_for_C_Function recod -> sprintf "Uncompatible_type_for_C_Function for %s " (ExternalFunc.string_of_external_func_decl recod.external_func_decl)
| Mismatched_Parameters_Type record -> sprintf "Mismatched_Parameters_Type : %s" (string_of_found_expected (`ktype(record.expected, record.found)))
| Unknow_Function_Error -> "Unknow_Function_Error"
let string_of_ast_error = let open Ast.Error in let open Printf in function
| Bin_operator_Different_type -> "Bin_operator_Different_type"
| Undefined_Identifier s -> sprintf "Undefined_Identifier : %s" s
| Undefined_Const s -> sprintf "Undefined_Const : %s" s
| Undefined_Struct s -> sprintf "Undefined_Struct : %s" s
| Unbound_Module s -> sprintf "Unbound_Module : %s" s
| Struct_Error s -> string_of_struct_error s
| Enum_Error e -> string_of_enum_error e
| Statement_Error e -> string_of_statement_error e
| Func_Error e -> string_of_function_error e
| Uncompatible_type e -> sprintf "Uncompatible_type %s" (string_of_found_expected (`ktype(e.expected, e.found)))
| Uncompatible_type_If_Else e -> sprintf "Uncompatible_type_If_Else %s" (string_of_found_expected (`ktype(e.if_type, e.else_type)))
| Not_Boolean_Type_Condition e -> sprintf "Uncompatible_type_If_Else : %s" (string_of_ktype e.found)
| Impossible_field_Access e -> sprintf "Impossible_field_Access : %s" (string_of_ktype e)
| Unvalid_Deference -> sprintf "Unvalid_Deference"