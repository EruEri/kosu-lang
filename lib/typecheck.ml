open Ast


module Env = struct
  type t = {
    contexts : ((string * ktype) list) list
  }

  let flat_context env = env.contexts |> List.flatten
end

type struct_error = 
| Unexpected_field of { expected: string ; found : string }
| Wrong_field_count of { expected: int ; found : int }

type type_error = 
  | Bin_operator_Different_type
  | Undefined_Identifier of string
  | Undefined_Const of string
  | Undefined_Struct of string
  | Unbound_Module of string
  | Struct_Error of struct_error
  | Unvalid_Deference

let rec typeof_expected (env : Env.t) (current_mod_name: string) (prog : program) (expression: kexpression) = 
  match expression with
  | Empty -> Ok TUnit
  | True | False -> Ok TBool
  | EUn_op ( UMinus e | UNot e ) -> typeof_expected env current_mod_name prog e
  | EBin_op bin_op -> begin 
    match bin_op with
    | BAdd (lhs, rhs) | BMinus (lhs, rhs) | BMult (lhs, rhs) | BDiv (lhs, rhs) 
    | BMod (lhs, rhs) | BBitwiseOr (lhs, rhs) | BBitwiseAnd (lhs, rhs) | BBitwiseXor (lhs, rhs)
    | BShiftLeft (lhs, rhs) | BShiftRight (lhs, rhs) 
      -> if lhs = rhs then typeof_expected env current_mod_name prog lhs else Error Bin_operator_Different_type
    | BOr (lhs, rhs) | BAnd (lhs, rhs) | BEqual (lhs, rhs) | BDif (lhs, rhs) | BSup (lhs, rhs) 
    | BSupEq (lhs, rhs) | BInf (lhs, rhs) | BInfEq (lhs, rhs) -> if lhs = rhs then Ok TBool else Error Bin_operator_Different_type
  end
  | EInteger (sign, size, _ )-> Ok (TInteger (sign, size) ) 
  | EFloat _ -> Ok TFloat
  | ESizeof _ -> Ok (TInteger (Unsigned, I64))
  | EString _ -> Ok TString_lit
  | EDeference (indirection_count, id) -> begin
    let rec loop count ktype = 
      match count with
      | 0 ->  Ok ktype
      | s -> match ktype with | Ast.TPointer t -> loop (s-1) t | _ -> Error Unvalid_Deference
    in
    match env |> Env.flat_context |> List.assoc_opt id with
    | None -> Error (Undefined_Identifier id)
    | Some t -> loop indirection_count t

  end
  | EAdress s -> begin 
    let flat_env = Env.flat_context env in
    match flat_env |> List.assoc_opt s with
    | None -> Error (Undefined_Identifier s)
    | Some t -> Ok ( TPointer t )
  end
  | EConst_Identifier { modules_path ; identifier } -> begin
    let consts_opt = (if modules_path = "" then Some (prog |> Asthelper.Program.module_of_string current_mod_name) else prog |> Asthelper.Program.module_of_string_opt current_mod_name)
    |> Option.map Asthelper.Module.retrieve_const_decl in 

    match consts_opt with
    | None -> Error (Unbound_Module modules_path)
    | Some consts -> 
      consts 
      |> List.find_map (fun c -> if c.const_name = identifier then Some c.explicit_type else None)
      |> Option.to_result ~none: (Unbound_Module modules_path)
    end
  | EIdentifier { modules_path = _; identifier } ->
    env |> Env.flat_context |> List.assoc_opt identifier |> Option.to_result ~none: (Undefined_Identifier identifier)
  | ETuple exprs -> begin 
    let mapped = exprs |> List.map (typeof_expected env current_mod_name prog) in
    match mapped |> List.find_opt Result.is_error with
    | Some e -> e
    | None -> Ok (TTuple (mapped |> List.map Result.get_ok))
  end
  | EStruct { modules_path; struct_name; fields } -> begin
    let structs_opt = (if modules_path = "" then Some (prog |> Asthelper.Program.module_of_string current_mod_name) else prog |> Asthelper.Program.module_of_string_opt current_mod_name)
    |> Option.map Asthelper.Module.retrieve_struct_decl in
    
    match structs_opt with
    | None -> Error (Unbound_Module modules_path)
    | Some structs -> begin
      match structs |> List.find_opt (fun s -> s.struct_name = struct_name) with
      | None -> Error (Undefined_Struct struct_name)
      | Some _struct -> (
        _struct |> validate_struct_initialisation env current_mod_name prog fields 
      )
    end
  end
  (* | EStruct { modules_path; struct_name; fields } -> begin

  end *)
  | _ -> failwith ""
and validate_struct_initialisation env (current_mod_name: string) program (fields: (string * Ast.kexpression) list) struct_decl =         
  if struct_decl |> Asthelper.Struct.contains_generics  then Ok( TType_Identifier struct_decl.struct_name )
  else begin
    let parameters_lenght = fields |> List.length in
    let expected_lenght = struct_decl.fields |> List.length in
    if parameters_lenght <> expected_lenght
      then Error ( Struct_Error (Wrong_field_count { expected = expected_lenght; found = parameters_lenght }))
    else
      let zipped = List.combine fields struct_decl.fields in
      let _ = zipped |> List.map (fun ( (init_field, init_expr), (decl_field, decl_type)) -> 
        if init_field <> decl_field then Error (Struct_Error (Unexpected_field { expected = decl_field ; found = init_field }))
        else
          match typeof_expected env current_mod_name program init_expr  with
          | Error e -> Error e
          | Ok t -> begin
            match (t, decl_type) with
            |  _ -> failwith ""
          end
        ) in
        failwith ""
end