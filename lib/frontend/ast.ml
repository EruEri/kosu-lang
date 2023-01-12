(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of Kosu                                                                  *)
(* Copyright (C) 2022 Yves Ndiaye                                                             *)
(*                                                                                            *)
(* Kosu is free software: you can redistribute it and/or modify it under the terms            *)
(* of the GNU General Public License as published by the Free Software Foundation,            *)
(* either version 3 of the License, or (at your option) any later version.                    *)
(*                                                                                            *)
(* Kosu is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;          *)
(* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR           *)
(* PURPOSE.  See the GNU General Public License for more details.                             *)
(* You should have received a copy of the GNU General Public License along with Kosu.         *)
(* If not, see <http://www.gnu.org/licenses/>.                                                *)
(*                                                                                            *)
(**********************************************************************************************)

open Position

type signedness = Signed | Unsigned
type isize = I8 | I16 | I32 | I64

type switch_case =
  | SC_Enum_Identifier of { variant : string location }
  | SC_Enum_Identifier_Assoc of {
      variant : string location;
      assoc_ids : string location option list;
    }

type parser_unary_op = PNot | PUMinus

type parser_binary_op =
  | Add
  | Minus
  | Mult
  | Div
  | Modulo
  | BitwiseOr
  | BitwiseAnd
  | BitwiseXor
  | ShiftLeft
  | ShiftRight
  | Sup
  | Inf
  | Equal

type ktype =
  | TParametric_identifier of {
      module_path : string location;
      parametrics_type : ktype location list;
      name : string location;
    }
  | TType_Identifier of {
      module_path : string location;
      name : string location;
    }
  | TInteger of (signedness * isize)
  | TPointer of ktype location
  | TTuple of ktype location list
  | TFunction of ktype location list * ktype location
  | TString_lit
  | TUnknow
  | TFloat
  | TBool
  | TUnit

type kbody = kstatement location list * kexpression location

and kstatement =
  | SDeclaration of {
      is_const : bool;
      variable_name : string location;
      explicit_type : ktype location option;
      expression : kexpression location;
    }
  | SAffection of string location * kexpression location
  | SDiscard of kexpression location
  | SDerefAffectation of string location * kexpression location

and kexpression =
  | Empty
  | True
  | False
  | ENullptr
  | EInteger of (signedness * isize * int64)
  | EFloat of float
  | ESizeof of (ktype location, kexpression location) Either.t
  | EString of string
  | EAdress of string location
  | EDeference of int * string location
  | EWhile of kexpression location * kbody
  | ELambda of {
    params: (string location * (ktype location option)) list;
    kbody: kbody
  }
  | EIdentifier of {
      modules_path : string location;
      identifier : string location;
    }
  | EFieldAcces of {
      first_expr : kexpression location;
      field : string location;
    }
  | EConst_Identifier of {
      modules_path : string location;
      identifier : string location;
    }
  | EStruct of {
      modules_path : string location;
      struct_name : string location;
      fields : (string location * kexpression location) list;
    }
  | EEnum of {
      modules_path : string location;
      enum_name : string location option;
      variant : string location;
      assoc_exprs : kexpression location list;
    }
  | ETuple of kexpression location list
  | EBuiltin_Function_call of {
      fn_name : string location;
      parameters : kexpression location list;
    }
  | EFunction_call of {
      modules_path : string location;
      generics_resolver : ktype location list option;
      fn_name : string location;
      parameters : kexpression location list;
    }
  | EIf of kexpression location * kbody * kbody
  | ECases of { cases : (kexpression location * kbody) list; else_case : kbody }
  | ESwitch of {
      expression : kexpression location;
      cases : (switch_case list * kbody) list;
      wildcard_case : kbody option;
    }
  | EBin_op of kbin_op
  | EUn_op of kunary_op

and kbin_op =
  | BAdd of kexpression location * kexpression location
  | BMinus of kexpression location * kexpression location
  | BMult of kexpression location * kexpression location
  | BDiv of kexpression location * kexpression location
  | BMod of kexpression location * kexpression location
  | BBitwiseOr of kexpression location * kexpression location
  | BBitwiseAnd of kexpression location * kexpression location
  | BBitwiseXor of kexpression location * kexpression location
  | BShiftLeft of kexpression location * kexpression location
  | BShiftRight of kexpression location * kexpression location
  | BAnd of kexpression location * kexpression location
  | BOr of kexpression location * kexpression location
  | BSup of kexpression location * kexpression location
  | BSupEq of kexpression location * kexpression location
  | BInf of kexpression location * kexpression location
  | BInfEq of kexpression location * kexpression location
  | BEqual of kexpression location * kexpression location
  | BDif of kexpression location * kexpression location

and kunary_op = UMinus of kexpression location | UNot of kexpression location

type struct_decl = {
  struct_name : string location;
  generics : string location list;
  fields : (string location * ktype location) list;
}

type enum_decl = {
  enum_name : string location;
  generics : string location list;
  variants : (string location * ktype location list) list;
}

type function_decl = {
  fn_name : string location;
  generics : string location list;
  parameters : (string location * ktype location) list;
  return_type : ktype location;
  body : kbody;
}

type operator_decl =
  | Unary of {
      op : parser_unary_op location;
      field : string location * ktype location;
      return_type : ktype location;
      kbody : kbody;
    }
  | Binary of {
      op : parser_binary_op location;
      fields :
        (string location * ktype location) * (string location * ktype location);
      return_type : ktype location;
      kbody : kbody;
    }

type syscall_decl = {
  syscall_name : string location;
  parameters : ktype location list;
  return_type : ktype location;
  opcode : int64 location;
}

type external_func_decl = {
  sig_name : string location;
  fn_parameters : ktype location list;
  r_type : ktype location;
  is_variadic : bool;
  c_name : string option;
}

type const_decl = {
  const_name : string location;
  explicit_type : ktype;
  value : kexpression location;
}

type sig_decl = {
  sig_name : string;
  generics : string list;
  parameters : ktype list;
  return_type : ktype;
}

type module_node =
  | NExternFunc of external_func_decl
  | NFunction of function_decl
  | NOperator of operator_decl
  | NSyscall of syscall_decl
  | NStruct of struct_decl
  | NEnum of enum_decl
  | NConst of const_decl

type _module = Mod of module_node list
type module_path = { path : string; _module : _module }
type named_module_path = { filename : string; module_path : module_path }
type program = named_module_path list

module Isize = struct
  let size_of_isize = function I8 -> 8 | I16 -> 16 | I32 -> 32 | I64 -> 64
end

module Type_Decl = struct
  type type_decl = Decl_Enum of enum_decl | Decl_Struct of struct_decl

  let decl_enum e = Decl_Enum e
  let decl_struct s = Decl_Struct s
end

module Function_Decl = struct
  type t =
    | Decl_External of external_func_decl
    | Decl_Kosu_Function of function_decl
    | Decl_Syscall of syscall_decl

  let decl_external e = Decl_External e
  let decl_kosu_function e = Decl_Kosu_Function e
  let decl_syscall e = Decl_Syscall e
  let is_external = function Decl_External _ -> true | _ -> false
  let is_syscall = function Decl_Syscall _ -> true | _ -> false
  let is_kosu_func = function Decl_Kosu_Function _ -> true | _ -> false

  let calling_name = function
    | Decl_External { sig_name = name; _ }
    | Decl_Kosu_Function { fn_name = name; _ }
    | Decl_Syscall { syscall_name = name; _ } ->
        name

  let parameters = function
    | Decl_External { fn_parameters = parameters; _ }
    | Decl_Syscall { parameters; _ } ->
        parameters
    | Decl_Kosu_Function { parameters; _ } -> parameters |> List.map snd

  let return_type = function
    | Decl_External { r_type = return_type; _ }
    | Decl_Kosu_Function { return_type; _ }
    | Decl_Syscall { return_type; _ } ->
        return_type
end

module OperatorFunction = struct
  type operator =
    | Add
    | Minus
    | Mult
    | Div
    | Modulo
    | BitwiseOr
    | BitwiseAnd
    | BitwiseXor
    | ShiftLeft
    | ShiftRight
    | And
    | Or
    | Sup
    | SupEq
    | Inf
    | InfEq
    | Equal
    | Diff
    | Not
    | UMinus

  let name_of_operator = function
    | Add -> "add"
    | Minus -> "minus"
    | Mult -> "mult"
    | Div -> "div"
    | Modulo -> "modulo"
    | BitwiseAnd -> "bitwiseand"
    | BitwiseOr -> "bitwiseor"
    | BitwiseXor -> "bitwisexor"
    | ShiftLeft -> "shiftleft"
    | ShiftRight -> "shiftright"
    | And -> "and"
    | Or -> "or"
    | Sup -> "sup"
    | SupEq -> "supeq"
    | Inf -> "inf"
    | InfEq -> "infeq"
    | Equal -> "equal"
    | Diff -> "diff"
    | UMinus -> "uminus"
    | Not -> "not"

  let symbole_of_operator = function
    | Add -> "+"
    | Minus -> "-"
    | Mult -> "*"
    | Div -> "/"
    | Modulo -> "%"
    | BitwiseAnd -> "&"
    | BitwiseOr -> "|"
    | BitwiseXor -> "^"
    | ShiftLeft -> "<<"
    | ShiftRight -> ">>"
    | And -> "&&"
    | Or -> "||"
    | Sup -> ">"
    | SupEq -> ">="
    | Inf -> "<"
    | InfEq -> "<="
    | Equal -> "=="
    | Diff -> "!="
    | Not -> "!"
    | UMinus -> "(-)"
end

module Error = struct
  type struct_error =
    | Unexpected_field of {
        expected : string location;
        found : string location;
      }
    | Wrong_field_count of {
        struct_name : string location;
        expected : int;
        found : int;
      }

  type enum_error =
    | Wrong_length_assoc_type of {
        variant : string location;
        expected : int;
        found : int;
      }
    | No_variant_found_for_enum of { variant : string location }
    | Conflict_variant_multiple_decl of {
        module_path : string;
        variant : string location;
        enum_decls : enum_decl list;
      }

  type statement_error =
    | Undefine_Identifier of { name : string location }
    | Already_Define_Identifier of { name : string location }
    | Reassign_Constante of { name : string location }
    | Uncompatible_type_Assign of { expected : ktype; found : ktype location }
    | Dereference_No_pointer of { name : string location; ktype : ktype }
    | Dereference_Wrong_type of {
        identifier : string location;
        expected : ktype;
        found : ktype location;
      }
    | Need_explicit_type_declaration of {
        variable_name : string location;
        infer_type : ktype;
      }

  type func_error =
    | Unmatched_Parameters_length of {
        fn_name : string location;
        expected : int;
        found : int;
      }
    | Unmatched_Generics_Resolver_length of {
        fn_name : string location;
        expected : int;
        found : int;
      }
    | Uncompatible_type_for_C_Function of {
        fn_name : string location;
        ktype : ktype location;
      }
    | Uncompatible_type_for_Syscall of {
        index : int option;
        syscall_decl : syscall_decl;
      }
    | Mismatched_Parameters_Type of {
        fn_name : string;
        expected : ktype;
        found : ktype location;
      }
    | Unknow_Function_Error

  type operator_error =
    | Invalid_pointer_arithmetic of ktype location
    | No_built_in_op of {
        bin_op : OperatorFunction.operator;
        ktype : ktype location;
      }
    | Incompatible_Type of {
        bin_op : OperatorFunction.operator;
        expr_loc : kexpression location;
        lhs : ktype location;
        rhs : ktype location;
      }
    | Operator_not_found of {
        bin_op : OperatorFunction.operator;
        ktype : ktype location;
      }
    | Too_many_operator_declaration of {
        operator_decls : (string * operator_decl list) list;
        bin_op : OperatorFunction.operator;
        ktype : ktype location;
      }
    | Not_Boolean_operand_in_And of ktype location
    | Not_Boolean_operand_in_Or of ktype location
    | Invalid_Uminus_for_Unsigned_integer of isize location

  type builtin_func_error =
    | Unknow_built_function of string location
    | Wrong_parameters of {
        fn_name : string;
        expected : ktype;
        found : ktype location;
      }
    | Mismatched_Parameters_Length of {
        fn_name : string location;
        expected : int;
        found : int;
      }
    | Found_no_Integer of { fn_name : string; found : ktype location }

  type switch_error =
    | Not_enum_type_in_switch_Expression of ktype location
    | Not_fully_known_ktype of ktype location
    | Not_all_cases_handled of {
        expression_loc : kexpression location;
        missing_variant : string location * ktype location list;
      }
    | Duplicated_case of string location
    | Variant_not_found of { enum_decl : enum_decl; variant : string location }
    | Mismatched_Assoc_length of {
        variant : string location;
        expected : int;
        found : int;
      }
    | Incompatible_Binding_Name of {
        switch_expr : kexpression location;
        base_variant : string location;
        base_bound_id : string location;
        wrong_variant : string location;
        wrong_bound_id : string location;
      }
    | Incompatible_Binding_Ktype of {
        switch_expr : kexpression location;
        base_variant : string location;
        base_bound_id : string location;
        base_bound_ktype : ktype location;
        wrong_variant : string location;
        wrong_bound_id : string location;
        wrong_bound_ktype : ktype location;
      }
    | Incompatible_Binding_Position of {
        base_index : int;
        base_variant : string location;
        base_bound_id : string location;
        wrong_index : int;
        wrong_variant : string location;
        wrong_bound_id : string location;
      }
    | Identifier_already_Bound of string location

  type ast_error =
    | Wrong_Assoc_Length_for_Parametrics of {
        type_name : string location;
        expected : int;
        found : int;
      }
    | Undefined_Identifier of string location
    | Undefined_Const of string location
    | Undefined_Struct of string location
    | Unbound_Module of string location
    | Undefine_Type of string location
    | Undefine_function of string location
    | Struct_Error of struct_error
    | Enum_Error of enum_error
    | Statement_Error of statement_error
    | Func_Error of func_error
    | Operator_Error of operator_error
    | Switch_error of switch_error
    | Builtin_Func_Error of builtin_func_error
    | Uncompatible_type of { expected : ktype; found : ktype location }
    | Uncompatible_type_If_Else of {
        position : unit location;
        if_type : ktype;
        else_type : ktype;
      }
    | Not_unit_type_while of { position : unit location; wrong_type : ktype }
    | Not_Boolean_Type_Condition of { found : ktype location }
    | Impossible_field_Access of {
        field : string location;
        struct_decl : struct_decl;
      }
    | Enum_Access_field of { field : string location; enum_decl : enum_decl }
    | Unvalid_Deference of string location
    | Conflicting_type_declaration of {
        path : string;
        ktype_name : string location;
        type_decls : Type_Decl.type_decl list;
      }
    | Conflicting_module_path_declaration of {
        module_path : string location;
        choices : module_path list;
      }
    | Conflicting_function_declaration of {
        fn_def_path : string;
        fn_name : string location;
        fn_decls : Function_Decl.t list;
      }

  exception Ast_error of ast_error

  let ast_error e = Ast_error e
  let struct_error e = ast_error (Struct_Error e)
  let enum_error e = ast_error (Enum_Error e)
  let stmt_error e = ast_error (Statement_Error e)
  let func_error e = ast_error (Func_Error e)
  let operator_error e = ast_error (Operator_Error e)
  let switch_error e = ast_error (Switch_error e)
  let built_in_func_error e = ast_error (Builtin_Func_Error e)
end

module Type = struct
  let ktuple kts = TTuple kts
  let pointer kt = TPointer kt
  let is_any_ptr = function TPointer _ -> true | _ -> false

  let is_unknown_ptr = function
    | TPointer { v = TUnknow; _ } -> true
    | _ -> false

  let is_any_integer = function TInteger _ -> true | _ -> false
  let is_string_litteral = function TString_lit -> true | _ -> false

  let pointee_fail = function
    | TPointer kt -> kt.v
    | _ -> failwith "Ktype is not a pointer"

  let rec is_builtin_type = function
    | TParametric_identifier _ | TType_Identifier _ -> false
    | TTuple kts -> kts |> List.for_all (fun kt -> is_builtin_type kt.v)
    | _ -> true

  let is_parametric = function TParametric_identifier _ -> true | _ -> false

  let rec set_module_path generics new_module_name = function
    | TType_Identifier { module_path = { v = ""; position }; name }
      when generics |> List.map Position.value |> List.mem name.v |> not ->
        TType_Identifier
          { module_path = { v = new_module_name; position }; name }
    | TParametric_identifier { module_path; parametrics_type; name } ->
        TParametric_identifier
          {
            module_path =
              (if module_path.v = "" then
               { v = new_module_name; position = module_path.position }
              else module_path);
            parametrics_type =
              parametrics_type
              |> List.map
                   (Position.map (set_module_path generics new_module_name));
            name;
          }
    | TPointer t ->
        t |> Position.map (set_module_path generics new_module_name) |> pointer
    | TTuple kts ->
        TTuple
          (kts
          |> List.map (fun kt ->
                 {
                   v = set_module_path generics new_module_name kt.v;
                   position = kt.position;
                 }))
    | _ as kt -> kt

  let rec is_type_full_known ktype =
    match ktype with
    | TUnknow -> false
    | TParametric_identifier
        { module_path = _; parametrics_type = kts; name = _ }
    | TTuple kts ->
        kts |> List.for_all (fun kt -> is_type_full_known kt.v)
    | TPointer kt -> is_type_full_known kt.v
    | _ -> true

  let extract_parametrics_ktype ktype =
    match ktype with
    | TParametric_identifier { module_path = _; parametrics_type; name = _ } ->
        parametrics_type
    | _ -> []

  let type_name_opt = function
    | TType_Identifier { module_path = _; name }
    | TParametric_identifier { module_path = _; parametrics_type = _; name } ->
        Some name
    | _ -> None

  let module_path_opt = function
    | TType_Identifier { module_path; name = _ }
    | TParametric_identifier { module_path; parametrics_type = _; name = _ } ->
        Some module_path
    | _ -> None

  let rec ( === ) lhs rhs =
    match (lhs, rhs) with
    | ( TParametric_identifier
          { module_path = mp1; parametrics_type = pt1; name = n1 },
        TParametric_identifier
          { module_path = mp2; parametrics_type = pt2; name = n2 } ) ->
        n1.v = n2.v && mp1.v = mp2.v
        && pt1 |> Util.are_same_lenght pt2
        && List.for_all2 (fun kt1 kt2 -> kt1.v === kt2.v) pt1 pt2
    | TPointer pt1, TPointer pt2 -> pt1.v === pt2.v
    | ( TType_Identifier { module_path = mp1; name = n1 },
        TType_Identifier { module_path = mp2; name = n2 } ) ->
        mp1.v = mp2.v && n1.v = n2.v
    | TTuple t1, TTuple t2 ->
        Util.are_same_lenght t1 t2
        && List.combine t1 t2
           |> List.map Position.assocs_value
           |> List.for_all (fun (k1, k2) -> k1 === k2)
    | _, _ -> lhs = rhs

  let ( !== ) lhs rhs = lhs === rhs |> not

  let rec extract_mapped_ktype generics ktype =
    match ktype with
    | TType_Identifier { module_path = { v = ""; _ }; name } -> (
        try
          let _, kt = Hashtbl.find generics name.v in
          kt
        with _ -> ktype)
    | TParametric_identifier { module_path; parametrics_type; name } ->
        TParametric_identifier
          {
            module_path;
            parametrics_type =
              parametrics_type
              |> List.map (Position.map (extract_mapped_ktype generics));
            name;
          }
    | TTuple kts ->
        kts |> List.map (Position.map (extract_mapped_ktype generics)) |> ktuple
    | _ -> ktype

  let rec are_compatible_type (lhs : ktype) (rhs : ktype) =
    match (lhs, rhs) with
    | ( TParametric_identifier
          { module_path = mp1; parametrics_type = pt1; name = n1 },
        TParametric_identifier
          { module_path = mp2; parametrics_type = pt2; name = n2 } ) ->
        n1.v = n2.v && mp1.v = mp2.v
        && pt1 |> Util.are_same_lenght pt2
        && List.for_all2
             (fun kt1 kt2 -> are_compatible_type kt1.v kt2.v)
             pt1 pt2
    | TUnknow, _ | _, TUnknow -> true
    | TPointer _, TPointer { v = TUnknow; _ } -> true
    | TPointer { v = TUnknow; _ }, TPointer _ -> true
    | TPointer pt1, TPointer pt2 -> are_compatible_type pt1.v pt2.v
    | ( TType_Identifier { module_path = mp1; name = n1 },
        TType_Identifier { module_path = mp2; name = n2 } ) ->
        mp1.v = mp2.v && n1.v = n2.v
    | TTuple t1, TTuple t2 ->
        Util.are_same_lenght t1 t2
        && List.combine t1 t2
           |> List.map Position.assocs_value
           |> List.for_all (fun (k1, k2) -> are_compatible_type k1 k2)
    | TFunction (plhs, lr) , TFunction (prhs, rr) when Util.are_same_lenght plhs prhs -> 
      are_compatible_type lr.v rr.v &&
      List.for_all2 (fun pl pr -> are_compatible_type pl.v pr.v) plhs prhs
    | _, _ -> lhs === rhs

  let rec update_generics map init_type param_type () =
    match (init_type.v, param_type.v) with
    | kt, TType_Identifier { module_path = { v = ""; _ }; name } -> (
        match Hashtbl.find_opt map name.v with
        | Some t -> (
            match t with
            | index, TUnknow ->
                let () = Hashtbl.replace map name.v ((index : int), kt) in
                ()
            | _ as _t -> ())
        | None -> ())
    | ( TParametric_identifier
          { module_path = lmp; parametrics_type = lpt; name = lname },
        TParametric_identifier
          { module_path = rmp; parametrics_type = rpt; name = rname } ) ->
        if lmp.v <> rmp.v || lname.v <> rname.v || Util.are_diff_lenght lpt rpt
        then ()
        else List.iter2 (fun l r -> update_generics map l r ()) lpt rpt
    | TPointer lhs, TPointer rhs -> update_generics map lhs rhs ()
    | TTuple lhs, TTuple rhs ->
        List.iter2 (fun l r -> update_generics map l r ()) lhs rhs
    | _ -> ()

  let equal_fields =
    List.for_all2 (fun ((lfield : string location), lktype) (rfield, rtype) ->
        lfield.v = rfield.v && lktype.v === rtype.v)

  let find_diff_field l1 l2 =
    l2 |> List.combine l1
    |> List.find_opt (fun ((lfield, lktype), (rfield, rtype)) ->
           not (lfield.v = rfield.v && lktype.v === rtype.v))

  let find_field_error l1 l2 =
    l2 |> List.combine l1
    |> List.find_map (fun ((lindex, lfield, lktype), (rindex, rfield, rtype)) ->
           if lfield.v <> rfield.v then
             Some (`diff_binding_name ((lfield, lktype), (rfield, rtype)))
           else if lktype.v |> ( === ) rtype.v |> not then
             Some (`diff_binding_ktype ((lfield, lktype), (rfield, rtype)))
           else if lindex <> rindex then
             Some
               (`diff_binding_index
                 (((lindex : int), lfield), (rindex, rfield)))
           else None)

  let rec module_path_return_type ~(current_module : string)
      ~(module_type_path : string) return_type =
    match return_type with
    | TType_Identifier { module_path; name } ->
        TType_Identifier
          {
            module_path =
              {
                v =
                  (if module_path.v = "" then current_module else module_path.v);
                position = module_path.position;
              };
            name;
          }
    | TParametric_identifier { module_path; parametrics_type; name } ->
        TParametric_identifier
          {
            module_path =
              {
                module_path with
                v =
                  (if module_path.v = "" then current_module else module_path.v);
              };
            parametrics_type =
              parametrics_type
              |> List.map
                   (Position.map
                      (module_path_return_type ~current_module ~module_type_path));
            name;
          }
    | TTuple kts ->
        TTuple
          (kts
          |> List.map
               (Position.map
                  (module_path_return_type ~current_module ~module_type_path)))
    | TPointer kt ->
        kt
        |> Position.map
             (module_path_return_type ~current_module ~module_type_path)
        |> pointer
    | _ as kt -> kt

  let rec remap_naif_generic_ktype generics_table ktype =
    match ktype with
    | TType_Identifier { module_path = { v = ""; position = kposition }; name }
      -> (
        match Hashtbl.find_opt generics_table name.v with
        | None ->
            TType_Identifier
              { module_path = { v = ""; position = kposition }; name }
        | Some typ -> typ)
    | TType_Identifier { module_path; name } -> (
        match Hashtbl.find_opt generics_table name.v with
        | None -> TType_Identifier { module_path; name }
        | Some typ -> typ)
    | TParametric_identifier { module_path; parametrics_type; name } ->
        TParametric_identifier
          {
            module_path;
            parametrics_type =
              parametrics_type
              |> List.map
                   (Position.map (remap_naif_generic_ktype generics_table));
            name;
          }
    | TTuple kts ->
        TTuple
          (kts
          |> List.map (Position.map (remap_naif_generic_ktype generics_table)))
    | TPointer kt ->
        kt |> Position.map (remap_naif_generic_ktype generics_table) |> pointer
    | _ as kt -> kt

  let rec remap_generic_ktype ~current_module generics_table ktype =
    match ktype with
    | TType_Identifier { module_path = { v = ""; position = kposition }; name }
      -> (
        match Hashtbl.find_opt generics_table name.v with
        | None ->
            TType_Identifier
              { module_path = { v = ""; position = kposition }; name }
        | Some (_, typ) -> typ)
    | TType_Identifier { module_path; name } -> (
        match Hashtbl.find_opt generics_table name.v with
        | None ->
            TType_Identifier
              {
                module_path =
                  module_path
                  |> Position.map (fun smodule_path ->
                         if current_module = smodule_path then ""
                         else smodule_path);
                name;
              }
        | Some (_, typ) -> typ)
    | TParametric_identifier { module_path; parametrics_type; name } ->
        TParametric_identifier
          {
            module_path;
            parametrics_type =
              parametrics_type
              |> List.map
                   (Position.map
                      (remap_generic_ktype ~current_module generics_table));
            name;
          }
    | TTuple kts ->
        TTuple
          (kts
          |> List.map
               (Position.map
                  (remap_generic_ktype ~current_module generics_table)))
    | TPointer kt ->
        kt
        |> Position.map (remap_generic_ktype ~current_module generics_table)
        |> pointer
    | _ as kt -> kt

  let rec map_generics_type (generics_combined : (string location * ktype) list)
      (primitive_generics : string location list) ktype =
    match ktype with
    (* | TType_Identifier {module_path = { v = ""; _ }; name} when primitive_generics |> List.map Position.value |> List.mem name.v -> ktype *)
    | TType_Identifier { module_path = { v = ""; _ }; name } ->
        generics_combined
        |> List.map Position.assoc_value_left
        |> List.assoc_opt name.v
        |> Option.value ~default:ktype
    | TParametric_identifier { module_path; parametrics_type; name } ->
        TParametric_identifier
          {
            module_path;
            parametrics_type =
              parametrics_type
              |> List.map
                   (Position.map
                      (map_generics_type generics_combined primitive_generics));
            name;
          }
    | TTuple kts ->
        TTuple
          (kts
          |> List.map
               (Position.map
                  (map_generics_type generics_combined primitive_generics)))
    | TPointer kt ->
        kt
        |> Position.map (map_generics_type generics_combined primitive_generics)
        |> pointer
    | _ -> ktype

  (**
  Returns the restricted version of the left type.
  this function returns the left type if [not (are_compatible_type to_restrict_type restrict_type)]
  *)
  let rec restrict_type (to_restrict_type : ktype) (restricted_type : ktype) =
    if not (are_compatible_type to_restrict_type restricted_type) then
      to_restrict_type
    else
      match (to_restrict_type, restricted_type) with
      | ( TParametric_identifier
            { module_path = mp1; parametrics_type = pt1; name = n1 },
          TParametric_identifier
            { module_path = mp2; parametrics_type = pt2; name = n2 } ) ->
          if n1.v <> n2.v || mp1.v <> mp2.v || Util.are_diff_lenght pt1 pt2 then
            to_restrict_type
          else
            TParametric_identifier
              {
                module_path = mp1;
                parametrics_type =
                  List.combine pt1 pt2
                  |> List.map (fun (lhs, rhs) ->
                         {
                           v = restrict_type lhs.v rhs.v;
                           position = lhs.position;
                         });
                (*(fun (lhs, rhs) -> restrict_type lhs.v rhs.v)*)
                name = n1;
              }
      | TUnknow, t -> t
      | (TPointer _ as kt), TPointer { v = TUnknow; _ } -> kt
      | _, _ -> to_restrict_type
end

module Builtin_Function = struct
  type functions =
    | Tos8
    | Tou8
    | Tos16
    | Tou16
    | Tos32
    | Tou32
    | Tos64
    | Tou64
    | Stringl_ptr

  let isize_of_functions = function
    | Tos8 | Tou8 -> I8
    | Tos16 | Tou16 -> I16
    | Tos32 | Tou32 -> I32
    | Tos64 | Tou64 | Stringl_ptr -> I64
end

module Env = struct
  type variable_info = { is_const : bool; ktype : ktype }
  type t = { contexts : (string * variable_info) list list }

  let empty = { contexts = [] }
  let vi_ktype { ktype; _ } = ktype
  let vi_is_const { is_const; _ } = is_const
  let create_empty_env : t = { contexts = [] }
  let create_env first_context = { contexts = [ first_context ] }
  let flat_context env = env.contexts |> List.flatten

  let push_context (context : (string * variable_info) list) (env : t) =
    { contexts = context :: env.contexts }

  let push_empty_context env = { contexts = [] :: env.contexts }

  let find_identifier_opt (identifier : string) (env : t) =
    env |> flat_context |> List.assoc_opt identifier

  let is_identifier_exists identifier (env : t) =
    env |> flat_context |> List.assoc_opt identifier |> Option.is_some

  let rec restrict_variable_type_context identifier ktype
      (context : (string * variable_info) list) =
    match context with
    | [] -> []
    | t :: q ->
        let ctx_id, ctx_variable_info = t in
        if ctx_id <> identifier then
          t :: restrict_variable_type_context identifier ktype q
        else
          let { is_const = ctx_is_const; ktype = ctx_ktype } =
            ctx_variable_info
          in
          let new_variable_info =
            {
              is_const = ctx_is_const;
              ktype = Type.restrict_type ctx_ktype ktype;
            }
          in
          (ctx_id, new_variable_info) :: q

  let restrict_variable_type identifier ktype (env : t) =
    {
      contexts =
        env.contexts
        |> List.map (restrict_variable_type_context identifier ktype);
    }

  let add_variable couple (env : t) =
    match env.contexts with
    | [] -> env |> push_context [ couple ]
    | t :: q -> { contexts = (couple :: t) :: q }

  let add_fn_parameters ~const (name, ktype) (env : t) =
    let is_const = const in
    let variable_info = { is_const; ktype } in
    env |> add_variable (name, variable_info)

  let pop_context env =
    match env.contexts with [] -> env | _ :: q -> { contexts = q }
end
