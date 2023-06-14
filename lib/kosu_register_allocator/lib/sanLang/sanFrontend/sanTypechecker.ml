(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of San: A 3 address code language/compiler                               *)
(* Copyright (C) 2023 Yves Ndiaye                                                             *)
(*                                                                                            *)
(* San is free software: you can redistribute it and/or modify it under the terms             *)
(* of the GNU General Public License as published by the Free Software Foundation,            *)
(* either version 3 of the License, or (at your option) any later version.                    *)
(*                                                                                            *)
(* San is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;           *)
(* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR           *)
(* PURPOSE.  See the GNU General Public License for more details.                             *)
(* You should have received a copy of the GNU General Public License along with San.          *)
(* If not, see <http://www.gnu.org/licenses/>.                                                *)
(*                                                                                            *)
(**********************************************************************************************)

open SanAst
open SanError

(**
  @raise Undefinded_Variable if the variable is not in [env]    
*)
let rec typeof_atom san_module env atom_loc =
  match atom_loc.SanPosition.value with
  | String _ -> Stringl
  | Int _ -> Ssize
  | Boolean _ -> Boolean
  | Variable v -> (
      match SanEnv.typeof v env with
      | Some t -> t
      | None ->
          raise @@ san_error
          @@ Undefinded_Variable (atom_loc |> SanPosition.map (fun _ -> v)))

and typeof_rvalue san_module env rvalue_loc =
  let error_location = SanPosition.unit_located rvalue_loc in
  match rvalue_loc.SanPosition.value with
  | RVExpr e -> typeof_atom san_module env e
  | RVDiscard t | RVLater t -> t.value
  | RVUnary record -> (
      let type_atom = typeof_atom san_module env record.atom in
      match SanHelper.does_support_unop record.unop type_atom with
      | true -> type_atom
      | false ->
          raise @@ san_error
          @@ UnaryOperator_not_suppored
               { error_location; unop = record.unop; for_type = type_atom })
  | RVBinary { binop = TacSelf self_op as binop; blhs; brhs } -> (
      let ltype = typeof_atom san_module env blhs in
      let rtype = typeof_atom san_module env brhs in
      let () =
        if ltype <> rtype then
          raise @@ san_error
          @@ BinOp_diff_type
               {
                 error_location = rvalue_loc |> SanPosition.map (fun _ -> ());
                 lhs = ltype;
                 rhs = rtype;
               }
      in
      match SanHelper.does_support_arithmetic_operator self_op ltype with
      | true -> ltype
      | false ->
          raise @@ san_error
          @@ BinaryOperator_not_suppored
               { error_location; binop; for_type = ltype })
  | RVBinary { binop = TacBool self_bool as binop; blhs; brhs } -> (
      let ltype = typeof_atom san_module env blhs in
      let rtype = typeof_atom san_module env brhs in
      let () =
        if ltype <> rtype then
          raise @@ san_error
          @@ BinOp_diff_type
               {
                 error_location = rvalue_loc |> SanPosition.map (fun _ -> ());
                 lhs = ltype;
                 rhs = rtype;
               }
      in
      match SanHelper.does_support_logicial_operator self_bool rtype with
      | true -> Boolean
      | false ->
          raise @@ san_error
          @@ BinaryOperator_not_suppored
               { error_location; binop; for_type = rtype })
  | RVFunctionCall { fn_name; parameters } ->
      let signature =
        match SanHelper.find_sig_opt san_module fn_name.SanPosition.value with
        | Some signature -> signature
        | None -> raise @@ san_error @@ Undefined_Function fn_name
      in
      let typed_parameters =
        parameters
        |> List.map (SanPosition.map_use (typeof_atom san_module env))
      in
      let typed_parameters_len = List.length typed_parameters in
      let sig_param_len = List.length @@ fst @@ signature in
      let () =
        if typed_parameters_len <> sig_param_len then
          raise @@ san_error
          @@ Function_Wrong_args_number
               {
                 error_location;
                 fn_name = fn_name.value;
                 expected = sig_param_len;
                 found = typed_parameters_len;
               }
      in

      let () =
        signature |> fst
        |> List.combine typed_parameters
        |> List.iter (fun (typed_par, expected_type) ->
               let open SanPosition in
               if typed_par.value <> expected_type.value then
                 raise @@ san_error
                 @@ Incompatible_type
                      { expected = expected_type.value; found = typed_par })
      in

      (snd signature).value

and type_check_block ~return_type san_module env
    ({ label = _; statements; ending } as block) =
  match statements with
  | stmt :: q -> (
      match stmt with
      | SSDeclaration (variable, san_rvalue) -> (
          match SanEnv.exists variable.value env with
          | true -> raise @@ san_error @@ Already_define_variable variable
          | false ->
              let typeof = typeof_rvalue san_module env san_rvalue in
              let extended_env = SanEnv.add (variable.value, typeof) env in
              type_check_block ~return_type san_module extended_env
                { block with statements = q }))
  | [] -> (
      match ending with
      | None -> env
      | Some (SE_return atom) ->
          let atom_type = typeof_atom san_module env atom in
          let () =
            if atom_type <> return_type then
              let error_location = SanPosition.unit_located atom in
              raise @@ san_error
              @@ Wrong_return_type
                   { error_location; expected = return_type; found = atom_type }
          in
          env
      | Some (SE_If { expr; _ }) ->
          let atom_type = typeof_atom san_module env expr in
          let () =
            if atom_type <> Boolean then
              let error_location = SanPosition.unit_located expr in
              raise @@ san_error
              @@ If_Not_boolean_type
                   { error_location; expected = return_type; found = atom_type }
          in
          env)

and type_check_function san_module san_function =
  let open SanPosition in
  let base_env =
    san_function.parameters
    |> List.map (fun (p, t) -> (p.value, t.value))
    |> SanEnv.of_list
  in
  let _ =
    san_function.san_basic_blocks
    |> List.fold_left
         (type_check_block ~return_type:san_function.return_type.value
            san_module)
         base_env
  in
  ()
