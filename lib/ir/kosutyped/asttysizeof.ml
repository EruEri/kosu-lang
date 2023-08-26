(*****************************************************************************************)
(*                                                                                       *)
(* This file is part of Kosu                                                             *)
(* Copyright (C) 2023 Yves Ndiaye                                                        *)
(*                                                                                       *)
(* Kosu is free software: you can redistribute it and/or modify it under the terms       *)
(* of the GNU General Public License as published by the Free Software Foundation,       *)
(* either version 3 of the License, or (at your option) any later version.               *)
(*                                                                                       *)
(* Kosu is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;     *)
(* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR      *)
(* PURPOSE.  See the GNU General Public License for more details.                        *)
(* You should have received a copy of the GNU General Public License along with Kosu.    *)
(* If not, see <http://www.gnu.org/licenses/>.                                           *)
(*                                                                                       *)
(*****************************************************************************************)

open Asttyped
open Asttyhelper
open KosuFrontend.Ast

let mapsize : (rktype, int64) Hashtbl.t = Hashtbl.create 17
let ( ++ ) = Int64.add
let ( -- ) = Int64.sub

let align n b =
  let m = Int64.unsigned_rem n b in
  if m = 0L then
    n
  else
    n ++ b -- m

let align_16 b = align b 16L
let align_8 b = align b 8L
let align_4 b = align b 4L

let rec size calcul program rktype =
  match rktype with
  | RTUnit | RTBool | RTUnknow | RTChar | RTOrdered ->
      1L
  | RTInteger (_, isize) ->
      Isize.size_of_isize isize / 8 |> Int64.of_int
  | RTFloat fsize ->
      Fsize.size_of_fsize fsize / 8 |> Int64.of_int
  | RTPointer _ | RTString_lit | RTFunction _ | RTOpaque _ ->
      8L
  | RTTuple kts ->
      size_tuple calcul program kts
  | RTArray { size = nb_elt; rktype } ->
      let sizeof = Int64.mul nb_elt @@ size calcul program rktype in
      let min = match calcul with `align -> 1L | `size -> 0L in
      if nb_elt = 0L then
        min
      else
        sizeof
  | (RTParametric_identifier _ | RTType_Identifier _) as kt -> (
      (* let () = Printf.printf "%s\n%!" (Asttypprint.string_of_rktype kt) in *)
      let type_decl =
        Option.get @@ RProgram.find_type_decl_from_rktye kt program
      in

      match type_decl with
      | Rtype_Decl.RDecl_Enum enum_decl ->
          size_enum calcul program
            (kt |> RType.extract_parametrics_rktype
            |> List.combine enum_decl.generics
            |> List.to_seq |> Hashtbl.of_seq
            )
            enum_decl
      | Rtype_Decl.RDecl_Struct struct_decl ->
          size_struct calcul program
            (kt |> RType.extract_parametrics_rktype
            |> List.combine struct_decl.generics
            |> List.to_seq |> Hashtbl.of_seq
            )
            struct_decl
    )

and size_tuple calcul program = function
  | list -> (
      let size, alignment, _packed_size =
        list
        |> List.fold_left
             (fun (acc_size, acc_align, _acc_packed_size) kt ->
               let comming_size = kt |> size `size program in
               let comming_align = kt |> size `align program in

               let aligned = align acc_size comming_align in
               let new_align = max acc_align comming_align in
               ( aligned ++ comming_size,
                 new_align,
                 _acc_packed_size ++ comming_size
               )
             )
             (0L, 0L, 0L)
      in
      match calcul with
      | `size ->
          if alignment = 0L then
            0L
          else
            align size alignment
      | `align ->
          alignment
    )

and size_struct calcul program generics struct_decl =
  struct_decl.rfields
  |> List.map (fun (_, kt) -> RType.remap_generic_ktype generics kt)
  |> size_tuple calcul program

and size_enum calcul program generics enum_decl =
  enum_decl.rvariants
  |> List.map (fun (_, kts) ->
         kts
         |> List.map (RType.remap_generic_ktype generics)
         |> List.cons (RTInteger (Unsigned, I32))
         |> RType.rtuple |> size calcul program
     )
  |> List.fold_left max 0L

let sizeof program ktype = size `size program ktype

let sizeof_kt kt =
  match Hashtbl.find_opt mapsize kt with
  | Some i ->
      i
  | None ->
      kt |> Asttypprint.string_of_rktype
      |> Printf.sprintf "Size not found for : %s"
      |> failwith

let alignmentof program ktype = size `align program ktype

let offset_of_tuple_index ?(generics = Hashtbl.create 0) index rktypes rprogram
    =
  let ( ++ ) = Int64.add in

  rktypes
  |> List.mapi (fun i v -> (i, v))
  |> List.fold_left
       (fun ((acc_size, acc_align, found) as acc) (tindex, rktype) ->
         let comming_size =
           rktype |> RType.remap_generic_ktype generics |> size `size rprogram
         in
         let comming_align =
           rktype |> RType.remap_generic_ktype generics |> size `align rprogram
         in

         let aligned = align acc_size comming_align in
         let new_align = max acc_align comming_align in

         if found then
           acc
         else if index = tindex then
           (aligned, new_align, true)
         else
           (aligned ++ comming_size, new_align, found)
       )
       (0L, 0L, false)
  |> function a, _, _ -> a

let offset_of_field ?(generics = Hashtbl.create 0) field rstruct_decl rprogram =
  let field_index =
    rstruct_decl.rfields
    |> List.mapi (fun index value -> (index, value))
    |> List.find_map (fun (index, (sfield, _)) ->
           if field = sfield then
             Some index
           else
             None
       )
    |> Option.get
  in

  let rktypes = rstruct_decl.rfields |> List.map snd in
  offset_of_tuple_index ~generics field_index rktypes rprogram

(* To refacto later in functor*)
(* on X86 and Arm64: if the retuned_value can be held in R0 and R1 (RAX, RCX)*)
(* If so, there is no need to pass the address of the destination to the function*)
(* Therefore : the retunred values dont need to be on the stack since there are discarded*)
(* But Kosu ABI is than is the return size is > than a word size*)
(* The return address must be passed so we cannot discard from 8-16 L *)
let discardable_size = function 1L | 2L | 4L | 8L -> true | _ -> false

let compute_ktype rprogram ktype =
  match Hashtbl.find_opt mapsize ktype with
  | Some _ ->
      ()
  | None ->
      let size = sizeof rprogram ktype in
      Hashtbl.replace mapsize ktype size

let rec compute_all_size_typed_expr rprogram typed_expression =
  let () = compute_ktype rprogram typed_expression.rktype in
  match typed_expression.rexpression with
  | REmpty
  | RTrue
  | RFalse
  | RENullptr
  | RECmpLess
  | RECmpEqual
  | RECmpGreater
  | REInteger (_, _, _)
  | REFloat (_, _)
  | REChar _
  | REstring _
  | REAdress _
  | REDeference (_, _)
  | REIdentifier _
  | REConst_Identifier _ ->
      ()
  | REStruct struct_expr ->
      struct_expr.fields
      |> List.iter (fun (_, type_expr) ->
             compute_all_size_typed_expr rprogram type_expr
         )
  | REEnum { assoc_exprs = ty_exprs; _ }
  | REArray ty_exprs
  | RETuple ty_exprs
  | REBuiltin_Function_call { parameters = ty_exprs; _ }
  | REFunction_call { parameters = ty_exprs; _ } ->
      ty_exprs |> List.iter (compute_all_size_typed_expr rprogram)
  | REWhile (condition_expr, kbody) ->
      let () = compute_all_size_typed_expr rprogram condition_expr in
      let () = compute_all_size_kbody rprogram kbody in
      ()
  | REIf (condition, if_body, else_body) ->
      let () = compute_all_size_typed_expr rprogram condition in
      let () = compute_all_size_kbody rprogram if_body in
      let () = compute_all_size_kbody rprogram else_body in
      ()
  | RECases { cases; else_case } ->
      let () =
        cases
        |> List.iter (fun (condition, body) ->
               let () = compute_all_size_typed_expr rprogram condition in
               let () = compute_all_size_kbody rprogram body in
               ()
           )
      in
      let () = compute_all_size_kbody rprogram else_case in
      ()
  | RESizeof rktype ->
      compute_ktype rprogram rktype
  | REAdressof raffacted_value -> (
      match raffacted_value with
      | RAFVariable (_, kt) | RAFField { variable = _, kt; fields = _ } ->
          compute_ktype rprogram kt
    )
  | REFieldAcces { first_expr; _ } | RETupleAccess { first_expr; _ } ->
      compute_all_size_typed_expr rprogram first_expr
  | REArrayAccess { array_expr; index_expr } ->
      let () = compute_all_size_typed_expr rprogram array_expr in
      let () = compute_all_size_typed_expr rprogram index_expr in
      ()
  | RESwitch { rexpression; cases; wildcard_case : rkbody option } ->
      let () = compute_all_size_typed_expr rprogram rexpression in
      let () =
        cases
        |> List.iter (fun (_, uplets, kbody) ->
               let () =
                 uplets
                 |> List.iter (fun (_, _, kt) -> compute_ktype rprogram kt)
               in
               compute_all_size_kbody rprogram kbody
           )
      in
      let () = wildcard_case |> Option.iter (compute_all_size_kbody rprogram) in
      ()
  | REBinOperator_Function_call binop | REBin_op binop ->
      let lhs, rhs = Asttyhelper.Binop.operands binop in
      let () = compute_all_size_typed_expr rprogram lhs in
      let () = compute_all_size_typed_expr rprogram rhs in
      ()
  | REUnOperator_Function_call unop | REUn_op unop -> (
      match unop with
      | RUMinus typed_expression | RUNot typed_expression ->
          compute_all_size_typed_expr rprogram typed_expression
    )

and compute_all_size_statement rprogram = function
  | RSDeclaration { typed_expression; _ }
  | RSAffection (_, typed_expression)
  | RSDiscard typed_expression
  | RSDerefAffectation (_, typed_expression) ->
      compute_all_size_typed_expr rprogram typed_expression

and compute_all_size_kbody rprogram (stmts, final_expr) =
  let () = stmts |> List.iter (compute_all_size_statement rprogram) in
  let () = compute_all_size_typed_expr rprogram final_expr in
  ()

let compute_all_size_module_node rprogram = function
  | RNOpaque _ ->
      ()
  | RNConst rconst_decl ->
      compute_ktype rprogram rconst_decl.value.rktype
  | RNExternFunc { fn_parameters = parameters; return_type; _ }
  | RNSyscall { parameters; return_type; _ } ->
      let () = parameters |> List.iter (compute_ktype rprogram) in
      let () = compute_ktype rprogram return_type in
      ()
  | RNOperator roperator_decl -> (
      match roperator_decl with
      | RBinary { rbfields = (_, lhs), (_, rhs); return_type; kbody; _ } ->
          let () = compute_ktype rprogram lhs in
          let () = compute_ktype rprogram rhs in
          let () = compute_ktype rprogram return_type in
          compute_all_size_kbody rprogram kbody
      | RUnary { rfield = _, kt; return_type; kbody; _ } ->
          let () = compute_ktype rprogram kt in
          let () = compute_ktype rprogram return_type in
          compute_all_size_kbody rprogram kbody
    )
  | RNEnum _ ->
      (* let () = renum_decl.rvariants |> List.iter ( fun (_, kts) ->
           kts |> List.iter (compute_ktype rprogram)
         ) in *)
      ()
  | RNStruct { rfields = _; _ } ->
      ()
  | RNFunction rfunction_decl ->
      (* let () = Printf.printf "fn = %s\n%!" rfunction_decl.rfn_name in *)
      let () =
        rfunction_decl.rparameters
        |> List.iter (fun (_, kt) -> compute_ktype rprogram kt)
      in
      let () = compute_ktype rprogram rfunction_decl.return_type in
      let () = compute_all_size_kbody rprogram rfunction_decl.rbody in
      ()

let compute_all_size_module_path rprogram { rmodule = RModule rmodules; _ } =
  rmodules |> List.iter (compute_all_size_module_node rprogram)

(** To be call once all generics are replaced *)
let compute_all_size rprogram () =
  (* type of enum tag *)
  let () = compute_ktype rprogram @@ RTInteger (Unsigned, I32) in
  rprogram
  |> List.iter (fun { rmodule_path : rmodule_path; _ } ->
         compute_all_size_module_path rprogram rmodule_path
     )
