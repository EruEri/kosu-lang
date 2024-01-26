(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of Kosu                                                                  *)
(* Copyright (C) 2024 Yves Ndiaye                                                             *)
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


let of_kosu_type : KosuFrontendAlt.Type.Ty.kosu_type -> TyzuType.tyzu_type = function
  | KosuFrontendAlt.Type.Ty.TyIdentifier {module_resolver; parametrics_type; name} -> 
    failwith ""
    | TyPolymorphic _ -> failwith ""
    | TyPointer _ -> failwith ""
    | TyInteger integer -> 
      let integer = Option.value ~default:(failwith "") integer in
      TyzuInteger integer
    | TyFloat _ -> failwith ""
    | TyFunctionPtr _ -> failwith ""
    | TyClosure _ -> failwith ""
    | TyArray _ -> failwith ""
    | TyTuple _ -> failwith ""
    | TyOpaque _ -> failwith ""
    | TyOrdered -> TyzuOrdered
    | TyStringLit -> TyzuStringLit
    | TyChar -> TyzuChar
    | TyBool -> TyzuBool
    | TyUnit -> TyzuUnit

let of_external_decl kosu_program current_module external_func_decl = 
  let KosuFrontendAlt.Ast.{sig_name; parameters; return_type; c_name} = external_func_decl in


let of_module_node kosu_program current_module = function
  | KosuFrontendAlt.Ast.NExternFunc e -> failwith ""
  | NFunction _ -> failwith ""
  | NStruct _ -> failwith ""
  | NEnum _ -> failwith ""
  | NConst _ -> failwith ""
  | NOpaque _ -> failwith ""

let of_module kosu_program kosu_module = 
  List.map (of_module_node kosu_program kosu_module) kosu_module

let of_named_module kosu_program kosu_named_module = 
  let KosuFrontendAlt.Ast.{kosu_module; filename} = kosu_named_module in
  let tyzu_module = of_module kosu_program kosu_module in
  TyzuAst.{tyzu_module; filename}


let of_program kosu_program =
  List.map (of_named_module kosu_program) kosu_program