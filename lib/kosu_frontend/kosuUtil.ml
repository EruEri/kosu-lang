(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of Kosu                                                                  *)
(* Copyright (C) 2023 Yves Ndiaye                                                             *)
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



module LocType = struct
  open KosuAst.TyLoc
  let isize_8 = Some KosuAst.I8
  let isize_16 = Some KosuAst.I16
  let isize_32 = Some KosuAst.I32
  let isize_64 = Some KosuAst.I64

  let signed = Some KosuAst.Signed
  let unsigned = Some KosuAst.Unsigned
  let s8 = TyLocInteger (signed, isize_8)
  let u8 = TyLocInteger (unsigned, isize_8)
  let s16 = TyLocInteger (signed, isize_16)
  let u16 = TyLocInteger (unsigned, isize_16)
  let s32 = TyLocInteger (signed, isize_32)
  let u32 = TyLocInteger (unsigned, isize_32)
  let s64 = TyLocInteger (signed, isize_64)
  let u64 = TyLocInteger (unsigned, isize_64)

end