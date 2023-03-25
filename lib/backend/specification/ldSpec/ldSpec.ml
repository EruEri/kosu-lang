(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of Kosu                                                                  *)
(* Copyright (C) 2022-2023 Yves Ndiaye                                                        *)
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

module MacOSLdSpec : KosuBackend.Compil.LinkerOption = struct
  type linker_option = string

  let string_of_option = Fun.id
  let ld_command = "ld"

  let options =
    [ "syslibroot $(xcrun --sdk macosx --show-sdk-path)"; "lSystem" ]

  let raw_args = []

  let disable = None

  let should_create_entry_point = None
end

module LinuxLdSpec : KosuBackend.Compil.LinkerOption = struct
  type linker_option = string

  let entry_point = "_start"

  let should_create_entry_point = Some entry_point

  let string_of_option = Fun.id
  let ld_command = "ld"
  let options = [ Printf.sprintf "-entry=%s" entry_point; "lc" ]
  let raw_args = []

  let disable = None
end

module FreeBSD : KosuBackend.Compil.LinkerOption = struct
  type linker_option = string

  let entry_point = "_start"

  let should_create_entry_point = ignore entry_point; None

  let string_of_option = Fun.id
  let ld_command = "ld"
  let options = ["lc"; "dynamic-linker /libexec/ld-elf.so.1"; "L/usr/lib" ]
  let raw_args = ["/usr/lib/crt1.o"; "/usr/lib/crti.o"; "/usr/lib/crtbegin.o"; "/usr/lib/crtend.o"; "/usr/lib/crtn.o"]

  let disable = None
end
