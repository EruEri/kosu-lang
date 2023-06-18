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

type passed_kind = Other | Float

type passing_style =
  | Simple_Reg of passed_kind
  | Double_Reg of passed_kind * passed_kind

type ('a, 'b) return_kind = Simple_return of 'a | Double_return of ('a * 'b)

let simple_return a = Simple_return a
let double_return a b = Double_return (a, b)
let pop_opt = function [] -> (None, []) | t :: q -> (Some t, q)

let double_pop_opt = function
  | ([] | _ :: []) as l -> (None, l)
  | t :: t2 :: q -> (Some (t, t2), q)


let rec consume_args_sysv ~reversed_stack ~fregs ~iregs ~fargs ~iargs ~stacks_args
  ~fpstyle ttes =
match ttes with
| [] -> (List.rev iargs, List.rev fargs, if reversed_stack then stacks_args else List.rev stacks_args) (* Since args are stacked in the reversed order og ttes *)
| t :: q -> begin
        match fpstyle t with
        | Simple_Reg kind -> (
            match kind with
            | Other -> (
                let head_reg, remain_reg = pop_opt iregs in
                match head_reg with
                | None ->
                  consume_args_sysv ~reversed_stack ~fregs ~iregs ~fargs ~iargs ~fpstyle
                      ~stacks_args:(t :: stacks_args) q
                | Some reg ->
                  consume_args_sysv ~reversed_stack ~fregs ~iregs:remain_reg ~fargs
                      ~iargs:((t, simple_return reg) :: iargs)
                      ~stacks_args ~fpstyle q)
            | Float -> (
                let head_reg, freg_remain = pop_opt fregs in
                match head_reg with
                | None ->
                  consume_args_sysv ~reversed_stack ~fregs ~iregs ~fargs ~iargs
                      ~stacks_args:(t :: stacks_args) ~fpstyle q
                | Some reg ->
                  consume_args_sysv ~reversed_stack ~fregs:freg_remain ~iregs
                      ~fargs:((t, simple_return reg) :: fargs)
                      ~iargs ~stacks_args ~fpstyle  q))
        | Double_Reg (lhs, rhs) -> begin
            match (lhs, rhs) with
            | Float, Float -> (
                match fregs with
                | [] | _ :: [] ->
                  consume_args_sysv ~reversed_stack ~fregs ~iregs ~fargs ~iargs ~fpstyle
                      ~stacks_args:(t :: stacks_args)  q
                | fr1 :: fr2 :: remains ->
                    let arg = (t, double_return fr1 fr2) in
                    consume_args_sysv ~reversed_stack ~fregs:remains ~iregs ~fargs:(arg :: fargs)
                      ~iargs ~fpstyle ~stacks_args:(t :: stacks_args)
                       q)
            | _ -> (
                match fregs with
                | [] | _ :: [] ->
                  consume_args_sysv ~reversed_stack ~fregs ~iregs ~fargs ~iargs ~fpstyle
                      ~stacks_args:(t :: stacks_args)  q
                | ir1 :: ir2 :: remains ->
                    let arg = (t, double_return ir1 ir2) in
                    consume_args_sysv ~reversed_stack ~fregs ~iregs:remains ~fargs
                      ~iargs:(arg :: iargs) ~fpstyle
                      ~stacks_args:(t :: stacks_args) q
                )
                end
    end 

let rec consume_args ~novariadic_args ~fregs ~iregs ~fargs ~iargs ~stacks_args ~variadic_args
    ~fpstyle ~i ttes =
  match ttes with
  | [] -> (List.rev iargs, List.rev fargs, List.rev stacks_args, List.rev variadic_args)
  | t :: q -> (
      let next_i = i + 1 in
      match novariadic_args with
      | Some nvags when i >= nvags ->
          consume_args 
          ~novariadic_args
          ~fregs ~iregs ~fargs ~iargs ~fpstyle
            ~stacks_args ~variadic_args:(t :: variadic_args) ~i:next_i q
      | None | Some _ -> (
          match fpstyle t with
          | Simple_Reg kind -> (
              match kind with
              | Other -> (
                  let head_reg, remain_reg = pop_opt iregs in
                  match head_reg with
                  | None ->
                      consume_args 
                      ~novariadic_args
                      ~fregs ~iregs ~fargs ~iargs ~fpstyle
                        ~variadic_args
                        ~stacks_args:(t :: stacks_args) ~i:next_i q
                  | Some reg ->
                      consume_args 
                      ~novariadic_args
                      ~fregs ~iregs:remain_reg ~fargs
                        ~iargs:((t, simple_return reg) :: iargs)
                        ~variadic_args
                        ~stacks_args ~fpstyle ~i:next_i q)
              | Float -> (
                  let head_reg, freg_remain = pop_opt fregs in
                  match head_reg with
                  | None ->
                      consume_args 
                      ~novariadic_args
                      ~fregs ~iregs ~fargs ~iargs
                      ~variadic_args
                        ~stacks_args:(t :: stacks_args) ~fpstyle ~i:next_i q
                  | Some reg ->
                      consume_args 
                      ~novariadic_args
                      ~fregs:freg_remain ~iregs
                        ~fargs:((t, simple_return reg) :: fargs)
                        ~variadic_args
                        ~iargs ~stacks_args ~fpstyle ~i:next_i q))
          | Double_Reg (lhs, rhs) -> (
              match (lhs, rhs) with
              | Float, Float -> (
                  match fregs with
                  | [] | _ :: [] ->
                      consume_args 
                      ~novariadic_args
                      ~fregs ~iregs ~fargs ~iargs ~fpstyle
                      ~variadic_args
                        ~stacks_args:(t :: stacks_args) ~i:next_i q
                  | fr1 :: fr2 :: remains ->
                      let arg = (t, double_return fr1 fr2) in
                      consume_args 
                      ~novariadic_args
                      ~fregs:remains ~iregs ~fargs:(arg :: fargs)
                      ~variadic_args
                        ~iargs ~fpstyle ~stacks_args:(t :: stacks_args)
                        ~i:next_i q)
              | _ -> (
                  match fregs with
                  | [] | _ :: [] ->
                      consume_args 
                      ~novariadic_args
                      ~fregs ~iregs ~fargs ~iargs ~fpstyle
                      ~variadic_args
                        ~stacks_args:(t :: stacks_args) ~i:next_i q
                  | ir1 :: ir2 :: remains ->
                      let arg = (t, double_return ir1 ir2) in
                      consume_args 
                      ~novariadic_args
                      ~fregs ~iregs:remains ~fargs
                        ~iargs:(arg :: iargs) ~fpstyle
                        ~variadic_args
                        ~stacks_args:(t :: stacks_args) ~i:next_i q))))

let consume_args ?novariadic_args = consume_args ~novariadic_args ~fargs:[] ~iargs:[] ~stacks_args:[] ~variadic_args:[] ~i:0

let consume_args_sysv ~reversed_stack = consume_args_sysv ~reversed_stack ~fargs:[] ~iargs:[] ~stacks_args:[] 
