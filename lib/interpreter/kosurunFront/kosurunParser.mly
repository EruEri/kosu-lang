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


%{
    open KosurunAst
    open KosuVirtualMachine.FFIType
%}


%token <string> Bytecode
%token <string> Shebang
%token <string> Identifier
%token <string> StringLit
%token <string> Cfn_name
%token <int> Integer
%token NEWLINE
%token LPARENT
%token RPARENT
%token EQUAL
%token LBRACE
%token RBRACE
%token Croisillon
%token DOLLAR
%token EOF
%token TY_S8 
%token TY_S16
%token TY_S32
%token TY_S64
%token TY_U8 
%token TY_U16
%token TY_U32
%token TY_U64
%token TY_PTR

%start kosurun_ast

%type <kosurun_ast> kosurun_ast

%%


%inline parenthesis(X):
    | delimited(LPARENT, X, RPARENT) { $1 }

%inline bracked(X):
    | delimited(LBRACE, X, RBRACE) { $1 }

kosurun_ast:
    | shebang=option(Shebang) 
        lines=list(line)
        bytecode=Bytecode
    { 
        {shebang; lines; bytecode}
    }
    | EOF { failwith "Parsing error" }
    | error {
        failwith "Catch error"
    }

line:
    | parameter NEWLINE { 
        let k, v = $1 in
        Parameter (k,v)
    }
    | parenthesis(ccentry) NEWLINE { CCentry $1 } 

ccentry:
    | function_name=Cfn_name arity=Integer dynlib_entry=Integer 
        args=parenthesis(list(parenthesis(address)))
        ty_args=parenthesis(list(ffi_type))
        ty_return=ffi_type
    {
        {
            function_name; arity; dynlib_entry;
            args; ty_args; ty_return
        
        }
    }


ffi_type:
    | TY_S8  { FFI_S8 }
    | TY_S16   { FFI_S16 }
    | TY_S32 { FFI_S32 }
    | TY_S64  { FFI_S64 }
    | TY_U8 { FFI_U8 }
    | TY_U16 { FFI_U16 }
    | TY_U32 { FFI_S32 }
    | TY_U64 { FFI_S64 }
    | TY_PTR { FFI_Pointer }
    | bracked(list(ffi_type)) {
        FFI_Struct $1
    }


address:
    | base_reg=Integer offset=parenthesis(address_offset) {
        { base_reg; offset }
    }

address_offset:
    | Croisillon Integer { Off_Reg $2 }
    | DOLLAR Integer { Off_value $2 }
parameter:
    | Identifier EQUAL StringLit {
        $1, $3
    }