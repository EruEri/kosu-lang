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

%{
    open KosuAst
    open Position

    (* let identifier_to_expr id_loc = 
    Position.map_use (fun id -> 
        EIdentifier {
            modules_path = {
                v = String.empty;
                position = Position.end_to_start id.position
            };
            identifier = id
        }
    ) id_loc *)
%}

%token <(KosuAst.signedness option * KosuAst.isize option) * int64> IntegerLitteral
%token <string> StringLitteral 
%token <char> CharLitteral
%token <KosuAst.fsize option * float> FloatLitteral
%token <string> Identifier
%token <string> Builtin
%token <string> Constant
%token <string> ModuleIdentifier
%token <string> PolymorphicVar
%token <string> INFIX_PIPE
%token <string> INFIX_AMPERSAND
%token <string> INFIX_EQUAL
%token <string> INFIX_INF
%token <string> INFIX_SUP
%token <string> INFIX_CARET (* ^ *)
%token <string> INFIX_PLUS
%token <string> INFIX_MINUS
%token <string> INFIX_MULT
%token <string> INFIX_DIV
%token <string> INFIX_DOLLAR
%token <string> INFIX_PERCENT
%token <string> INFIX_TILDE
%token <string> PREFIX_EXCLA
%token <string> PREFIX_QUESTIONMARK
%token LPARENT RPARENT LBRACE RBRACE LSQBRACE RSQBRACE WILDCARD
%token CROISILLION
%token SEMICOLON 
%token TYPE OPAQUE
%token ENUM ARRAY EXTERNAL FUNCTION STRUCT TRUE FALSE EMPTY SWITCH IF ELSE CONST VAR OF 
%token CASES DISCARD NULLPTR SYSCALL OPERATOR WHILE OPEN MUT
%token CMP_LESS CMP_EQUAL CMP_GREATER MATCH ADDRESSOF
%token TRIPLEDOT DOT
%token BACKTICK
%token EQUAL
%token COMMA

%token DOUBLECOLON COLON
%token EOF

%start kosu_module

%type <KosuAst.kosu_module> kosu_module


%%

%inline located(X): x=X {
  Position.located_value $startpos $endpos x
}

%inline parenthesis(X):
    | delimited(LPARENT, X, RPARENT) { $1 }


%inline bracketed(X):
    | delimited(LBRACE, X, RBRACE) { $1 }

%inline module_resolver:
    | mp=loption(terminated(separated_nonempty_list(DOUBLECOLON, located(ModuleIdentifier)), DOT)) { 
        mp
    }

%inline loption_parenthesis_separated_list(sep, X):
    | loption(parenthesis(separated_nonempty_list(sep, X))) {
        $1
    }

%inline trailing_separated_list(sep, elt):
    | nonempty_list(terminated(elt, sep)) { $1 }
    | separated_nonempty_list(sep, elt) { $1 }

%inline backticked(X):
    | delimited(BACKTICK, X, BACKTICK) { $1 }

%inline infix_operator:
    | INFIX_PIPE
    | INFIX_AMPERSAND
    | INFIX_EQUAL
    | INFIX_INF
    | INFIX_SUP
    | INFIX_CARET (* ^ *)
    | INFIX_PLUS
    | INFIX_MINUS
    | INFIX_MULT
    | INFIX_DIV
    | INFIX_DOLLAR
    | INFIX_PERCENT
    | INFIX_TILDE { $1 }

%inline prefix_operator:
    | PREFIX_EXCLA
    | PREFIX_QUESTIONMARK { $1 }


%inline loc_var_identifier:
    | located(Identifier)
    | located(prefix_operator)
    | backticked(located(infix_operator)) { $1 }


%inline loc_poly_vars:
    | located(PolymorphicVar) { 
        KosuAst.TyLoc.PolymorphicVar $1
    }

kosu_module:
    | list(kosu_module_node) EOF { $1 }

kosu_module_node:
    | kosu_external_func_decl { NExternFunc $1 }
    | kosu_opaque_decl { NOpaque $1 }
    | kosu_const_decl { NConst $1 }
    | kosu_enum_decl { NEnum $1 }
    | kosu_struct_decl { NStruct $1 }


%inline typed_parameter_loc(X):
    | WILDCARD COLON x=located(X) { x }
    | Identifier COLON x=located(X)  { x }

kosu_external_func_decl:
    | EXTERNAL sig_name=loc_var_identifier 
        parameters=parenthesis(
            trailing_separated_list(
                COMMA, typed_parameter_loc(c_type)
            )
        )
        return_type=located(c_type)
        c_name=option(preceded(EQUAL, StringLitteral))
    { 
        {
            sig_name;
            parameters;
            return_type;
            c_name
        }
     }
;;



kosu_opaque_decl:
    | OPAQUE TYPE name=located(Identifier) {
        { name }
    }

kosu_const_decl:
    | CONST const_name=located(Constant) COLON explicit_type=located(kosu_type) EQUAL value=located(kosu_expression) {
        {const_name; explicit_type; value}
    }

kosu_enum_decl:
    | ENUM enum_name=located(Identifier) 
        poly_vars=loption_parenthesis_separated_list(COMMA, loc_poly_vars)
        tag_type=located(option(preceded(COLON, located(kosu_type)))) variants=bracketed(
            trailing_separated_list(
                COMMA, 
                kosu_enum_decl_case
            )
        ) 
        { 
            let tag_type = match tag_type.value with
                | Some t -> t
                | None -> Position.map (fun _ -> KosuUtil.LocType.s32 ) tag_type
            in

            {
                enum_name;
                poly_vars;
                tag_type;
                variants
            }
        }

%inline kosu_enum_decl_case:
    | id=located(Identifier) t=loption_parenthesis_separated_list(COMMA, located(kosu_type)) {
        id, t
    }

kosu_struct_decl:
    | { failwith ""}

kosu_expression:
    | { failwith "Kosu expresion"}

kosu_type:
    | { failwith "Type todo" }

c_type:
    | { failwith "C Type" }