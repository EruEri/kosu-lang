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
%token ENUM ARRAY EXTERNAL STRUCT TRUE FALSE EMPTY SWITCH IF ELSE CONST VAR OF MUT
%token FUNCTION CLOSURE
%token CASES DISCARD NULLPTR SYSCALL OPERATOR WHILE OPEN
%token CMP_LESS CMP_EQUAL CMP_GREATER MATCH ADDRESSOF
%token STAR
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
        ModuleResoler mp
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

%inline pointer_state:
    | CONST { Const }
    | MUT | VAR { Mutable }

%inline variable_constancy:
    | CONST { true }
    | VAR { false }

%inline kosu_lvalue:
    | variable=loc_var_identifier fields=loption(preceded(DOT, separated_nonempty_list(DOT, located(Identifier)))) {
        KosuLvalue {variable; fields}
    }

kosu_module:
    | list(kosu_module_node) EOF { $1 }

kosu_module_node:
    | kosu_external_func_decl { NExternFunc $1 }
    | kosu_function_decl { NFunction $1 }
    | kosu_syscall_decl { NSyscall $1 }
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

kosu_syscall_decl:
    | SYSCALL syscall_name=located(Identifier) parameters=parenthesis(separated_list(COMMA, typed_parameter_loc(c_type))) return_type=located(c_type) EQUAL opcode=located(IntegerLitteral)
    {
        {
            syscall_name;
            parameters;
            return_type;
            opcode = Position.map (fun (_, value) -> value ) opcode
        }
    }



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
    | ENUM struct_name=located(Identifier) 
        poly_vars=loption_parenthesis_separated_list(COMMA, loc_poly_vars)
        fields=bracketed(
            trailing_separated_list(
                COMMA, 
                id=located(Identifier) COLON kt=located(kosu_type) {id, kt}
            )
        )
    {
        {
            struct_name;
            poly_vars;
            fields
        }
    }

kosu_function_decl:
    | FUNCTION 
        poly_vars=loption(terminated(separated_nonempty_list(COMMA, loc_poly_vars), DOT))
        fn_name=loc_var_identifier
        parameters=parenthesis(separated_list(COMMA, kosu_function_parameters))
        return_type=located(kosu_type)
        body=kosu_function_block 
        {
            {
                fn_name;
                poly_vars;
                parameters;
                return_type;
                body
            }
        }


%inline kosu_function_parameters:
    | is_var=boption(VAR) name=loc_var_identifier COLON kosu_type=located(kosu_type) {
        {
            is_var;
            name;
            kosu_type
        }
    }

kosu_function_block:
    | preceded(EQUAL, located(kosu_expression)) {
        $1
    }
    | located(kosu_block) {
        Position.map (fun block -> EBlock block) $1
    }

kosu_statement:
    | located(kosu_statement_base) SEMICOLON { $1 }

kosu_statement_base:
    | is_const=variable_constancy 
        pattern=located(kosu_pattern) 
        explicit_type=option(preceded(COLON, located(kosu_type)))
        expression=preceded(EQUAL, located(kosu_expression)) {
        SDeclaration {is_const; pattern; explicit_type; expression}
    }
    | MUT is_deref=boption(STAR) lvalue=kosu_lvalue expression=preceded(EQUAL, located(kosu_expression)) {
        SAffection {
            is_deref;
            lvalue;
            expression
        }
    }
    | DISCARD expr=located(kosu_expression) {
        SDiscard expr
    }
    | OPEN module_resolver=module_resolver {
        SOpen { module_resolver}
    }

    

kosu_block:
    | bracketed(kosu_block_base) { $1 }

kosu_block_base:
    | kosu_stmts=list(kosu_statement) kosu_expr=located(kosu_expression) {
        {
            kosu_stmts;
            kosu_expr
        }
    }

kosu_expression:
    | { failwith "Kosu expresion"}

kosu_pattern:
    | { failwith "Kosu pattern"}



kosu_type:
    | module_resolver=module_resolver name=located(Identifier) parametrics_type=parenthesis(separated_nonempty_list(COMMA, located(kosu_type)))  {
        KosuAst.TyLoc.TyLocParametricIdentifier {
            module_resolver;
            parametrics_type;
            name
        }
    }
    | module_resolver=module_resolver id=located(Identifier) {
        let open KosuUtil.LocType in
        let open KosuAst.TyLoc in
        let ModuleResoler content = module_resolver in
        match Util.Ulist.is_empty content with
        | false -> TyLocIdentifier {
            module_resolver = module_resolver;
            name = id
        } 
        | true -> begin match id.value with
            | "unit" -> TyLocUnit
            | "bool" -> TyLocBool
            | "char" -> TyLocChar
            | "f64" -> f64
            | "f32" -> f32
            | "s8" -> s8
            | "u8" -> u8
            | "s16" -> s16
            | "u16" -> u16
            | "s32" -> s32
            | "u32" -> u32
            | "s64" -> s64
            | "u64" -> u64
            | "usize" -> usize
            | "isize" -> ssize
            | "order" -> TyLocOrdered
            | "stringl" -> TyLocStringLit
            | _ -> TyLocIdentifier {
                module_resolver = module_resolver;
                name = id
            } 
        end
    }
    | module_resolver=module_resolver CROISILLION name=located(Identifier) {
        TyLocOpaque {module_resolver; name}
    }
    | ARRAY parenthesis(size=located(IntegerLitteral) COLON ktype=located(kosu_type) {ktype, size}) {
        let ktype, size = $2 in
        let size = Position.map (fun (_, value) -> value) size in
        TyLocArray {
            size;
            ktype
        }
    }
    | parenthesis(separated_list(COMMA, located(kosu_type) )) {
        match $1 with
        | [] -> KosuAst.TyLoc.TyLocUnit
        | t::[] -> t.value
        | _::_ as l -> TyLocTuple l
    }
    | STAR pointer_state=pointer_state pointee_type=located(kosu_type) {
        KosuAst.TyLoc.TyLocPointer {
            pointer_state;
            pointee_type;
        }
    }
    | loc_poly_vars {
        KosuAst.TyLoc.TyLocPolymorphic $1
    }

c_type:
    | { failwith "C Type" }