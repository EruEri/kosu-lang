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
%}

%token <KosuAst.integer_info option * int64> IntegerLitteral
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
%token STAR MINUS PIPE MINUS_SUP
%token LPARENT RPARENT LBRACE RBRACE LSQBRACE RSQBRACE WILDCARD
%token CROISILLION
%token SEMICOLON 
%token TYPE OPAQUE AS
%token ENUM ARRAY EXTERNAL STRUCT TRUE FALSE EMPTY IF ELSE CONST VAR OF MUT
%token FUNCTION CLOSURE
%token CASES DISCARD NULLPTR NULLMUTPTR SYSCALL WHILE OPEN
%token CMP_LESS CMP_EQUAL CMP_GREATER MATCH ADDRESSOF SIZEOF
%token DOT
%token BACKTICK
%token EQUAL
%token COMMA
%token DOUBLECOLON COLON
%token EOF

%left INFIX_PIPE
%left INFIX_AMPERSAND
%left PIPE
%left INFIX_CARET
%left INFIX_EQUAL INFIX_TILDE
%left INFIX_INF INFIX_SUP
%left INFIX_PLUS INFIX_MINUS MINUS
%left STAR INFIX_MULT INFIX_DIV INFIX_PERCENT
%left INFIX_DOLLAR
%left DOT

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

%inline sqrbracketed(X):
    | delimited(LSQBRACE, X, RSQBRACE) { $1 }

%inline module_resolver:
    | mp=loption(terminated(separated_nonempty_list(DOUBLECOLON, located(ModuleIdentifier)), DOT)) { 
        ModuleResolverLoc mp
    }

%inline loption_parenthesis_separated_list(sep, X):
    | loption(parenthesis(separated_nonempty_list(sep, X))) {
        $1
    }

%inline splitted(lhs, sep, rhs):
    | lhs=lhs sep rhs=rhs { lhs, rhs }

%inline trailing_separated_list(sep, elt):
    | nonempty_list(terminated(elt, sep)) { $1 }
    | separated_nonempty_list(sep, elt) { $1 }

%inline backticked(X):
    | delimited(BACKTICK, X, BACKTICK) { $1 }


%inline infix_operator:
    | STAR { "*" }
    | MINUS { "-" }
    | PIPE { "|" }
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
        KosuType.TyLoc.PolymorphicVarLoc $1
    }

%inline pointer_state:
    | CONST { Const }
    | MUT | VAR { Mutable }

%inline anon_function_kind:
    | FUNCTION { KAFunctionPointer }
    | CLOSURE { KAClosure }

%inline variable_constancy:
    | CONST { true }
    | VAR { false }

%inline kosu_lvalue:
    | variable=loc_var_identifier fields=loption(preceded(DOT, separated_nonempty_list(DOT, located(Identifier)))) {
        KosuLvalue {variable; fields}
    } 

%inline else_block:
    | located(option(preceded(ELSE, kosu_block))) { 
        match $1.value with
        | Some body -> body
        | None -> KosuAst.{
            kosu_stmts = [];
            kosu_expr = Position.map (fun _ -> EEmpty) $1
        }
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
    | STRUCT struct_name=located(Identifier) 
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
    | TRUE { ETrue }
    | FALSE { EFalse }
    | NULLPTR { ENullptr }
    | CMP_LESS { ECmpLess }
    | CMP_EQUAL { ECmpEqual }
    | CMP_GREATER { ECmpGreater }
    | StringLitteral { EStringl $1 }
    | CharLitteral { EChar $1 }
    | IntegerLitteral { 
        let integer_info, ivalue = $1 in
        EInteger {
            integer_info;
            ivalue
        }
    }
    | FloatLitteral { 
        let fsize, fvalue = $1 in
        EFloat { fsize; fvalue }
    }
    | SIZEOF value=parenthesis(preceded(COLON, located(kosu_expression))) {
        ESizeof (Either.right value)
    }
    | SIZEOF value=parenthesis(located(kosu_type)) {
        ESizeof (Either.left value)
    }
    | kosu_block {
        EBlock $1
    }
    | lhs=located(kosu_expression) fn_name=located(infix_operator) rhs=located(kosu_expression) {
        let parameters = lhs::rhs::[] in
        EFunctionCall {
            module_resolver = KosuUtil.ModuleResolver.empty_module;
            generics_resolver = None;
            fn_name;
            parameters
        }
    }
    | fn_name=located(Builtin) parameters=parenthesis(separated_list(COMMA, located(kosu_expression))) {
        EBuiltinFunctionCall {
            fn_name;
            parameters
        }
    }
    | module_resolver=module_resolver identifier=located(Constant) {
        EConstIdentifier {
            module_resolver;
            identifier
        }
    }
    | module_resolver=module_resolver id=loc_var_identifier {
        EIdentifier {
            module_resolver;
            id
        }
    }
    | first_expr=located(kosu_expression) DOT field=located(Identifier) {
        EFieldAccess {
            first_expr;
            field
        }
    }
    | module_resolver=module_resolver struct_name=located(Identifier) COLON fields=bracketed(
        trailing_separated_list(
            COMMA,
                id=located(Identifier) expr=option(preceded(EQUAL, located(kosu_expression))) {id, expr}
        )
    ) {

        let fields = List.map (fun (id, expr_opt) -> 
            let expr = match expr_opt with
                | Some expr -> expr
                | None -> Position.map_use (fun id -> EIdentifier {
                    module_resolver = KosuUtil.ModuleResolver.empty_module;
                    id
                } ) id
            in
            id, expr
        ) fields in
        EStruct {
            module_resolver;
            struct_name;
            fields
        }

    }
    | module_resolver=module_resolver enum_name=enum_resolver 
        variant=located(Identifier)
        assoc_exprs=loption(parenthesis(trailing_separated_list(COMMA, located(kosu_expression)))) {
            EEnum {
                module_resolver;
                enum_name;
                variant;
                assoc_exprs
            }
        }
    | WHILE condition_expr=located(kosu_expression) body=kosu_block {
        EWhile {
            condition_expr;
            body
        }
    }
    | IF condition_expr=located(kosu_expression) if_block=kosu_block else_body=else_block {
        let cases = (condition_expr, if_block)::[] in
        ECases {
            cases;
            else_body
        }
    }
    | CASES cases=bracketed(
        cases=nonempty_list(
            preceded(
                OF,
                splitted(
                    located(kosu_expression),
                    MINUS_SUP,
                    kosu_block
                )
            )
        )
        else_body=else_block {cases, else_body}
    ) {
        let cases, else_body = cases in
        ECases {
            cases; 
            else_body
        }
    }
    | MATCH expression=located(kosu_expression) patterns=bracketed(
        nonempty_list(
            preceded(
                PIPE,
                splitted(
                    located(kosu_pattern),
                    MINUS_SUP,
                    kosu_block
                )
            )
        )
    ) {
        EMatch {
            expression;
            patterns
        }
    }
    | kind=anon_function_kind 
        parameters=separated_list(COMMA, kosu_function_parameters)
        MINUS_SUP
        body=kosu_block {
            EAnonFunction {
                kind;
                parameters;
                body
            }
    }
    | parenthesis(separated_list(COMMA, located(kosu_expression))) {
        match $1 with
        | [] -> EEmpty
        | t::[] -> t.value
        | _::_ as elts -> ETuple elts
    }

%inline enum_resolver:
    | DOT { None }
    | terminated(located(Identifier), DOUBLECOLON) { Some $1 }

kosu_pattern:
    | TRUE { PTrue }
    | FALSE { PFalse }
    | EMPTY { PEmpty }
    | CMP_LESS { PCmpLess }
    | CMP_EQUAL { PCmpEqual }
    | CMP_GREATER { PCmpGreater }
    | NULLPTR { PNullptr }
    | WILDCARD { PWildcard }
    | located(FloatLitteral) { 
        let value = Position.map snd $1 in
        PFloat value
    }
    | located(CharLitteral) { 
        PChar $1
    }
    | neg_sign=boption(MINUS) located(IntegerLitteral) {
        let value = Position.map snd $2 in
        let value = Position.map (fun value -> 
            match neg_sign with
            | true -> Int64.neg value
            | false -> value
        ) value
        in
        PInteger {
            value
        }
    }
    | module_resolver=module_resolver id=loc_var_identifier {
        let () = ignore module_resolver in
        PIdentifier id
    }
    | module_resolver=module_resolver enum_name=enum_resolver DOT 
        variant=located(Identifier) 
        assoc_patterns=loption(parenthesis(separated_nonempty_list(COMMA, located(kosu_pattern))))  {
        PCase {
            module_resolver;
            enum_name;
            variant;
            assoc_patterns
        }
    }
    |  module_resolver=module_resolver struct_name=located(Identifier) DOT pfields=bracketed(
        trailing_separated_list(SEMICOLON, 
            splitted(located(Identifier), EQUAL, located(kosu_pattern))
        )) {
            PRecord {
                module_resolver;
                struct_name;
                pfields;
            }
        }
    | lpattern=located(kosu_pattern) PIPE rpattern=located(kosu_pattern) {
        let lpattern = KosuUtil.Pattern.flatten_por lpattern in
        let rpattern = KosuUtil.Pattern.flatten_por rpattern in
        let patterns = lpattern @ rpattern in
        POr patterns
    }
    | t=parenthesis(
        splitted(
            located(kosu_pattern),
            AS,
            loc_var_identifier
        )
    ) {
        let pas_pattern, pas_bound = t in
        PAs {
            pas_pattern;
            pas_bound
        }
    }
    | parenthesis(separated_list(COMMA, located(kosu_pattern))) {
        match $1 with
        | [] -> PEmpty
        | p::[] -> p.value
        | list -> PTuple list
    }




kosu_type:
    | module_resolver=module_resolver name=located(Identifier) parametrics_type=parenthesis(separated_nonempty_list(COMMA, located(kosu_type)))  {
        KosuType.TyLoc.TyLocIdentifier {
            module_resolver;
            parametrics_type;
            name
        }
    }
    | module_resolver=module_resolver id=located(Identifier) {
        let open KosuUtil.LocType in
        let open KosuType.TyLoc in
        let ModuleResolverLoc content = module_resolver in
        match Util.Ulist.is_empty content with
        | false -> TyLocIdentifier {
            module_resolver = module_resolver;
            parametrics_type = [];
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
            | "ssize" -> ssize
            | "order" -> TyLocOrdered
            | "stringl" -> TyLocStringLit
            | _ -> TyLocIdentifier {
                module_resolver = module_resolver;
                parametrics_type = [];
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
        | [] -> KosuType.TyLoc.TyLocUnit
        | t::[] -> t.value
        | _::_ as l -> TyLocTuple l
    }
    | STAR pointer_state=pointer_state pointee_type=located(kosu_type) {
        KosuType.TyLoc.TyLocPointer {
            pointer_state;
            pointee_type;
        }
    }
    | FUNCTION parameters=parenthesis(separated_list(COMMA, located(kosu_type))) return_type=located(kosu_type) {
        KosuType.TyLoc.TyLocFunctionPtr (parameters, return_type)
    } 
    | CLOSURE parameters=parenthesis(separated_list(COMMA, located(kosu_type))) return_type=located(kosu_type) {
        KosuType.TyLoc.TyLocClosure (parameters, return_type)
    }
    | loc_poly_vars {
        KosuType.TyLoc.TyLocPolymorphic $1
    }

c_type:
    | { failwith "C Type" }

