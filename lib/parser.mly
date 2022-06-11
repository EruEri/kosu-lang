%{
    open Ast
%}


%token <Ast.signedness * Ast.isize * int64> Integer_lit
%token <string> String_lit
%token <float> Float_lit
%token <string> IDENT
%token <string> Constant
%token <string> Module_IDENT 
%token LPARENT RPARENT LBRACE RBRACE LSQBRACE RSQBRACE WILDCARD
%token SEMICOLON ARROWFUNC MINUSUP
%token ENUM EXTERNAL SIG FUNCTION STRUCT TRUE FALSE EMPTY SWITCH IF ELSE FOR CONST VAR OF CASES
%token TRIPLEDOT
%token COMMA
%token PIPESUP
%token EQUAL
%token PIPE
%token OR
%token AND
%token XOR
%token AMPERSAND
%token DOUBLEQUAL DIF
%token SUP SUPEQ INF INFEQ
%token SHIFTLEFT SHIFTRIGHT
%token PLUS MINUS
%token MULT DIV MOD
%token NOT SIZEOF DOLLAR
%token DOT
%token DOUBLECOLON COLON
%token EOF


// %left AMPERSAND XOR PIPE AND OR COMMA PIPESUP 
// %left DOUBLECOLON DOT MULT DIV MOD SHIFTLEFT SHIFTRIGHT SUP SUPEQ INF INFEQ DOUBLEQUAL DIF PLUS MINUS
// %nonassoc EOF LPARENT RPARENT LBRACE RBRACE SEMICOLON COLON ARROWFUNC TRIPLEDOT EQUAL Integer_lit String_lit
%left COMMA
%left PIPESUP
%left PIPE
%left OR
%left AND
%left XOR
%left AMPERSAND
%left DOUBLEQUAL DIF
%left SUP SUPEQ INF INFEQ
%left SHIFTLEFT SHIFTRIGHT
%left PLUS MINUS
%left MULT DIV MOD
%nonassoc UMINUS NOT SIZEOF DOLLAR
%left DOUBLECOLON MINUSUP
// %nonassoc ENUM EXTERNAL SIG FUNCTION STRUCT TRUE FALSE EMPTY SWITCH IF ELSE FOR CONST VAR

%start prog


%type <Ast.ktype> ktype
%type <Ast.program> prog
%type <Ast.kexpression> expr

%%

prog:
    | pns = list(prog_nodes) EOF { Prog pns }
;;

prog_nodes:
    | enum_decl { NEnum $1 }
    | struct_decl { NStruct $1 }
    | external_func_decl { NExternFunc $1 }
    | function_decl { NFunction $1 }
    | sig_decl { NSigFun $1 }
    | const_decl { NConst $1 }
;;

enum_decl:
    | ENUM generics_opt=option( LPARENT l=separated_nonempty_list(COMMA, IDENT) RPARENT { l }) LBRACE 
    variants=separated_list(COMMA,enum_assoc) 
    RBRACE name=IDENT SEMICOLON { 
        let generics = generics_opt |> Option.value ~default: [] in
        {
            enum_name = name;
            generics = generics;
            variants = variants;
        }
    }
;;

enum_assoc:
    | id=IDENT opt=option(LPARENT l=separated_nonempty_list(COMMA, ktype) RPARENT {l} ) {
        (
            id,
            opt |> Option.value ~default: []
        )
    }
;;

struct_decl:
    | STRUCT generics_opt=option( LPARENT l=separated_nonempty_list(COMMA, IDENT) RPARENT { l }) LBRACE
    fields=separated_list(COMMA, id=IDENT COLON kt=ktype { id, kt  })
    RBRACE name=IDENT SEMICOLON {
        {
            struct_name = name;
            generics = generics_opt |> Option.value ~default: [];
            fields;
        }
    }
;;

external_func_decl:
    | EXTERNAL id=IDENT LPARENT ctypes=separated_list(COMMA, ctype) varia=option( p=preceded(SEMICOLON, TRIPLEDOT { () }) { p } ) 
    RPARENT r_type=ctype c_name=option(EQUAL s=String_lit { s }) SEMICOLON {
        {
            sig_name = id;
            fn_parameters = ctypes;
            is_variadic = varia |> Option.is_some;
            r_type;
            c_name;
        }
    }
;;

declarer:
    | CONST { true }
    | VAR { false }
;;
statement:
    | declarer IDENT EQUAL expr SEMICOLON { SDeclaration ($2, $4, $1) }
    | IDENT EQUAL expr SEMICOLON { SAffection ($1, $3) }
    | DOLLAR expr { SExpression $2 }
;;


function_decl:
    | FUNCTION name=IDENT generics_opt=option(d=delimited(INF, separated_nonempty_list(COMMA, id=IDENT {id}), SUP ) { d })
    parameters=delimited(LPARENT, separated_list(COMMA, id=IDENT COLON kt=ktype { id, kt  }), RPARENT )
    r_type=ktype LBRACE body=list(statement) RBRACE {
        {
            fn_name = name;
            generics = generics_opt |> Option.value ~default: [];
            parameters;
            return_type = r_type;
            body
        }
    }
;;
sig_decl:
    | SIG name=IDENT generics_opt=option(d=delimited(INF, separated_nonempty_list(COMMA, id=IDENT {id}), SUP ) { d })  
    parameters=delimited(LPARENT, separated_list(COMMA, ktype ), RPARENT ) return_type=ktype SEMICOLON {
        {
            sig_name = name;
            generics = generics_opt |> Option.value ~default: [];
            parameters;
            return_type;
        }
    }
const_decl:
    | CONST Constant EQUAL Integer_lit SEMICOLON {
        let sign, size, value = $4 in
        {
            const_name = $2;
            explicit_type = TInteger (sign, size);
            value = EInteger (sign, size, value);
        }
    }
    | CONST Constant EQUAL String_lit SEMICOLON {
        {
            const_name = $2;
            explicit_type = TString_lit;
            value = EString $4
        }
    }
    | CONST Constant EQUAL Float_lit SEMICOLON {
        {
            const_name = $2;
            explicit_type = TFloat;
            value = EFloat $4
        }
    }
expr:
    | Integer_lit { EInteger $1 }
    | String_lit { EString $1 }
    | Float_lit { EFloat $1 }
    | TRUE { True }
    | FALSE { False }
    | SIZEOF delimited(LPARENT, expr, RPARENT) { ESizeof ( Either.Right $2) }
    | SIZEOF delimited(LPARENT, COLON t=ktype { t } , RPARENT) { ESizeof (Either.Left $2)  }
    | nonempty_list(MULT) IDENT { 
        EDeference ( $1 |> List.length , $2 )
    }
    | AMPERSAND IDENT { EAdress $2 }
    | expr PLUS expr { EBin_op (BAdd ($1, $3) ) }
    | expr MINUS expr { EBin_op (BMinus ($1, $3)) }
    | expr MULT expr { EBin_op (BMult ($1, $3)) }
    | expr DIV expr { EBin_op (BDiv ($1, $3)) }
    | expr MOD expr { EBin_op (BMod ($1, $3)) }
    | expr PIPE expr {  EBin_op (BBitwiseOr ($1, $3)) }
    | expr XOR expr {  EBin_op (BBitwiseXor ($1, $3)) }
    | expr AMPERSAND expr {  EBin_op (BBitwiseAnd ($1, $3)) }
    | expr SHIFTLEFT expr { EBin_op (BShiftLeft ($1, $3)) }
    | expr SHIFTRIGHT expr { EBin_op (BShiftRight ($1, $3)) }
    | expr AND expr { EBin_op (BAnd ($1, $3)) }
    | expr OR expr { EBin_op (BOr ($1, $3)) }
    | expr SUP expr { EBin_op (BSup ($1, $3)) }
    | expr SUPEQ expr { EBin_op (BSupEq ($1, $3)) }
    | expr INF expr { EBin_op (BInf ($1, $3)) }
    | expr INFEQ expr { EBin_op (BInfEq ($1, $3)) }
    | expr DOUBLEQUAL expr { EBin_op (BEqual ($1, $3)) }
    | expr DIF expr { EBin_op (BDif ($1, $3)) }
    | expr MINUSUP separated_nonempty_list(MINUSUP, IDENT) {
        EFieldAcces {
            first_expr = $1;
            fields = $3
        }
    }
    | NOT expr { EUn_op (UNot $2) }
    | MINUS expr %prec UMINUS { EUn_op (UMinus $2) }
    | l=separated_list(DOUBLECOLON, Module_IDENT) name=IDENT generics_resolver=option(DOUBLECOLON INF s=separated_nonempty_list(COMMA, ktype) SUP { s } ) LPARENT exprs=separated_list(COMMA, expr) RPARENT {
        EFunction_call { 
            modules_path = l;
            generics_resolver;
            fn_name = name;
            parameters = exprs;
        }
    }
    | l=separated_list(DOUBLECOLON, Module_IDENT) id=IDENT {
        EIdentifier { 
            modules_path = l;
            identifier = id
        }

    }
    | l=separated_list(DOUBLECOLON, Module_IDENT) id=Constant {
        EConst_Identifier {
            modules_path = l;
            identifier = id
        }
    }
    | expr PIPESUP calls=separated_nonempty_list(PIPESUP, 
        modules=separated_list(DOUBLECOLON, Module_IDENT) name=IDENT
         LPARENT exprs=separated_list(COMMA, expr) RPARENT { name, exprs , modules }) {
            calls |> List.fold_left (
                fun acc value  -> 
                    let fn_name, parameters, modules_path = value in
                    EFunction_call { 
                        modules_path;
                        generics_resolver = None;
                        fn_name;
                        parameters = acc::parameters;
                    }
                ) $1
        }
    | modules_path=separated_list(DOUBLECOLON, Module_IDENT)  struct_name=IDENT fields=delimited(LBRACE, separated_list(COMMA, id=IDENT COLON expr=expr { id, expr } ) , RBRACE) {
        EStruct {
            modules_path;
            struct_name;
            fields
        }
    }
    | modules_path=separated_list(DOUBLECOLON, Module_IDENT) enum_name=option(IDENT) DOT variant=IDENT assoc_exprs=delimited(LPARENT, separated_nonempty_list(COMMA, expr) ,RPARENT) {
        EEnum {
            modules_path;
            enum_name;
            variant;
            assoc_exprs
        }
    }
    | CASES delimited(LBRACE, 
        s=nonempty_list(OF conds=separated_nonempty_list(COMMA, expr) ARROWFUNC LBRACE stmts=nonempty_list(statement) RBRACE { conds, stmts } ) 
        ELSE LBRACE else_case=nonempty_list(statement) RBRACE { s, else_case } , RBRACE) {
            let cases, else_case = $2 in
        ECases {
            cases;
            else_case
        }
    }
    | IF expr delimited(LBRACE, list(statement), RBRACE) ELSE delimited(LBRACE, list(statement), RBRACE)  {
        EIf (
            $2, 
            $3, 
            $5
            (* $4 |> Option.map (fun e -> (SExpression e)::[] ) *)
        )
    }
    | SWITCH expr LBRACE nonempty_list(cases=separated_nonempty_list(COMMA, s_case) ARROWFUNC LBRACE stmts=nonempty_list(statement) RBRACE { cases, stmts } ) 
        wildcard_case=option(WILDCARD ARROWFUNC d=delimited(LBRACE, nonempty_list(statement), RBRACE) { d } ) RBRACE { 
        ESwitch {
            expression = $2;
            cases = $4;
            wildcard_case
        }
    }
    | d=delimited(LPARENT, expr, RPARENT ) { d }
;;
s_case:
    | DOT IDENT { SC_Enum_Identifier { variant = $2 } }
    | DOT IDENT delimited(LPARENT, separated_nonempty_list(COMMA, IDENT { Some $1 } | WILDCARD { None } ), RPARENT) {
        SC_Enum_Identifier_Assoc {
            variant = $2;
            assoc_ids = $3
        }
    }
    | IDENT {
        SC_Identifier $1
    }
    | Integer_lit {
        SC_Integer_Literal $1
    }

ctype:
    | id=IDENT { 
        match id with
        | "f64" -> TFloat
        | "unit" -> TUnit
        | "bool" -> TBool
        | "s8" -> TInteger( Signed, I8)
        | "u8" -> TInteger( Unsigned, I8)
        | "s16" -> TInteger( Signed, I16)
        | "u16" -> TInteger( Unsigned, I16)
        | "s32" -> TInteger( Signed, I32)
        | "u32" -> TInteger( Unsigned, I32)
        | "s64" -> TInteger( Signed, I64)
        | "u64" -> TInteger( Unsigned, I64)
        | _ as s -> TType_Identifier s
     }
    | MULT ktype { TPointer $2 } 

ktype:
    | id=IDENT { 
        match id with
        | "f64" -> TFloat
        | "unit" -> TUnit
        | "bool" -> TBool
        | "s8" -> TInteger( Signed, I8)
        | "u8" -> TInteger( Unsigned, I8)
        | "s16" -> TInteger( Signed, I16)
        | "u16" -> TInteger( Unsigned, I16)
        | "s32" -> TInteger( Signed, I32)
        | "u32" -> TInteger( Unsigned, I32)
        | "s64" -> TInteger( Signed, I64)
        | "u64" -> TInteger( Unsigned, I64)
        | "stringl" -> TString_lit
        | _ as s -> TType_Identifier s
     }
    | MULT ktype { TPointer $2 }
    | LPARENT l=separated_nonempty_list(COMMA, ktype) RPARENT id=IDENT { TParametric_identifier (id, l)  }
    | LPARENT l=separated_nonempty_list(COMMA, ktype) RPARENT { TTuple (l)  }
;;
