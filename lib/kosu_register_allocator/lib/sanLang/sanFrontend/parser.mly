(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of San: A 3 address code language/compiler                               *)
(* Copyright (C) 2023 Yves Ndiaye                                                             *)
(*                                                                                            *)
(* San is free software: you can redistribute it and/or modify it under the terms             *)
(* of the GNU General Public License as published by the Free Software Foundation,            *)
(* either version 3 of the License, or (at your option) any later version.                    *)
(*                                                                                            *)
(* San is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;           *)
(* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR           *)
(* PURPOSE.  See the GNU General Public License for more details.                             *)
(* You should have received a copy of the GNU General Public License along with San.          *)
(* If not, see <http://www.gnu.org/licenses/>.                                                *)
(*                                                                                            *)
(**********************************************************************************************)

%{
    open SanAst
    open SanPosition
%}

%token <Nativeint.t> Integer_lit
%token <string> Identifier
%token <string> String_lit
%token <string> Label
%token LPARENT RPARENT
%token SSIZE BOOL STRINGL UNIT COMMA COLON SEMICOLON
%token IF GOTO DEF EXTERNAL TRUE FALSE DISCARD LATEINIT END
%token OR RETURN
%token PIPE
%token AND
%token XOR 
%token AMPERSAND EQUAL
%token DOUBLEQUAL DIF
%token SUP SUPEQ INF INFEQ
%token SHIFTLEFT SHIFTRIGHT
%token PLUS MINUS
%token MULT DIV
%token EOF
%token NOT

%start san_module

%type <SanAst.san_module> san_module
%type <SanAst.atom> atom

%%

%inline located(X): x=X {
  SanPosition.located_value $startpos $endpos x
};;


san_module:
    | list(san_node) EOF { $1 }

san_node:
    | EXTERNAL located(Label) delimited(LPARENT, separated_list(COMMA, located(san_type)), RPARENT) located(san_type) option(preceded(EQUAL, located(String_lit))) {
        External { fn_name = $2; signature = ($3, $4); cname = $5 }
    }
    | san_function { $1 }

atom:
    | Identifier { Variable $1 }
    | Integer_lit { Int $1 }
    | String_lit { String $1 }
    | TRUE { Boolean true }
    | FALSE { Boolean false }

san_rvalue:
    | DISCARD preceded(COLON, located(san_type)) { RVDiscard $2 }
    | LATEINIT preceded(COLON, located(san_type)) { RVLater $2 }
    | located(atom) { RVExpr $1 }
    | MINUS located(atom) { RVUnary { unop = TacUminus; atom = $2 } }
    | NOT located(atom) { RVUnary { unop = TacNot; atom = $2} }
    | located(atom) PLUS located(atom) { 
        RVBinary { 
            binop = TacSelf TacAdd;
            blhs = $1;
            brhs = $3 
        }
    }
    | located(atom) MINUS located(atom) { 
        RVBinary { 
            binop = TacSelf TacMinus;
            blhs = $1;
            brhs = $3
        }
    }
    | located(atom) MULT located(atom) { 
        RVBinary { 
            binop = TacSelf TacMult;
            blhs = $1;
            brhs = $3
        }
    }
    | located(atom) DIV located(atom) {
         RVBinary { 
            binop = TacSelf TacDiv;
            blhs = $1;
            brhs = $3
        }
    }
    | located(atom) PIPE located(atom) {
         RVBinary { 
            binop = TacSelf TacBitwiseOr;
            blhs = $1;
            brhs = $3
        }
    }
    | located(atom) AMPERSAND located(atom) {
         RVBinary { 
            binop = TacSelf TacBitwiseAnd;
            blhs = $1;
            brhs = $3
        }
    }
    | located(atom) XOR located(atom) {
         RVBinary { 
            binop = TacSelf TacBitwiseXor;
            blhs = $1;
            brhs = $3
        }
    }
    | located(atom) SHIFTLEFT located(atom) {
         RVBinary { 
            binop = TacSelf TacShiftLeft;
            blhs = $1;
            brhs = $3
        }
    }
    | located(atom) SHIFTRIGHT located(atom) {
         RVBinary { 
            binop = TacSelf TacShiftRight;
            blhs = $1;
            brhs = $3
        }
    }
    | located(atom) OR located(atom) {
         RVBinary { 
            binop = TacBool TacOr;
            blhs = $1;
            brhs = $3
        }
    }
    | located(atom) AND located(atom) {
         RVBinary { 
            binop = TacBool TacAnd;
            blhs = $1;
            brhs = $3
        }
    }
    | located(atom) SUP located(atom) {
         RVBinary { 
            binop = TacBool TacSup;
            blhs = $1;
            brhs = $3
        }
    }
    | located(atom) SUPEQ located(atom) {
         RVBinary { 
            binop = TacBool TacSupEq;
            blhs = $1;
            brhs = $3
        }
    }
    | located(atom) INF located(atom) {
         RVBinary { 
            binop = TacBool TacInf;
            blhs = $1;
            brhs = $3
        }
    }
    | located(atom) INFEQ located(atom) {
         RVBinary { 
            binop = TacBool TacInfEq;
            blhs = $1;
            brhs = $3
        }
    }
    | located(atom) DOUBLEQUAL located(atom) {
         RVBinary { 
            binop = TacBool TacEqual;
            blhs = $1;
            brhs = $3
        }
    }
    | located(atom) DIF located(atom) {
         RVBinary { 
            binop = TacBool TacDiff;
            blhs = $1;
            brhs = $3
        }
    }
    | located(Label) delimited(LPARENT, separated_list(COMMA, located(atom)), RPARENT) {
        RVFunctionCall { fn_name = $1; parameters = $2 }
    }
    
san_statement:
    | located(Identifier) EQUAL located(san_rvalue) SEMICOLON {
        SSDeclaration ($1, $3)
    }

san_ending:
    | RETURN located(atom) { SE_return $2 }
    | IF located(atom) GOTO located(Label) GOTO located(Label) { SE_If {expr = $2; if_label = $4; else_label = $6 } }

san_basic_block:
    | located(Label) COLON nonempty_list(san_statement) option(san_ending) {
        {
            label = $1;
            statements = $3;
            ending = $4
        }
    }
    | located(Label) COLON san_ending {
        {
            label = $1;
            statements = [];
            ending = Some $3
        }
    }
starting_basic_block:
    | nonempty_list(san_statement) option(san_ending) { ($1, $2) }
    | san_ending { [], Some $1 }

san_function:
    | DEF label=located(Label) 
        delimited(LPARENT, separated_list(COMMA, param=located(Identifier) COLON san_type=located(parameter_san_type) {param, san_type} ), RPARENT) 
        located(san_type) COLON 
        sbb=starting_basic_block blocks=list(san_basic_block) END {
            let stmts, return = sbb in
            let convert_starting_block = { label; statements = stmts; ending = return } in
            let san_function = {
                fn_name = label;
                parameters = $3;
                return_type = $4;
                san_basic_blocks = convert_starting_block::blocks
            } in
            Declaration san_function
        }

parameter_san_type:
    | SSIZE { Ssize }
    | BOOL { Boolean }
    | STRINGL { Stringl }

san_type:
    | parameter_san_type { $1 }
    | UNIT { Unit }

