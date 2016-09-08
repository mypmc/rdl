{

module RDL.Parser where

import RDL.Lexer
import RDL.Expr

}

%name expr
%tokentype { Token }
%error { parseError }

%token
    let   { TokLet }
    in    { TokIn }
    NUM   { TokNum $$ }
    VAR   { TokSym $$ }
    '\\'  { TokLambda }
    '->'  { TokArrow }
    '='   { TokEq }
    '+'   { TokAdd }
    '-'   { TokSub }
    '*'   { TokMul }
    '('   { TokLParen }
    ')'   { TokRParen }

%left '+' '-'
%left '*'
%%

Expr : let VAR '=' Expr in Expr    { App (Abs $2 $6) $4 }
     | '\\' VAR '->' Expr          { Abs $2 $4 }
     | Form                        { $1 }

Form : Form '+' Form               { Binop Add $1 $3 }
     | Form '-' Form               { Binop Sub $1 $3 }
     | Form '*' Form               { Binop Mul $1 $3 }
     | Juxt                        { $1 }

Juxt : Juxt Atom                   { App $1 $2 }
     | Atom                        { $1 }

Atom : '(' Expr ')'                { $2 }
     | NUM                         { Num $1 }
     | VAR                         { Var $1 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

parseExpr :: String -> Expr
parseExpr = expr . scanTokens
}
