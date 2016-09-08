{

{-# OPTIONS_GHC -w #-}
module RDL.Lexer
  ( Token(..)
  , scanTokens
  ) where

import RDL.Expr

}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$eol   = [\n]

tokens :-

  $eol                          ;
  $white+                       ;
  "#".*                         ;
  let                           { \s -> TokLet }
  in                            { \s -> TokIn }
  $digit+                       { \s -> TokNum (read s) }
  "->"                          { \s -> TokArrow }
  \=                            { \s -> TokEq }
  \\                            { \s -> TokLambda }
  [\+]                          { \s -> TokAdd }
  [\-]                          { \s -> TokSub }
  [\*]                          { \s -> TokMul }
  \(                            { \s -> TokLParen }
  \)                            { \s -> TokRParen }
  $alpha [$alpha $digit \_ \']* { \s -> TokSym s }

{

data Token
    = TokLet
    | TokIn
    | TokLambda
    | TokNum Int
    | TokSym String
    | TokArrow
    | TokEq
    | TokAdd
    | TokSub
    | TokMul
    | TokLParen
    | TokRParen
    deriving (Eq, Show)

scanTokens = alexScanTokens

}
