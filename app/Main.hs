module Main where

import RDL (parseExpr)
import RDL.Eval
import RDL.Expr

main :: IO ()
main =
  do cs <- getContents
     runEvalWith parseExpr cs

runEvalWith
  :: (String -> Expr) -> String -> IO ()
runEvalWith parseExpr input =
  do let ast = parseExpr input
     putStrLn $ "Source: " ++ (source ast)
     putStrLn $ "AST: " ++ (show ast)
     putStrLn $ "Eval: " ++ (show (eval ast))
