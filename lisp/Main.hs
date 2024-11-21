{-# LANGUAGE OverloadedStrings #-}


import SchemeParser (parseExpr)
import Text.Megaparsec.Byte (printChar)

main :: IO ()
main = do
    let input = "(define x 42)"
    print $ parseExpr input
    -- print line return
    putStrLn ""


    let lambdaInput = "(lambda (x y) (+ x y))"
    print $ parseExpr lambdaInput
    putStrLn ""

    let ifInput = "(if #t 1 0)"
    print $ parseExpr ifInput
    putStrLn ""

    let callInput = "(+ 1 2)"
    print $ parseExpr callInput
    putStrLn ""
