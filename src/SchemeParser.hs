{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module SchemeParser where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- Define AST types
data Expr
  = Number Integer
  | Bool Bool
  | Symbol String
  | List [Expr]
  | Define String Expr
  | Lambda [String] Expr
  | If Expr Expr Expr
  | Call Expr [Expr]
  deriving (Show, Eq)

type Parser = Parsec Void Text

-- | Helper: Skipping spaces and comments
sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") empty

-- | Helper: Parsing surrounded by parentheses
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Helper: Symbol parser
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | Parser for identifiers (symbols)
identifier :: Parser String
identifier = lexeme ((:) <$> letterChar <*> many (alphaNumChar <|> oneOf ("?!+-*/<=>:$%^&_~@" :: String)))

-- | Helper: Lexeme parser
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Parser for numbers
number :: Parser Expr
number = Number <$> lexeme L.decimal

-- | Parser for booleans
boolean :: Parser Expr
boolean = lexeme $ (Bool True <$ string "#t") <|> (Bool False <$ string "#f")

-- | Parser for symbols (variable names or function calls)
symbolExpr :: Parser Expr
symbolExpr = Symbol <$> identifier

-- | Parser for a Scheme list
list :: Parser Expr
list = List <$> parens (many expr)

-- | Parser for `define`
define :: Parser Expr
define = parens $ do
  _ <- symbol "define"
  choice
    [ parens $ do
        name <- identifier
        args <- many identifier
        body <- expr
        return $ Define name (Lambda args body),
      do
        name <- identifier
        value <- expr
        return $ Define name value
    ]

-- | Parser for `lambda`
lambda :: Parser Expr
lambda = parens $ do
  _ <- symbol "lambda"
  params <- parens (many identifier)
  body <- expr
  return $ Lambda params body

-- | Parser for `if`
ifExpr :: Parser Expr
ifExpr = parens $ do
  _ <- symbol "if"
  cond <- expr
  thenExpr <- expr
  elseExpr <- expr
  return $ If cond thenExpr elseExpr

-- | Parser for function calls
call :: Parser Expr
call = parens $ do
  func <- expr -- Function being called
  args <- many expr -- Arguments
  return $ Call func args

-- | Main expression parser
expr :: Parser Expr
expr =
  choice
    [ try define, -- Define must precede others
      try lambda, -- Lambda is a special form
      try ifExpr, -- If expressions
      try call, -- Generic function calls
      try list, -- Parse lists
      number, -- Parse numbers
      boolean, -- Parse booleans
      symbolExpr -- Variable or function names
    ]

-- | Top-level parser with spaces handled
parseExpr :: Text -> Either (ParseErrorBundle Text Void) Expr
parseExpr = runParser (sc *> expr <* eof) ""
