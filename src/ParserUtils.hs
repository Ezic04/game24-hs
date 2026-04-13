module ParserUtils where

import Control.Applicative (Alternative (empty))
import Control.Monad.Combinators.Expr (Operator (..))
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, between)
import Text.Megaparsec.Char (space1)
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

binaryL, binaryR :: Text -> (a -> a -> a) -> Operator Parser a
binaryL name f = InfixL (f <$ symbol name)
binaryR name f = InfixR (f <$ symbol name)

prefix, postfix :: Text -> (a -> a) -> Operator Parser a
prefix name f = Prefix (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)
