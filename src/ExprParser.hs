module ExprParser where

import Control.Monad.Combinators.Expr (Operator, makeExprParser)
import Expr (BinOpType (..), Expr (..), ExprNat, UnrOpType (..))
import ParserUtils (Parser, binaryL, binaryR, lexeme, parens, postfix, prefix, spaceConsumer, symbol)
import Text.Megaparsec (MonadParsec (eof), some, (<?>), (<|>))
import Text.Megaparsec.Char (space, string)
import Text.Megaparsec.Char.Lexer qualified as L

pLit :: Parser ExprNat
pLit = Lit <$> lexeme L.decimal

pMul :: Parser ExprNat
pMul = BinOp Mul <$> pLit <*> (foldr1 (BinOp Mul) <$> pRest)
  where
    pRest = some $ symbol "*" *> pLit

pTerm :: Parser ExprNat
pTerm = pLit <|> parens pExpr <?> "term"

operatorTable :: [[Operator Parser ExprNat]]
operatorTable =
  [ [ postfix "!" $ UnrOp Fct,
      binaryR "^" $ BinOp Pow
    ],
    [ prefix "-" $ UnrOp Neg
    ],
    [ binaryL "*" $ BinOp Mul,
      binaryL "/" $ BinOp Div
    ],
    [ binaryL "+" $ BinOp Add,
      binaryL "-" $ BinOp Sub
    ]
  ]

pExpr :: Parser ExprNat
pExpr = makeExprParser pTerm operatorTable <?> "expression"

pExprEof :: Parser ExprNat
pExprEof = spaceConsumer *> pExpr <* eof