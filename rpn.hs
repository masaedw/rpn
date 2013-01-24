{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
--import Data.Conduit.Attoparsec
import Control.Applicative ((<|>),(*>),(<$>),pure)
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.Char8 as AC
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
  return ()

data RPN = Plus
         | Minus
         | Mul
         | Div
         | Val Integer
         deriving (Show, Eq)

rpn :: Parser [RPN]
rpn = (rpnValue <|> rpnOp) `A.sepBy1` A.many' AC.space

rpnOp :: Parser RPN
rpnOp = ops [("+", Plus)
            ,("-", Minus)
            ,("*", Mul)
            ,("/", Div)]
  where ops = A.choice . map (\(s::B.ByteString,v) -> A.string s *> pure v)

rpnValue :: Parser RPN
rpnValue = Val . toInteger <$> AC.decimal
