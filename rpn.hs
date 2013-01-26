{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative ((<|>),(<*),(<$>))
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.Char8 as AC
import qualified Data.ByteString.Char8 as B
import Data.Conduit (($=),($$))
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Data.Functor ((<$))
import System.IO

main :: IO ()
main = C.runResourceT
       $ CB.sourceHandle stdin
       $= CB.lines
       $= CL.map ((`B.append` "\n") . B.pack . eval . AC.parseOnly rpn)
       $$ CB.sinkHandle stdout

data RPN = Plus
         | Minus
         | Mul
         | Div
         | Val Double
         deriving (Show, Eq)

rpn :: Parser [RPN]
rpn = (rpnValue <|> rpnOp) `A.sepBy1` A.many' AC.space <* AC.endOfInput

rpnOp :: Parser RPN
rpnOp = ops [("+", Plus)
            ,("-", Minus)
            ,("*", Mul)
            ,("/", Div)]
  where ops = A.choice . map (\(s,v) -> v <$ A.string s)

rpnValue :: Parser RPN
rpnValue = Val <$> AC.double

eval :: Either String [RPN] -> String
eval (Left msg) = "parse error: " ++ msg
eval (Right rpns) = evalRPN rpns

evalRPN :: [RPN] -> String
evalRPN = show . head . foldl f []
  where
    f s (Val n)      = n:s
    f (a:b:xs) Plus  = (b + a):xs
    f (a:b:xs) Minus = (b - a):xs
    f (a:b:xs) Mul   = (b * a):xs
    f (a:b:xs) Div   = (b / a):xs
    f _ _            = error "empty stack"
