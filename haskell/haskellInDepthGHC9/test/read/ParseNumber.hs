module ParseNumber (
  int,
  positiveInt,
  negativeInt,
  digit,
  sumOp,
  minusOp,
  multOp,
  arithmeticBinaryOp,
  arithmeticUnaryOp,
  negateOp,
  unaryExp,
  binaryExpr,
  test,
  ) where

import Combinators
import Base.Combinators qualified as PB
import Base.Parse qualified as PB

type PBinary a = P (a -> a -> a)
type PUnary a = P (a -> a)

digit :: P Int
digit = parserToP PB.digit

int :: P Int
int = negativeInt <|> positiveInt

negativeInt :: ParseBuilder Int
negativeInt = char '-' >> (negate <$> positiveInt)

positiveInt :: ParseBuilder Int
positiveInt = do
  (v : vs) <- many1 digit
  return $ foldl (\xs x -> 10 * xs + x) v vs

arithmeticBinaryOp :: PBinary Int
arithmeticBinaryOp = choice [sumOp, minusOp, multOp]

arithmeticUnaryOp :: PUnary Int
arithmeticUnaryOp = choice [negateOp]

sumOp :: PBinary Int
sumOp = char '+' >> return (+)

minusOp :: PBinary Int
minusOp = char '-' >> return (-)

multOp :: PBinary Int
multOp = char '*' >> return (*)

negateOp :: PUnary Int
negateOp = char '-' >> return negate

unaryExp :: PUnary a -> P a -> P a
unaryExp pun pv = do
  skipSpaces
  op <- pun
  skipSpaces
  v <- pv
  skipSpaces
  return $ op v

binaryExpr :: PBinary a -> P a -> P a
binaryExpr pop pv = do
  skipSpaces
  v1 <- pv
  skipSpaces
  op <-pop
  skipSpaces
  op v1 <$> pv


test :: IO ()
test = do
  print $ PB.parse (pToParser $ parserToP PB.int) "123"
