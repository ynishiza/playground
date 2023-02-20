{- ORMOLU_DISABLE -}
module ParseBasic (
  integral,
  positiveInt,
  negativeInt,
  digit,
  sumOp,
  minusOp,
  multOp,
  divOp,
  expOp,
  arithmeticBinaryOp,
  arithmeticUnaryOp,
  negateOp,
  unaryExp,
  binaryExpr,

  bool,
  and,
  or,

  test,
  ) where
{- ORMOLU_ENABLE -}

import Combinators
import Base.Combinators qualified as PB
import Base.Parse qualified as PB
import Prelude hiding (and, or)

type PBinary a = P (a -> a -> a)
type PUnary a = P (a -> a)

digit :: Integral n => P n
digit = parserToP PB.digit

integral :: Integral n => P n
integral = negativeInt <|> positiveInt

negativeInt :: Integral n => ParseBuilder n
negativeInt = char '-' >> (negate <$> positiveInt)

positiveInt :: Integral n => ParseBuilder n
positiveInt = do
  (v : vs) <- many1 digit
  return $ foldl (\xs x -> 10 * xs + x) v vs

arithmeticBinaryOp :: Integral n => PBinary n
arithmeticBinaryOp = choice [sumOp, minusOp, multOp, expOp]

arithmeticUnaryOp :: Integral n => PUnary n
arithmeticUnaryOp = choice [negateOp]

sumOp :: Integral n => PBinary n
sumOp = char '+' >> return (+)

minusOp :: Integral n => PBinary n
minusOp = char '-' >> return (-)

multOp :: Integral n => PBinary n
multOp = char '*' >> return (*)

divOp :: Fractional n => PBinary n
divOp = char '/' >> return (/)

expOp :: Integral n => PBinary n
expOp = char '^' >> return (^)

negateOp :: Integral n => PUnary n
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

ptrue :: P Bool
ptrue = string "True" >> return True

pfalse :: P Bool
pfalse = string "False" >> return False

bool :: P Bool
bool = ptrue <|> pfalse

and :: PBinary Bool
and = string "&&" >> return (&&)

or :: PBinary Bool
or = string "||" >> return (||)

test :: IO ()
test = do
  print $ PB.parse (pToParser $ parserToP PB.int) "123"
