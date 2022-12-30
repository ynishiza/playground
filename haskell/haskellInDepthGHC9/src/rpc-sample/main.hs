import qualified Client as C
import RPC.Base

main :: IO ()
main = do
  res <- requestRSIO @() defaultRPCParams $ do
    h <- C.hello
    x <- C.add 1 2
    return (h, x)
  print res
