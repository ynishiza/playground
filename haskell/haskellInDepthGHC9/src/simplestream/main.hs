import Control.Arrow ((>>>))
import Control.Monad.IO.Class
import Data.Function
import SimpleStream.Extra
import SimpleStream.Prelude (Of (..), StreamOf)
import SimpleStream.Prelude qualified as S
import SimpleStream.Stream

main :: IO ()
main =
  zipPair
    (repeatsM (putStrLn "Enter number" >> return (() :> ())))
    ( S.readLine @IO @Int
        & S.chain print
        & expand (\inner (x :> xs) -> x :> inner (x :> xs))
        & evenStr
        & S.effects
        & oddStr
    )
    & S.chain print
    & S.effects
  where
    evenStr :: MonadIO m => StreamOf Int m r -> StreamOf Int m r
    evenStr =
      S.filter even
        >>> S.chain (sputstrLn "EVEN")
        >>> S.chain sprint
        >>> S.map (* 2)
        >>> S.chain sprint

    oddStr :: MonadIO m => StreamOf Int m r -> StreamOf Int m r
    oddStr =
      S.filter odd
        >>> S.chain (sputstrLn "ODD")
        >>> S.chain sprint
        >>> S.map (* 10)
        >>> S.chain sprint

sputstrLn :: MonadIO m => String -> b -> m ()
sputstrLn s = const $ liftIO $ putStrLn s

sprint :: (MonadIO m, Show a) => a -> m ()
sprint = liftIO . print
