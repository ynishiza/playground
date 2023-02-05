{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

import Control.Monad.Free
import Control.Monad.IO.Class
import Control.Monad.RWS.Class
import Control.Monad.Trans.RWS (RWST, runRWST)

data Action w s a where
  PutStrLn :: String -> a -> Action w s a
  GetLine :: (String -> a) -> Action w s a
  Get :: (s -> a) -> Action w s a
  Put :: s -> a -> Action w s a
  Tell :: w -> a -> Action w s a
  deriving (Functor)

type ActionF w s = Free (Action w s)

putStrLnF :: String -> ActionF w s ()
putStrLnF s = liftF $ PutStrLn s ()

getLineF :: ActionF w s String
getLineF = liftF $ GetLine id

tellF :: w -> ActionF w s ()
tellF w = liftF $ Tell w ()

getF :: ActionF w s s
getF = liftF $ Get id

putF :: s -> ActionF w s ()
putF s = liftF $ Put s ()

foldIO :: forall a. ActionF String Int a -> IO a
foldIO = iterM f
  where
    f :: Action String Int (IO x) -> IO x
    f (PutStrLn msg a) = putStrLn msg >> a
    f (GetLine next) = putStr "Enter value:" >> getLine >>= next
    f (Get next) = next 0
    f (Put _ a) = a
    f (Tell w a) = putStrLn ("LOG:" <> w) >> a

foldString :: forall a. ActionF String Int a -> String
foldString (Pure _) = ""
foldString (Free (PutStrLn s a)) = s <> ":" <> foldString a
foldString (Free (GetLine next)) = "GETLN:" <> foldString (next "100")
foldString (Free (Get next)) = "GET:" <> foldString (next 0)
foldString (Free (Put _ a)) = "PUT:" <> foldString a
foldString (Free (Tell w a)) = "TELL:" <> w <> foldString a

foldRWS :: (MonadRWS r w s m, MonadIO m) => ActionF w s a -> m a
foldRWS = iterM f
  where
    f (PutStrLn s a) = fPutStr s >> a
    f (GetLine next) = liftIO (putStr "Enter value:" >> getLine) >>= next
    f (Get next) = fPutStr "Get:" >> get >>= next
    f (Put s a) = put s >> fPutStr "Put:" >> a
    f (Tell w a) = tell w >> a
    fPutStr msg = liftIO $ putStrLn msg

test :: IO ()
test = do
  let c :: ActionF String Int ()
      c = do
        tellF "INFO: Start"
        putStrLnF "Hello"
        putStrLnF "World"
        x <- getLineF
        y <- getLineF
        putStrLnF ("x=" <> x <> ", y=" <> y)

        putStrLnF "=== State === "
        getF >>= (putStrLnF . ("current state:" <>)) . show
        getF >>= putF . (+ 100)
        getF >>= (putStrLnF . ("updated state:" <>)) . show

        tellF "END: Start"
        pure ()

  putStrLn "========== foldIO =========="
  foldIO c

  putStrLn "========== foldString =========="
  putStrLn $ foldString c

  putStrLn "========== foldRWS =========="
  let r = foldRWS c :: RWST () String Int IO ()
  (_, finalState, finalLog) <- runRWST r () 0
  putStrLn $ "log:" <> finalLog
  putStrLn $ "state:" <> show finalState

  pure ()

type MaybeF = Free Maybe

runMaybe :: Free Maybe a -> Maybe a
runMaybe (Pure a) = Just a
runMaybe (Free x) = x >>= runMaybe
