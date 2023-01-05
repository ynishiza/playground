{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module DoorSpec
  ( spec,
  )
where

import Data.Maybe
import Door.Door qualified as D
import Door.DoorGen
import Fmt
import Test.Hspec

eq :: D.SomeDoor -> SomeDoor -> Expectation
eq (D.MkSomeDoor d1) (MkSomeDoor d2) = D.doorState d1 `shouldBe` doorState d2

spec :: SpecWith ()
spec = describe "Door" $ do
  let dro = D.mkDoor D.SOpened
      drc = D.mkDoor D.SClosed
      dOpen = fromSDoorState SOpened
      dClosed = fromSDoorState SClosed

  it "should agree" $ do
    D.doorState dro `shouldBe` doorState dOpen
    D.doorState drc `shouldBe` doorState dClosed
    D.MkSomeDoor (D.openAnyDoor dro) `eq` MkSomeDoor (openAnyDoor dOpen)
    D.MkSomeDoor (D.openAnyDoor drc) `eq` MkSomeDoor (openAnyDoor dClosed)
    D.toggleState dro `eq` toggleState dOpen
    D.toggleState drc `eq` toggleState dClosed

  describe "parseDoor" $ do
    it "should perform a sequence of actions" $ do
      let exec :: Show a => String -> a -> IO a
          exec l d = fmt (nameF (build l) (build $ show d)) >> return d
          actionOnOpen :: Door 'Opened -> IO SomeDoor
          actionOnOpen d =
            exec "start" d
              >>= (exec "closeDoor" . closeDoor)
              >>= (exec "openDoor" . openDoor)
              >>= (exec "closeAnyDoor" . closeAnyDoor)
              >>= (exec "openAnyDoor" . openAnyDoor)
              >>= (exec "toggleState" . toggleState)
              >>= (exec "toggleState" . withSomeDoorI toggleState)
              >>= (exec "toggleState" . withSomeDoorI toggleState)

          res :: String -> Maybe (IO SomeDoor)
          res s = do
            v <- parseDoor s
            return $
              withSomeDoor
                ( \sig d -> case sig of
                    SOpened -> actionOnOpen d
                    SClosed -> actionOnOpen $ openDoor d
                )
                v

      x <- fromMaybe undefined $ res "Opened"
      x `shouldBe` MkSomeDoor dClosed
      y <- fromMaybe undefined $ res "Closed"
      y `shouldBe` MkSomeDoor dClosed
      isNothing (res "A") `shouldBe` True

    it "should parse" $ do
      parseDoor "Opened" `shouldBe` Just (MkSomeDoor dOpen)
      parseDoor "Closed" `shouldBe` Just (MkSomeDoor dClosed)
      parseDoor "abc" `shouldBe` Nothing

    it "should be invertible" $ do
      parseDoor (show (doorState dOpen)) `shouldBe` Just (MkSomeDoor dOpen)
      parseDoor (show (doorState dClosed)) `shouldBe` Just (MkSomeDoor dClosed)

  describe "action" $ do
    it "should open any door" $ do
      openAnyDoor dOpen `shouldBe` dOpen
      openAnyDoor dClosed `shouldBe` dOpen

    it "should close any door" $ do
      closeAnyDoor dOpen `shouldBe` dClosed
      closeAnyDoor dClosed `shouldBe` dClosed

    it "should toggle any door" $ do
      toggleState dOpen `shouldBe` MkSomeDoor dClosed
      toggleState dClosed `shouldBe` MkSomeDoor dOpen
