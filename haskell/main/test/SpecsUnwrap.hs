module SpecsUnwrap
  ( specs,
  )
where

import Test.Hspec
import TestUnwrap

specs :: SpecWith ()
specs = describe "Expansion" $ do
  let one = EInt 1
      two = EInt 2
      three = EInt 3
      a = EStr "a"
      b = EStr "b"
      l1 = [EItem one, EItem two, EItem three]
      l2 = [EItem a, EItem b]

  it "should expand empty" $ do
    expandElem [] `shouldBe` [[]]

  it "should expand deep empty deep nested" $ do
    expandElem [EList []] `shouldBe` []
    expandElem [EList [EList [EList [EList []]]]] `shouldBe` []

  it "should short circuit" $ do
    expandElem [EItem one, EList []] `shouldBe` []

  it "should expand trivial" $ do
    expandElem l1 `shouldBe` [[one, two, three]]
    expandElem l2 `shouldBe` [[a, b]]
    expandElem (l1 <> l2) `shouldBe` [[one, two, three, a, b]]

  it "should expand nested" $ do
    expandElem [EItem one, EItem two, EList l1, EList l2]
      `shouldBe` [ [one, two, one, a],
                   [one, two, one, b],
                   [one, two, two, a],
                   [one, two, two, b],
                   [one, two, three, a],
                   [one, two, three, b]
                 ]
  it "should expand deep nested" $ do
    expandElem
      [ EItem one,
        EList
          [ EItem two,
            EList
              [ EItem three,
                EList
                  [ EItem a,
                    EList [EItem b]
                  ]
              ]
          ]
      ]
      `shouldBe` [[one, two], [one, three], [one, a], [one, b]]
