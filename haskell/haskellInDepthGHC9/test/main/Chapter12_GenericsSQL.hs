{-# LANGUAGE OverloadedStrings #-}

module Chapter12_GenericsSQL
  ( specs,
  )
where

import Chapter12.GenericsSQL
import Test.Hspec
import Test.Tasty.Hspec

specs :: SpecWith ()
specs = describe "Chapter12" $ do
  it "should form query for Student" $ do
    let s1 = MkStudent 1 "StudentA" 2000
        s2 = MkStudent 2 "StudentB" 2003
    insertInto "student" s1 `shouldBe` "INSERT INTO student (studentId,name,year) VALUES (1,'StudentA',2000)"
    insertInto "student" s2 `shouldBe` "INSERT INTO student (studentId,name,year) VALUES (2,'StudentB',2003)"
  it "should form query for Course" $ do
    let c1 = MkCourse 10 "Math" "TeacherA"
        c2 = MkCourse 21 "Science" "TeacherB"
    insertInto "course" c1 `shouldBe` "INSERT INTO course (courseId,title,instructor) VALUES (10,'Math','TeacherA')"
    insertInto "course" c2 `shouldBe` "INSERT INTO course (courseId,title,instructor) VALUES (21,'Science','TeacherB')"
