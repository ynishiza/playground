{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Chapter12.GenericsSQL
  ( run,
  )
where

import Data.List (intercalate)
import qualified Data.Text as T
import Fmt
import GHC.Generics
import GHC.Exts (IsString)
import Utils

run :: TestState
run =
  createChapterTest
    "12.2"
    "SQL"
    ( do
        let s1 = MkStudent 1 "StudentA" 2000
            s2 = MkStudent 2 "StudentB" 2003
            c1 = MkCourse 10 "Math" "TeacherA"
            c2 = MkCourse 21 "Science" "TeacherB"
         in do
              assertIsEqual (insertInto "student" s1) "INSERT INTO student (studentId,name,year) VALUES (1,'StudentA',2000)"
              assertIsEqual (insertInto "student" s2) "INSERT INTO student (studentId,name,year) VALUES (2,'StudentB',2003)"
              assertIsEqual (insertInto "course" c1) "INSERT INTO course (courseId,title,instructor) VALUES (10,'Math','TeacherA')"
              assertIsEqual (insertInto "course" c2) "INSERT INTO course (courseId,title,instructor) VALUES (21,'Science','TeacherB')"
        testDone
    )

data Student = MkStudent
  { studentId :: !Int,
    name :: !T.Text,
    year :: !Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSQL)

data Course = MkCourse
  { courseId :: !Int,
    title :: !T.Text,
    instructor :: !T.Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToSQL)

class ToSQL a where
  insertInto :: T.Text -> a -> T.Text
  default insertInto :: (Generic a, ColumnInfo (Rep a)) => T.Text -> a -> T.Text
  insertInto name entity = "INSERT INTO " +| name |+ " (" +| toList c1 |+ ") VALUES (" +| toList v1 |+ ")"
    where
      (c1, v1) = getColumnInfo (from entity)
      toList :: [Builder] -> T.Text
      toList builders = T.pack $ intercalate "," $ fmt <$> builders

class ColumnInfo m where
  getColumnInfo :: m a -> ([Builder], [Builder])

instance ColumnInfo U1 where
  getColumnInfo _ = ([], [])

instance (ColumnInfo a, ColumnInfo b) => ColumnInfo (a :*: b) where
  getColumnInfo (x :*: y) = (c1 <> c2, v1 <> v2)
    where
      (c1, v1) = getColumnInfo x
      (c2, v2) = getColumnInfo y

instance ColumnInfo a => ColumnInfo (M1 m s a) where
  getColumnInfo x =
    if l1 /= l2
      then error $ "Length mismatch. Columns=" +| l1 |+ " Values=" +| l2 |+ ""
      else info
    where
      info = getColumnInfo $ unM1 x
      l1 = length (fst info)
      l2 = length (snd info)

instance {-# OVERLAPPING #-} (ColumnInfo a, Selector s) => ColumnInfo (M1 S s a) where
  getColumnInfo x = (build (selName x) : c, v)
    where
      (c, v) = getColumnInfo $ unM1 x

instance {-# OVERLAPPING #-} ColumnInfo (K1 R T.Text) where
  getColumnInfo (K1 v) = ([], ["'" +| v |+ "'"])

instance {-# OVERLAPPING #-} Buildable a => ColumnInfo (K1 R a) where
  getColumnInfo (K1 v) = ([], [build v])
