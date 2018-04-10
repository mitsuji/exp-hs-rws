module Writer
    ( someFunc
    ) where

import Control.Monad.Writer (liftIO,WriterT(..),execWriterT,tell)

someFunc :: IO ()
someFunc = putStrLn "someFunc"


test1 :: IO ()
test1 = f
  where
    f :: IO ()
    f = do
      func1
      func1
      func1

func1 :: IO ()
func1 = do
  putStr "abc\n"
  putStr "def\n"
  putStr "ghi\n"



test2 :: IO ()
test2 = putStr =<< f
  where
    f :: IO String
    f = do
      s1 <- func2
      s2 <- func2
      s3 <- func2
      return $ mconcat [s1,s2,s3]

func2 :: IO String
func2 =
  return $ mconcat ["abc\n","def\n","ghi\n"]



test3 :: IO ()
test3 = putStr =<< execWriterT f
  where
    f :: WriterT String IO ()
    f = do
      func3
      func3
      func3
  
func3 :: WriterT String IO ()
func3 = do
  tell "abc\n"
  tell "def\n"
  tell "ghi\n"
