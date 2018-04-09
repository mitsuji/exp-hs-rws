module Writer
    ( someFunc
    ) where

import Control.Monad.Writer (liftIO,WriterT(..),execWriterT,tell)

someFunc :: IO ()
someFunc = putStrLn "someFunc"


test1 ::IO ()
test1 = func1

func1 :: IO ()
func1 = do
  write "abc\n"
  write "def\n"
  write "ghi\n"
  where
    write :: String -> IO ()
    write s = putStr s


test2 ::IO ()
test2 =  putStr =<< execWriterT func2
  
func2 :: WriterT String IO ()
func2 = do
  write "abc\n"
  write "def\n"
  write "ghi\n"
  where
    write :: String -> WriterT String IO ()
    write s = tell s
