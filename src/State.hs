module State where

--import Control.Monad.Writer (liftIO,WriterT(..),execWriterT,tell)
import Control.Monad.State (liftIO,StateT(..),evalStateT,get,put,modify)



test1 :: IO ()
test1 = evalStateT (f 100000) (0.0,1,1)
  where
    f :: Int -> StateT (Double,Int,Int) IO ()
    f 0 = return ()
    f i = do
      (x,a,b) <- get
      let
        x' = x + (fromIntegral a)/(fromIntegral b)
        a' = a * (-1)
        b' = b + 2
      liftIO $ putStrLn $ show $ x' * 4.0
      put (x',a',b')
      f (i-1)


{--
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
--}
