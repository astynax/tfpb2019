module PPrint where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer

type PPrinter a =
  ReaderT (Int, Int)
    (StateT Int
      (Writer [String])) a

comp :: PPrinter ()
comp = do
  brackets "{" "}" $ do
    write "a = 1"
    write "b = 2"
    brackets "[" "]" $ do
      write "42,"
      write "15,"

brackets :: String -> String -> PPrinter a -> PPrinter a
brackets l r inner = do
  write l
  x <- pad inner
  write r
  pure x

write :: String -> PPrinter ()
write msg = do
  -- reader
  (_, padding) <- ask
  -- state
  lift $ modify (+ 1)
  -- writer
  lift $ lift $ tell [replicate padding ' ' ++ msg]

pad :: PPrinter a -> PPrinter a
pad = local (\(s, x) -> (s, x + s))

run :: Int -> PPrinter a -> IO Int
run step action = do
  let (count, xs) =
        runWriter $ execStateT (runReaderT action (step, 0)) 0
  putStrLn (unlines xs)
  pure count
