module Main (main) where

import GA.GA
import Control.Monad.State
import Data.Random

main :: IO ()
main = do
  s <- sample $ execState (replicateM 300 threadState) (mkPopulation 30 (mkIndividual 30 stdUniform))
  putStrLn $ show (head s)

threadState :: State (RVar [Individual]) (RVar [Individual])
threadState = do
  _ <- modify iteration
  final <- get
  return final
