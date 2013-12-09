module GA.GA where

import Control.Monad
import Data.Random
import Data.List

data Individual = Individual { x :: [Double],
                               f :: Double } deriving (Show, Eq)

-- Could Double be a polymorphic param with a typeclass instead?
mkIndividual :: Int -> RVar Double -> RVar Individual
mkIndividual n s = do
  position <- sequence (replicate n s)
  return (Individual position (fitness position))

mkPopulation :: Int -> RVar Individual -> RVar [Individual]
mkPopulation = replicateM

-- Evaluate the Fitness of a solution. This is simply the spherical function.
fitness :: [Double] -> Double
fitness l = sum $ map (\a -> a * a) l

-- Now for some operators (not monadic)
onePoint :: Individual -> Individual -> Int -> [[Double]]
onePoint a b point = [aa, bb]
               where aa = take point xa ++ drop point xb
                     bb = take point xb ++ drop point xa
                     xa = x a
                     xb = x b

-- Monadic one-point (delegates to non-monadic version)
onePointM :: Individual -> Individual -> RVar Int -> RVar [[Double]]
onePointM a b point = do
  p <- point
  return (onePoint a b p)

-- Monadic gausian mutation
mutateM :: (Num a, Ord a) => a -> RVar a -> RVar a -> [a] -> RVar [a]
mutateM p_m uni dist position = do
  points <- replicateM (length position) uni
  mults <- replicateM (length position) dist
  return (zipWith3 (\a b c -> if p_m < b then a * c else a) position points mults)

-- Monadic reproduction is the composition of monadic crossover and mutation
reproduce :: Double -> Double -> Individual -> Individual -> RVar [[Double]]
reproduce p_c p_m a b = do
  r <- uniform 0.0 1.0
  if p_c < r then offspring else (return [])
    where len       = length $ zip (x a) (x b)
          offspring = do
            o <- onePointM a b (uniform 0 len)
            sequence (map (mutateM p_m (uniform 0.0 1.0) (normal 0.0 1.0)) o)

choose :: [a] -> RVar a
choose l = do
  r <- uniform 0 (length l - 1)
  return (l !! r)

randomSelection :: [Individual] -> RVar Individual
randomSelection = choose

selectReproduce :: [Individual] -> RVar [Individual]
selectReproduce l = do
  a <- randomSelection l
  b <- randomSelection l
  r <- reproduce 0.3 0.7 a b
  return (map (\z -> Individual z (fitness z)) r)

cull :: Int -> [Individual] -> [Individual]
cull size pop = take size $ sortBy (\a b -> compare (f a) (f b)) pop

iteration :: RVar [Individual] -> RVar [Individual]
iteration l = do
  ll <- l
  a <- sequence $ replicate (length ll) (selectReproduce ll)
  return (cull (length ll) (concat a))
