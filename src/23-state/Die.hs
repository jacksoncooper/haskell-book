-- Page 880

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random

data Die =
    DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    x -> error $ "The function 'intToDie' got "
                  ++ show x
                  ++ ", which is out of the range 1 through 6."

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes =
  let
    stdGen = mkStdGen 0
    (dieOne, stdGenOne) = randomR (1, 6) stdGen
    (dieTwo, stdGenTwo) = randomR (1, 6) stdGenOne
    (dieThree, _)       = randomR (1, 6) stdGenTwo
  in
    (intToDie dieOne, intToDie dieTwo, intToDie dieThree)

rollDie :: State StdGen Die
rollDie = state $ do
  (die, stdGen) <- randomR (1, 6)
  return $ (intToDie die, stdGen)

-- The function rollDie' uses the monad for functions, a.k.a, the Reader monad,
-- to compose a function from StdGen -> (Die, StdGen).

-- randomR (1, 6) ::
--   (Random a, RandomGen g, Num a) => g -> (a, g)

-- \stdGen ->
--   (\(die, stdGen) -> return (intToDie die, stdGen))
--     (randomR (1, 6) stdGen) stdGen

rollDie' :: State StdGen Die
rollDie' = state $
  randomR (1, 6) >>=
    \(die, stdGen) ->
      return (intToDie die, stdGen)

rollDie'' :: State StdGen Die
rollDie'' =
  intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' =
  liftA3 (,,) rollDie rollDie rollDie

nDie :: Int -> State StdGen [Die]
nDie n =
  replicateM n rollDie

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty = go 0 0
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= 20 = count
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
        in go (sum + die) (count + 1) nextGen

-- 1.

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN goal = go goal 0 0
  where
    go :: Int -> Int -> Int -> StdGen -> Int
    go goal sum count gen
      | sum >= goal = count
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
        in go goal (sum + die) (count + 1) nextGen

-- 2.

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged goal = go goal 0 0 []
  where
    go :: Int -> Int -> Int -> [Die] -> StdGen -> (Int, [Die])
    go goal sum count dice gen
      | sum >= goal = (count, dice)
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
        in go goal (sum + die) (count + 1) (intToDie die : dice) nextGen