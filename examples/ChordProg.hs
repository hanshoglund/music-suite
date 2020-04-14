{-# LANGUAGE TypeApplications #-}
import Music.Prelude hiding (score)
import Data.Traversable
import qualified Data.List
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Enumerator (normalForm)

main = defaultMain music

music = inspectableToMusic @[Voice Pitch] $
  [ view voice $ fmap fromPitch $ fst $ last $
    Data.List.sortOn snd $ normalForm $ species1 c cf
  , view voice $ fmap fromPitch $ cf
  ]
  where
    cf =
      [c,d,e,c,d]
      -- [f,e,c,d]
      -- [f,e,f,d,e]
      -- [e,f,g,e,d]
      -- [c,g,a,g,f]
      -- [b_,d,e,d,g,f]

consonant :: Interval -> Bool
consonant v = v /= _P4 && isConsonance v && isMelodicConsonance v

parallelsForbidden :: Interval -> Bool
parallelsForbidden v = v `elem` [_P5, _P1, _P8]

-- | Check if two melodic intervals approach in the same direction.
-- Returns False if at least one of the intervals is a perfect unison.
isParallel :: Interval -> Interval -> Bool
isParallel v w = if isPerfectUnison v || isPerfectUnison w
  then False
  else isPositive v == isPositive w

-- |
-- >>> withPrev [1..4]
-- [(1,2),(2,3),(3,4)]
withPrev :: [a] -> [(a, a)]
withPrev xs = zip xs (tail xs)

-- | First species counterpoint.
species1 :: MonadInfer m => Pitch -> [Pitch] -> m [Pitch]
species1 tonic cf = do
  -- Generate harmonically consonant countersubject
  voice2 <- for cf (noteAbove tonic)
  -- Check countersubject is melodically consonant
  condition $ all (\(x,y) -> isMelodicConsonance (x .-. y)) $ withPrev voice2
  -- Check no parallel unisons/fifths/octaves
  for (withPrev $ zip cf voice2) $ \((cf0, cs0),(cf1, cs1)) ->
    when (parallelsForbidden (cs1 .-. cf1)) $
       condition $ not $ isParallel (cf1 .-. cf0) (cs1 .-. cs0)
  -- Prefer countersubjects with few skips
  for (withPrev voice2) $ \(x, y) ->
    if (x == y)
      then condition =<< bernoulli 0.1
      else if isStep (x .-. y)
        then condition =<< bernoulli 0.9
        else condition =<< bernoulli 0.05
  pure voice2

-- | Add a note above given cantus firmus, relative tonic.
noteAbove :: MonadInfer m => Pitch -> Pitch -> m Pitch
noteAbove tonic = \x -> do
    n <- uniformD [1..7]
    let y = upDiatonicP tonic n x
    condition $ consonant (y .-. x)
    pure y
