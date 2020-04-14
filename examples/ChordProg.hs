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
      [f,e,c,d]
      -- [f,e,f,d,e]
      -- [e,f,g,e,d]
      -- [c,g,a,g,f]

consonant v = v /= _P4 && isConsonance v && isMelodicConsonance v

-- |
-- >>> withPrev [1..4]
-- [(1,2),(2,3),(3,4)]
withPrev :: [a] -> [(a, a)]
withPrev xs = zip xs (tail xs)

-- | First species counterpoint.
species1 :: MonadInfer m => Pitch -> [Pitch] -> m [Pitch]
species1 tonic cf = do
  -- Generate countersubject
  voice2 <- for cf (noteAbove tonic)
  -- Check countersubject is melodically consonant
  condition $ all (\(x,y) -> isMelodicConsonance (x .-. y)) $ withPrev voice2
  -- TODO check no parallel unisons/fifths/octaves
  -- Prefer countersubjects with few skips
  for (withPrev voice2) $ \(x, y) ->
    if (x == y)
      then condition =<< bernoulli 0.001
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
