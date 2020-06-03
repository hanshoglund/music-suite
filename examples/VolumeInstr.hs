{-# LANGUAGE TypeFamilies #-}

-- Useful to calibrate volume levels
module Main where

import Music.Prelude
import qualified Music.Score

main :: IO ()
main = defaultMain music

music :: Music
music = timeSignature (3/2) $ pseq $ fmap (\x -> level x kf) $
  (\x -> x <> reverse x) [ppp, pp, _p, mp, mf, _f, ff, fff]

kf =
  compress 4 $ renderAlignedVoice $ aligned 0 0 $
    klangfarben
      ( cycle
          [ violins,
            violas,
            cellos,
            doubleBasses,
            clarinets,
            trombones
          ]
      )
      (view voice $ [c,c,c,c,c,c])

klangfarben :: HasParts' a => [Music.Score.Part a] -> Voice a -> Voice a
klangfarben ps v = (^. voice) $ zipWith (set parts') ps (v ^. notes)
