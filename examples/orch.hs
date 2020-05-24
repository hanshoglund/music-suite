{-# LANGUAGE TypeFamilies #-}
{-
 - Orchestra piece
 -
 - TODO assemble material to be used
 -}

import Music.Prelude
import qualified Music.Score as S
import qualified Ex.StringTexture
import qualified P.SustainPunctuated.Score
import qualified P.WindsPhrasing.Score
import qualified P.BrassLargeEnsemble.Score
import qualified Ex.Sky
import qualified Ex.Voicing

main = defaultMain music

multiTempoCanon
  :: (IsPitch a, HasParts' a, S.Part a ~ Part)
  => [(Part, Interval, Span)]
  -- Entries (part, transposition, transformtion (phase/stretch)
  -> Voice Pitch
  -- ^ Subject
  -> Pattern a
multiTempoCanon entries subj = mconcat $ fmap
  (\(p,i,t) -> newPattern $ transform t $ set parts' p $ fmap fromPitch $ up i subj) entries

-- | Like multiTempoCanon but without repetition.
multiTempoCanon'
  :: (IsPitch a, HasParts' a, S.Part a ~ Part)
  => [(Part, Interval, Span)]
  -- Entries (part, transposition, transformtion (phase/stretch)
  -> Voice Pitch
  -- ^ Subject
  -> Score a
multiTempoCanon' entries subj = mconcat $ fmap
  (\(p,i,t) -> renderAlignedVoice $ transform t $ aligned 0 0 $ set parts' p $ fmap fromPitch $ up i subj) entries

-- TODO make most/all of these into functions
-- Explore variations of *similar* material
-- Edit/manipulate parameters to turn into some kind of logical sequence
--
-- TODO transcribe more from manual notes. Try to make everything into functions
-- (find some parameter to vary)
--
-- TODO musicalize *simple* data
-- For example Le Sacre "Dances of the Young Girls" is a "musicalization" of [Bool]
music =
  times 1 $ pseq
  [ P.SustainPunctuated.Score.music

  , Ex.StringTexture.music


  -- TODO more patterns (a la Interludes)

  -- TODO more floaters (a la Mist)

  -- TODO multitempo canons (a la Passages/Imitations)

  -- TODO large-scale Behavior (a la Shadowings/Layers)

  , Ex.Sky.music

  -- TODO palindrome phased patterns, a la Calliano "When I Dream"
  --  [[Pitch]] -> Pattern (Part, Pitch)

  -- TODO contrary motion of chords, a la Tan Dun "Map"
  --  { origChord :: Chord Interval Pitch
  --  , invChord  :: Chord Interval Pitch
  --  , tune      :: Voice Pitch
  --  } -> Pattern (Part, Pitch)

  -- TODO make proper chords, orchestrate
  , Ex.Voicing.music

  , P.WindsPhrasing.Score.music

  , arrangeFor (divide 3 trumpets ++ divide 4 horns ++ divide 2 trombones)
    $ P.BrassLargeEnsemble.Score.music
  ]

