{-# LANGUAGE TypeFamilies, NamedFieldPuns #-}
{-
 - Orchestra piece
 -
 - TODO assemble material to be used
 -}

import Music.Prelude
import qualified Data.Foldable
import qualified Music.Score as S
import qualified Ex.StringTexture
import qualified P.SustainPunctuated.Score
import qualified P.WindsPhrasing.Score
import qualified P.BrassLargeEnsemble.Score
import qualified Ex.Sky
import qualified Ex.Voicing
import qualified Data.List

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

applyBehavior :: Behavior (a -> b) -> Score a -> Score b
applyBehavior b = mapWithSpan (\s x -> b ! view onset s $ x)

-- Utility for showing a behavior
-- Note this shows the range [0..1] in the span (0 <-> 1), use transform/fmap
-- to "zoom"/"move"
drawBehavior :: RealFrac a => Behavior a -> IO ()
drawBehavior = fmap (const ()) . traverse print . drawBehavior'

drawBehavior' :: RealFrac a => Behavior a -> [[Char]]
drawBehavior' b = fmap (\x ->
    flip replicate 'x' $ floor (( b ! (x / numSamples)) * maxBars))
  [0..numSamples]
  where
    numSamples = 15
    maxBars = 33


data ChordMotion v p = ChordMotion
  { origChord :: Voiced Chord v p
  , invChord  :: Voiced Chord v p
  , tune      :: Voice p
  }
  deriving (Eq, Ord, Show)


-- TODO transcribe more from manual notes. Try to make everything into functions
-- (find some parameter to vary). Examples:
--
-- TODO a la Stravinsky
--  (IsPitch a, HasParts' a, GetPart a ~ Part) => [Bool] -> Pattern a

-- TODO a la S. Adler "Guitar Concerto"
--  data StrumType = Strum | BassStrum | NoStrum
--  (IsPitch a, HasParts' a, GetPart a ~ Part) => [StrumType] -> Pattern a

-- TODO a la "Incandescence"
--  (IsPitch a, HasParts' a, GetPart a ~ Part) => [Vec 3 Pitch] -> Pattern a

-- TODO a la "Bauer 1918"
--  (IsPitch a, HasParts' a, GetPart a ~ Part) => [(Interval, Integer)] -> Pattern a

-- TODO a la "When I Dream"
--  (IsPitch a, HasParts' a, GetPart a ~ Part) => [[Pitch]] -> Pattern a

-- TODO add orchestration
-- TODO allow other types of inversion (e.g. diatonic)
chordMotion :: ChordMotion Interval Pitch -> Pattern Pitch
chordMotion ChordMotion{ origChord, invChord, tune } =
  (mconcat $ fmap newPattern $ homoToPolyphonic $ fmap
    (\p -> Data.Foldable.toList $ getVoiced $ up (p .-. c) origChord)
    tune)
    <>
  (mconcat $ fmap newPattern $ homoToPolyphonic $ fmap
    (\p -> Data.Foldable.toList $ getVoiced $ up (p .-. c) invChord)
    (invertVoice tune))

invertVoice :: Voice Pitch -> Voice Pitch
invertVoice x = case lowestPitch x of
  Nothing -> x
  Just l -> invertPitches l x

newtype Floater a = Floater [(Alignment, Voice a)]

-- TODO canwe also render this into a pattern?
renderFloater :: Floater a -> Score a
renderFloater (Floater xs) = mconcat $
  fmap (renderAlignedVoice . uncurry (aligned 0)) xs

-- TODO make most/all of these into functions
-- Explore variations of *similar* material
-- Edit/manipulate parameters to turn into some kind of logical sequence
--
music =
  times 1 $ pseq
  [ mempty

  -- TODO use spread-out "randomly occuring" events, as in the beginning
  -- of "Circue Glacier"

  -- TODO more floaters (a la Mist)
  -- TODO pad!
  , set parts' violins $ renderFloater $ Floater
    [ (0, pure c |* 2)
    , (0, pure e |* 2.1)
    , (0, pure g |* 3)
    ]

  , flip renderPattern (0 <-> 30) $ fmap fromPitch $ chordMotion $ ChordMotion
    { origChord = voiceIn 4 $ chord c dominantSeventhChord
    , invChord  = voiceIn 2 $ chord c majorTriad
    , tune = [c,d,e,g,d |* 2]^.voice
    }

  , multiTempoCanon'
    [ (divide 2 cellos !! 0, _P5,        4 >-> (2/3))
    , (divide 2 cellos !! 1, _P1,        2 >-> 1)
    , (divide 2 doubleBasses !! 0, -_P4, 1 >-> 1.5)
    , (divide 2 doubleBasses !! 1, -_P8, 0 >-> 2)
    ]
    (view voice [c,d,e|*2,c,d,d,e|*1.5,e,f,d,e,c|*2,d|*2])

  -- TODO to use thirds we might want diatonic transposition
  -- instead of real
  , flip renderPattern (0 <-> 30) $ multiTempoCanon
    [ (divide 2 violas !! 0, _P8+_P5,        4 >-> (5/6))
    , (divide 2 cellos !! 0, _P5,        4 >-> (2/3))
    , (divide 2 cellos !! 1, _P1,        2 >-> 1)
    , (divide 2 doubleBasses !! 0, -_P4, 1 >-> 1.5)
    , (divide 2 doubleBasses !! 1, -_P8, 0 >-> 2)
    ]
    (view voice [c,d,e|*2,c,d,d,e|*1.5,e,f,d,e,c|*2,d|*2])

  -- TODO more patterns (a la Interludes)


  , P.SustainPunctuated.Score.music

  , Ex.StringTexture.music

  -- TODO large-scale Behavior (a la Shadowings/Layers)
    -- E.g. use different parameters to control
    --   * Pitch material (and ambitus)
    --   * Loudness
    --   * Timbre?
    --   * Type of pattern
    --      (e.g. number of notes per dur 1 in a rising/falling scale or nothing)
    --   * Or something like "Become Ocean" b. 109 cellos
    --    Note: in B.O. there are two layers per "chorus", one "slow" and one "fast"
    --      The tempo relation for the fast layer is 5:6:7 (winds, brass, strings)
    --      The slow layer plays strictly inside the grid generated by the fast layer


  , Ex.Sky.music

  -- TODO make proper chords, orchestrate
  , Ex.Voicing.music

  , P.WindsPhrasing.Score.music

  , arrangeFor (divide 3 trumpets ++ divide 4 horns ++ divide 2 trombones)
    $ P.BrassLargeEnsemble.Score.music
  ]


