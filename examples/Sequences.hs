{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, MultiParamTypeClasses, TypeFamilies #-}
import Music.Prelude
import qualified Music.Score as S
import Numeric.Natural

-- Working title "Sequences"
--
-- TODO add harmony from 2 and 4 page sketch
-- Render using only:
--
--  * Long notes
--  * multiTempoCanon (pattern version)
--  * Single lines


main = defaultMain music

music :: Music
music =
  -- TODO proper tempo
  tempo (metronome (1 / 4) 24) $
  -- TODO proper orchestration
  set parts' violins $

  pseq $ fmap render $ fmap snd sketch

data Material v p
  = Drones [p]
  | Canon [p]
  | Line (Voice p)
  | Empty
  | Rest -- For unfilled bars
  | Sim (Material v p) (Material v p)
  deriving (Functor, Foldable, Traversable)
  -- TODO add other info from sketch, e.g. dynamics, orchestration

type instance S.Pitch (Material v p) = S.Pitch p
type instance SetPitch p' (Material v p) = Material v (SetPitch p' p)

instance HasPitches p p' => HasPitches (Material v p) (Material v p') where
  pitches = traverse . pitches

instance Semigroup (Material v p) where
  (<>) = Sim
instance Monoid (Material v p) where
  mempty = Empty

render :: Material Interval Pitch -> Music
render (Drones xs) = ppar $ fmap fromPitch xs
render (Canon xs) =
  -- TODO use multiTempoCanon (using pitches composed in sequence as subject)
  ppar $ fmap fromPitch xs
render Empty = mempty
render Rest  = rest
render (Sim a b) = render a <> render b
-- TODO proper rhythm:
render (Line xs) = level _f $ stretchTo 1 $ fromV $ fmap fromPitch xs

fromV :: (Transformable a, HasPosition a, Monoid a) => Voice a -> a
fromV = pseq . fmap (uncurry stretch) . fmap (view $ from note) . view (from voice)

v :: [Note Pitch] -> Voice Pitch
v = view voice


section :: Natural -> Material v p -> (Natural, Material v p)
section = (,)

opening =
  [ section 1 $
    Drones [fs', g', d'', a'']

  , section 2 $
    Drones [fs', g', d'', a'']
      <>
    Canon [b, cs']

  , section 3 $
    Canon [a'', g'', fs'']
      <>
    Drones [d'', g', fs']
      <>
    Canon [as, b, cs']

  , section 4 $
    Canon [a'', g'', fs'']
      <>
    Drones [d'', g', fs']
      <>
    Canon [as, b, cs']
      <>
    Canon [e, fs]

  , section 5 $
    Drones [fs'']
      <>
    Drones [fs', cs', as, fs, b_]

  , section 6 $
    Drones [fs'', e'', b', gs']
      <>
    Drones [fs', cs', b, gs, fs, b_]

  , section 7 $
    Drones [fs', cs', as, fs, b_]

  , section 8 $
    Canon [a'', g'', fs'']
      <>
    Drones [fs', cs', as, fs, b_]
      <>
    Drones [b_, fs_, cs_]
  ]


sketch :: [(Natural, Material Interval Pitch)]
sketch =
  opening
  <>
  [ section 9 $
    Canon [a'', g'', fs'']
      <>
    Drones [fs', fs, b_] <> Line (v[cs', b, fs'])
      <>
    Drones [b_, fs_]
      <>
    Canon [cs_, d_, b__]

  , section 10 $
    Line (v[fs', e'])
      <>
    Drones [b, g, cs]
    -- TODO second (delayed) Line
      <>
    Drones [e__]

  , section 11 Rest
  , section 12 Rest
  , section 13 Rest
  , section 14 Rest
  , section 15 Rest
  ]
  <>
  opening
  <>
  -- TODO
  [ section 41 $
    Drones [g, d', a']
      <>
    Line (v[fs, e, fs, e, c, c, e, fs, e, fs, e, c, b_])
  , section 41 $
    Drones [g, d', a']
      <>
    Line (v[b_,d_,c,b_,b_,d_])

  , section 41 $
    Drones [g, d', a']
      <>
    Line (v[fs, e, fs, e, c, c, e, fs, e, fs, e, c, b_])

  -- TODO 42
  , section 42 Rest
  -- TODO 43
  , section 43 Rest
  -- TODO 44
  , section 44 Rest

  -- TODO transposed version of 41, deduplicate
  , section 45 $
    up m2 (Drones [g, d', a'])
      <>
    up m2 (Line $ v $ motBLyd ++ [fs, e, fs, e, c, b_])

  -- TODO 46
  , section 46 Rest

  , section 47 $ up _M3 $
    _8va (Line $ v [eb,db,eb,db,bb_,bb_,c,c,db,eb,db,c,f,eb,db,c,db])
      <>
    Drones [gb, c, f_]

  , section 48 $ up _M3 $
    _8va (Line $ v [eb,db,eb,db,bb_,bb_,c,c,db,eb,db,c,f,eb,db,c,db])
      <>
    Drones [gb, c, f_,bb__]

  , section 49 $
    -- TODO parallel diatonic thirds below in Bb minor/Db major
    _8va (Line $ v [f,eb,db,eb,f,eb,db,eb,f,eb,eb,gb,f,eb,db,eb])
      <>
    Drones [gb, c, f_]
      <>
    Canon [bb__,a] -- TODO delay switch from drone bb to canon [bb,ab]?

  , section 50 $
    _8va (Line $ v [e,ds,e,ds,b_,b_,cs,cs,ds]) -- TODO etc
      <>
    Drones [cs,fs, b_, e_, a__]

  -- TODO
  , section 51 Rest

  -- TODO
  , section 52 Rest

  , section 53 $
    up _P12 (Line $ v motB)
      <>
    Drones [c,f,bb,d']

  -- TODO
  , section 100 $
    down _P4 (Line $ v $ motBLyd ++ motBLyd)
      <>
    Drones (down _P15 [a,d',g',c''])

  , section 101 $
    down _P4 (Line $ v $ motB)
      <>
    Drones (down _P15 [a,d',g',c''])

  -- TODO melodies of 102
  , section 102 $
    Drones [g__,d_,g_,c]
  , section 102 $
    Drones [a__,d_,g_,c]
  , section 102 $
    Drones [g__,d_,g_,c]
  , section 102 $
    Drones [a__,d_,g_,c]

  , section 103 $
    Line (v motCLong)
  ]

-- motC = [a,g,g,d]
motCLong = [a,g,g,d,d',cs',cs',b] -- TODO etc

-- TODO use these everywhere applicable
motBLyd :: [Note Pitch]
motBLyd = concat [b, [a, b] |/ 2, a, f, f, a]
motB    = concat [c, [b_,c] |/ 2, b_,g_,g_,a_,a_,b_] -- TODO etd
