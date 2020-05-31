{-# OPTIONS_GHC -Wall
  -Wcompat
  -Wincomplete-record-updates
  -Wincomplete-uni-patterns
  -Werror
  -fno-warn-name-shadowing
  -fno-warn-unused-matches
  -fno-warn-missing-signatures
  -fno-warn-unused-imports #-}
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
  | LineHarm [(Voice p, [p])]
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
render (Drones xs) = renderHarm xs
render (Canon xs) =
  -- TODO use multiTempoCanon (using pitches composed in sequence as subject)
  ppar $ fmap fromPitch xs
render Empty = mempty
render Rest  = rest
render (Sim a b) = render a <> render b
-- TODO proper rhythm:
render (Line xs) = renderMel xs
render (LineHarm xs) = stretchTo 1 $ pseq $ fmap (\(mel, harm) -> renderMel mel <> renderHarm harm) xs

renderMel :: Voice Pitch -> Music
renderMel xs = level _f $ stretchTo 1 $ fromV $ fmap fromPitch xs

renderHarm :: [Pitch] -> Music
renderHarm xs = ppar $ fmap fromPitch xs

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
  -- A1 section
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

  , section 11 $ Drones [e__]
  , section 12 $ Drones [e__]
  , section 13 $ Drones [fs__]
  , section 14 $ Drones [fs__]
  , section 15 $ Drones [fs__]
  ]
  <>
  opening
  <>
  [ section 24 $
    Drones [cs', fs, b_]
      <>
    _8vb (Canon [cs, ds, g, f])
      <>
    Drones [b__,b___]

  , section 25 $
    -- TODO what?
    Drones [b__,b___]

  , section 26 $
    Drones [cs', fs, b_]
      <>
    _8vb (above _M3 $ Canon [g, f, cs, ds])
      <>
    Drones [b__,b___]

  , section 27 $
    -- TODO what?
    Drones [b__,b___]

  , section 28 $
    Drones [cs']
      <>
    Drones [b__,b___]

  , section 29 $
    Drones [cs']
      <>
    Drones [b__,b___]

  , section 30 $
    Drones [gs',cs']
      <>
    Drones [b__,b___]

  , section 31 $
    Drones [as'', ds'', gs',cs']
      <>
    Canon [fs, e]
      <>
    Drones [b__,b___]

  , section 32 $
    -- TODO above composes in seq, not in par
    Line (above _P8 $ down m2 $ v motC)
      <>
    Drones [b__,b___]

  , section 33 $ Drones [b__,b___]

  , section 34 $ Drones [b__,b___]

  , section 35 $ Drones [b__,b___]

  , section 36 $
    -- TODO above composes in seq, not in par
    Line (above _P8 $ down m2 $ v motC)
      <>
    Drones [b__,b___]

  , section 37 $ Drones [b__,b___]

  , section 38 $ Drones [b__,b___]

  , section 39 $ Drones [b__,b___]

  , section 40 $ Drones [b__,b___]
  ]
  <>
  -- B1 section
  [ section 41 $
    Drones [g, d', a']
      <>
    Line (v[fs, e, fs, e, c, c, e, fs, e, fs, e, c, b_])
  , section 41 $
    Drones [g, d', a']
      <>
    Line (v[b_,d_,c,b_,b_,d_])

  , section 42 $
    Drones [g, d', a']
      <>
    Line (v[fs, e, fs, e, c, c, e, fs, e, fs, e, c, b_])

  , section 43 $
    Drones [c'',f',bb]
      <>
    Line (up d5 $ v motA) -- TODO long version of motA

  , section 44 $
    Drones [c'',f',bb]
    -- TODO line

  -- TODO transposed version of 41, deduplicate
  , section 45 $ up m2 $
    (Drones [g, d', a'])
      <>
    (Line $ v $ motALyd ++ [fs, e, fs, e, c, b_])

  -- TODO transposed version of 42, deduplicate
  -- TODO 46
  , section 46 $ up m2 $
    Drones [g, d', a']
      <>
    Line (v[fs, e, fs, e, c, c, e, fs, e, fs, e, c, b_])

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

  , section 51 $
    Drones [d', g, d, a__]

  , section 52 $
    Drones [c',g]

  , section 53 $
    up _P12 (Line $ v motA) -- TODO longish
      <>
    Drones [c,f,bb,d']

  , section 54 $
    up _P4 (Line $ v motALyd) -- TODO long
      <>
    Drones [g,c,f_]

  -- TODO this the [bb,a] layer here should be *unison* (e.g. not a canon), but
  -- still alternate at a pace unsynchronized with the main line
  , section 55 $
    Line (v motC)
      <>
    Drones [c,f_,bb__]
  , section 55 $
    Line (v motC)
      <>
    Drones [c,f_,a__]
  , section 55 $
    Line (v motC)
      <>
    Drones [c,f_,bb__]
  , section 55 $
    Line (v motC)
      <>
    Drones [c,f_,a__]

  , section 56 $
    Line (_8va $ v [{-TODO rest-}e,a,e',d',fs',e])
      <>
    Drones [g, c, d_]
  , section 56 $
    Canon [g, fs]
      <>
    Drones [c, g_, d_]
  , section 56 $
    Canon [g, fs]
      <>
    Drones [bb_, d_]

  , section 57 $
    Drones [g,c,d_]

  , section 58 $
    Drones [a,e,b_,f_]

  , section 59 $
    Drones [bb,f,c]

  , section 60 $
    Line (v [d,db])

  , section 61 $
    Drones [eb',bb,f]

  , section 62 $
    Drones [f__,f___]

  , section 63 $
    Drones [bb,f,c_]
  , section 63 $
    Drones [a,f,c_]

  , section 64 $
    Drones [bb,f,c_]
  , section 64 $
    Drones [a,f,c_]

  , section 65 $
    Drones [d,c_]

  , section 66 $
    Drones [f__,f___]

  , section 67 $
    Drones [f__,f___]

  , section 68 $
    Drones [f__,f___]

  , section 69 $
    Drones [f__,f___]

  , section 70 $
    Drones [f__,f___]

  -- C section
  , section 71 $
    Drones [f'', bb', eb']
  , section 71 $
    Drones [f'', bb', eb', ab]

  , section 72 $
    Drones [f'', bb', eb', ab]
      <>
    Drones [gb__]

  , section 73 $
    Drones [ab__]

  , section 74 $
    Drones [ab__]


  , section 75 $
    Drones [c]

  , section 76 $
    Drones [d]

  , section 77 $
    Drones [fs]

  , section 78 $
    Drones [fs]

  , section 79 $
    Line (v [cs,b_,a_,fs_,gs_])

  , section 80 $
    Drones [g', cs', fs, d, gs_]
  ]
  -- RECAP
  <>
  opening
  <>
  -- B2
  [ section 100 $
    down _P4 (Line $ v $ motALyd ++ motALyd)
      <>
    Drones (down _P15 [a,d',g',c''])

  , section 101 $
    down _P4 (Line $ v $ motA)
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
    LineHarm motBLongHarm
      <>
    Drones [g__,g___]
  , section 103 $
    Line (v[a,g,g,d,d,d',d',cs', b,e',e',d',d',cs',cs',b]) -- TODO etc

  , section 104 $
    Line (v [e',d',d',a, c',b,b,g, g,a,a,b]) -- TODO etc

  , section 105 $
    down _P4 (LineHarm motBLongHarm)
      <>
    Drones [d__,d___]

  , section 106 $
    -- TODO line
    Drones [d__,d___]

  , section 107 $
    down m7 $ Line (v motCToV)

  , section 108 $
    -- TODO line
    Drones [e',a]
  , section 108 $
    -- TODO line
    Drones [e',gs]

  , section 109 $
    -- TODO line
    Drones [ds',gs]
  , section 109 $
    -- TODO line
    Drones [gs]

  , section 110 $
    -- TODO line
    Drones [fs,c_]
  , section 110 $
    -- TODO line
    Drones [fs, d, g_]

  , section 111 $
    Line (v motALyd)
      <>
    Drones [f,c,g_]

  , section 112 $
    Line (v motC)
      <>
    Drones [c_,c__]

  , section 113 $
    -- TODO line
    Drones [c_,c__]

  , section 114 $
    Line (v motC)
      <>
    Drones [c_,c__]

  , section 115 $
    LineHarm [(v motB,[d,bb_]),(v [d',c',c',g],[e])]
      <>
    Drones [g__]

  , section 116 $
    LineHarm [(v motB,[d,bb_]),(v [d',c',c',g],[e])]
      <>
    Drones [g__]

  , section 117 $
    -- TODO line
    Drones [g__]

  , section 118 $
    LineHarm [(up _P4 $ v motB,[d,bb_]),(mempty,[e]),(mempty,[d])]
      <>
    Drones [g__]

  , section 119 $
    LineHarm [(up _P4 $ v motB,[e]),(mempty,[d]),(mempty,[e]),(mempty,[d])]
      <>
    Drones [g__]

  -- CODA
  , section 120 $
    LineHarm [(v[f',d',d',bb], [bb,eb])
             ,(v[d',bb,bb,g], [g,eb])
             ,(v[c',a,a,f], [a,f,bb_])
             ]
      <>
    Drones [d, g_,c__]

  , section 121 $
    -- TODO lines
    Drones [d, g_,c_]

  , section 122 $
    Drones [d, g_,bb__]

  , section 123 $
    Drones [d, g_,c_]

  , section 124 $
    Drones [d, g_,bb__]

  , section 125 $
    Drones [c__]

  , section 126 $
    Drones [g__]

  , section 127 $
    Drones [c__]

  , section 128 $
    Drones [g__]

  , section 129 $
    Drones [fs', g', d'', a'']
  ]




-- TODO use these everywhere applicable
motALyd :: [Note Pitch]
motALyd = concat [b, [a, b] |/ 2, a, f, f, a]

motA :: [Note Pitch]
motA    = concat [c, [b_,c] |/ 2, b_,g_,g_,a_,a_,b_] -- TODO etd

motB :: [Note Pitch]
motB = [a,g,g,d]

motBLong :: [Note Pitch]
motBLong = [a,g,g,d, d',cs',cs',b] -- TODO etc

motBLongHarm = [(v[a,g,g,d], [d,b_]), (v[d',cs',cs',b], [a,e])] -- TODO etc

motC :: [Note Pitch]
motC = [f,e,a,g,e' |* 4]

motCToV:: [Note Pitch]
motCToV = [f,e,a,g,g' |* 4]
