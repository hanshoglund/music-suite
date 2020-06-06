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
import Data.Foldable (toList)
import qualified Music.Score as S
import Numeric.Natural
import Ex.OrchTextures (cut, multiTempoCanon, multiTempoCanon')

-- Working title "Sequences" (or "2020"?)
--
-- TODO add harmony from 2 and 4 page sketch
-- Render using only:
--
--  * Long notes
--  * multiTempoCanon (pattern version)
--  * Single lines

main :: IO ()
main = defaultMain music

music :: Music
music =
  -- TODO proper tempo
  tempo (metronome (1 / 4) 48) $
  -- TODO proper orchestration

  pseq $ fmap render $ fmap snd sketch

data Material v p
  = Drones [p]
  -- ^ Sustained throughout the section (long notes). Duration 4.
  | Canon [p]
  -- ^ Played in sequence througout the section as a multi-tempo canon. Duration 5.

  | FlexDrones [p]
  -- TODO like Drones, but stretched to fit the duration of line material
  | FlexCanon [p]
  -- TODO like Canon, but stretched to fit the duration of line material

  | Line (Maybe Part) (Voice p)
  -- ^ A single melodic line
  | LineT (Maybe Part) Span (Voice p)
  -- ^ A single melodic line with a transformation
  | LineHarm [(Voice p, [p])]
  -- ^ A single melodic line with accompanying harmony

  | Empty
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


data Flex = Flex | NoFlex
data MaterialG p
  = DronesG Flex [p]
  | CanonG Flex [p]
  | LineG (Maybe Part) (Maybe Span) (Voice p)
  | LineHarmG [(Voice p, [p])]
foo :: Material v p -> [MaterialG p]
foo = go
  where
    go Empty = []
    go (Sim x y) = go x ++ go y
    go (Drones xs) = [DronesG NoFlex xs]
    go (Canon xs) = [CanonG NoFlex xs]
    go (FlexDrones xs) = [DronesG Flex xs]
    go (FlexCanon xs) = [CanonG Flex xs]
    go (Line p v) = [LineG p Nothing v]
    go (LineT p t v) = [LineG p (Just t) v]
    go (LineHarm ts) = [LineHarmG ts]


-- | Simple rendering, looking a bit like the original sketch.
renderSimple :: Material Interval Pitch -> Music
renderSimple Empty = mempty
renderSimple (Sim a b) = renderSimple a <> renderSimple b
renderSimple (Drones xs) = renderHarmSimple xs
renderSimple (Canon xs) =
  ppar $ fmap fromPitch xs
renderSimple (FlexDrones xs) = renderSimple (Drones xs)
renderSimple (FlexCanon xs) = renderSimple (Canon xs)
renderSimple (Line _p xs) = renderMelSimple xs
renderSimple (LineT _p _t xs) = renderMelSimple xs
renderSimple (LineHarm xs) = stretchTo 1 $ pseq $ fmap (\(mel, harm) -> renderMelSimple mel <> renderHarmSimple harm) xs

renderMelSimple :: Voice Pitch -> Music
renderMelSimple xs = level _f $ stretchTo 1 $ fromV $ fmap fromPitch xs

renderHarmSimple :: [Pitch] -> Music
renderHarmSimple xs = ppar $ fmap fromPitch xs



safeMax :: Ord a => [Maybe a] -> Maybe a
safeMax xs = case concat $ fmap toList xs of
  [] -> Nothing
  xs -> Just (maximum xs)

-- |
-- >>> roundTo1 3
-- 3
--
-- >>> roundTo1 3.2
-- 4
roundTo1 :: Duration -> Duration
roundTo1 d = let (_q,r) = d `divModDur` 1 in
  if r > 0 then d + (1 - r) else d

render :: Material Interval Pitch -> Music
render = go . foo
  where
    -- | Render a single parallel composition of materials.
    go :: [MaterialG Pitch] -> Music
    go xs = let d = safeMax (fmap dur xs)
      in ppar $ fmap (renderAtDur d) xs

    renderAtDur :: Maybe Duration -> MaterialG Pitch -> Music
    renderAtDur md (DronesG flex xs) =
      let dur =
            case (md, flex) of
              (Just d, Flex) -> roundTo1 d
              _ -> 4
      in
        set parts' violins $
        stretch dur $
        renderHarm xs
    renderAtDur md (CanonG flex xs) =
      let endPoint =
              case (md, flex) of
                (Just d, Flex) -> 0 .+^ d
                -- Use duration from context
                _ -> 5
                -- Arbitrary fixed duration
      in
        -- TODO if there's more than one canon per Sim, merge them all
        -- before rendering (e.g. Canon should be a monoid homomorphism)
        -- TODO other spans
        --- TODO other aprts than strings!
        -- TODO other phases?
        flip renderPattern (0 <-> endPoint) $ multiTempoCanon
          (zip3 (cycle $ stringOrchestra ++ [doubleBasses]) (repeat _P1)
            (zipWith (<->) (repeat 0) [10/8, 13/8, 15/8, 17/8, 21/8]))
          -- TODO use durations other than 1 in the pattern melody
          (v $ fmap pure xs)
    renderAtDur _ (LineG mp mt v) = maybe id transform mt $
      set parts' (maybe violins id mp) $
      renderMel v
    renderAtDur _ (LineHarmG vs) =
      set parts' violins $
      -- TODO this should NOT use stretchTo, but behave like LineG with sequential composition
      stretchTo 1 $ pseq $ fmap (\(mel, harm) -> renderMel mel <> renderHarm harm) vs

    dur :: MaterialG a -> Maybe Duration
    dur (DronesG _ _) = Nothing
    dur (CanonG _ _) = Nothing
    dur (LineG _ mt v) = Just $ maybe id transform mt $ stretch (1/8) $ _duration v
    dur (LineHarmG vs) = Just $ sum $ fmap (_duration . fst) vs


-- TODO pad with rests at end to fill an even number of 4/4 bars?
renderMel :: Voice Pitch -> Music
renderMel xs = stretch (1/8) $ fromV $ fmap fromPitch xs

renderHarm :: [Pitch] -> Music
renderHarm xs = ppar $ fmap fromPitch xs

fromV :: (Transformable a, HasPosition a, Monoid a) => Voice a -> a
fromV = pseq . fmap (uncurry stretch) . fmap (view $ from note) . view (from voice)

v :: [Note Pitch] -> Voice Pitch
v = view voice


section :: Natural -> Material v p -> (Natural, Material v p)
section = (,)

section_A1 =
  [ section 1 $
    Drones [fs', g', d'', a'']

  , section 2 $
    Drones [fs', g', d'', a'']
      <>
    Canon [b, cs']

  , section 3 $
    Canon [a'', g'', fs'', as, b, cs']
      <>
    Drones [d'', g', fs']

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
      <>
    Line (Just $ solo violin)
      subjA
    -- TODO more lines/canons here

  , section 6 $
    Drones [fs'', e'', b', gs']
      <>
    Drones [fs', cs', b, gs, fs, b_]
      <>
    LineT (Just $ solo violin) (delaying 0.5)
      (up _P12 subjA)
    -- TODO more lines/canons here

  , section 7 $
    Drones [fs', cs', as, fs, b_]
    -- TODO abstract this 'echo' pattern
      <>
    Line (Just $ tutti corAnglais)
      subjX
      <>
    LineT (Just bassoons) (delaying (7/8)) (down _P5 subjX)
      <>
    LineT (Just oboes) (delaying (17/8)) (up _P4 subjX)

  , section 8 $
    Canon [a'', g'', fs'']
      <>
    Drones [fs', cs', as, fs, b_]
      <>
    Drones [b_, fs_, cs_]
  ]

section_A2A =
  -- A2a
  [ section 9 $
    FlexCanon [a'', g'', fs'']
      <>
    FlexDrones [fs', fs, b_]
      <>
    FlexDrones [b_, fs_]
      <>
    FlexCanon [cs_, d_, b__]
      <>
    -- TODO louder, slower and aligned to end
    Line Nothing (v[cs', b, fs']|*(8*2/3))

  , section 10 $
    -- TODO louder, slower
    Line Nothing (v[fs', e']|*8)
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

section_A2B =
  -- TODO throughout: fs,gs vs f,g (WT around B vs quartal around B)
  [ section 24 $
    Drones [cs', fs, b_]
      <>
    _8vb (Canon [cs, ds, g, f])
      <>
    Drones [b__,b___]

  , section 25 $
    _8vb (above _M3 $ Canon [g, f, cs, ds])
      <>
    Drones [b__,b___]

  , section 26 $
    Drones [cs', fs, b_] -- TODO thin out drones here?
      <>
    _8vb (above _M3 $ Canon [g, f, cs, ds])
      <>
    Drones [b__,b___]


  -- 27 cut

  , section 28 $
    Drones [cs']
      <>
    _8vb (above _M3 $ Canon [g, f, cs, ds])
      <>
    Drones [b__,b___]

  , section 29 $
    LineT Nothing (delaying 0) (up d5 $ subjX) -- TODO transpose?
      <>
    LineT Nothing (delaying 1) (up d5 $ v motA) -- TODO transpose?
      <>
    Drones [cs']
      <>
    Drones [b__,b___]

  , section 30 $
    LineT Nothing (delaying 0) (up d5 $ subjX) -- TODO transpose?
      <>
    LineT Nothing (delaying 1) (up d5 $ v motA) -- TODO transpose?
      <>
    Drones [gs',cs']
      <>
    Drones [b__,b___]

  , section 31 $
    Drones [as'', ds'', gs',cs']
      <>
    Canon [fs, e]
      <>
    Drones [b__,b___]

  , section 33 $ Drones [b__,b___]
      <>
    Canon [g,fs,e]

  , section 32 $
    above _P8 (Line Nothing (down m2 $ stretch 2 $ v motC))
      <>
    Line (Just horns) (v [fs] |* 16)
      <>
    FlexDrones [b__,b___]

  , section 34 $ Drones [b__,b___]
      <>
    Canon (reverse [e,fs,g,cs'])

  -- 35 cut

  , section 36 $
    above _P8 (Line Nothing (down m2 $ stretch 2 $ v motC))
      <>
    Line (Just horns) (v [fs,e] |* 16)
      <>
    FlexDrones [b__,b___]

  , section 37 $
    above _P8 (Line Nothing (stretch 2 $ v [ds' |* 2]))
      <>
    Line (Just horns) (v [fs,e] |* 16)
      <>
    Line (Just horns) (v [ds,cs] |* 16)
      <>
    Drones [b__,b___]

  -- , section 38 $ Drones [b__,b___]

  -- , section 39 $ Drones [b__,b___]

  -- , section 40 $ Drones [b__,b___]
  ]

section_B1 =
  -- B1 section
  [ section 41 $
    FlexDrones [g, d', a']
      <>
    Line (Just violas) (v[fs, e|/2, fs|/2, e, c, c, e, fs, e|/2, fs|/2, e, c])
  , section 41 $
    FlexDrones [g, d', a']
      <>
    Line (Just cellos) (v[c,b_,d_,c,b_,b_,d_])

  , section 42 $
    FlexDrones [g, d', a']
      <>
    Line (Just violas) (v[fs, e|/2, fs|/2, e, c, c, e, fs, e|/2, fs|/2, e, c])

  , section 43 $
    FlexDrones [c'',f',bb]
      <>
    Line Nothing (up d5 $ v motA) -- TODO long version of motA

  , section 44 $
    FlexDrones [c'',f',bb]
      <>
    Line (Just oboes) (_8va $ v [f,eb,eb,db,db,c,c,db,eb,db,db,c,c,bb_,bb_,c])

  -- TODO transposed version of 41, deduplicate
  -- TODO wrong rhythm!
  , section 45 $ up m2 $
    (FlexDrones [g, d', a'])
      <>
    (Line (Just horns) $ v $ motALyd ++ []) -- TODO

  -- TODO transposed version of 42, deduplicate
  -- TODO wrong rhythm!
  , section 46 $ up m2 $
    FlexDrones [g, d', a']
      <>
    Line (Just bassoons) (v[fs, e|/2, fs|/2, e, c, c, e, fs, e, fs, e, c, b_])

  -- TODO wrong rhythm!
  , section 47 $ up _M3 $
    _8va (Line (Just oboes) $ v [eb,db|/2,eb|/2,db,bb_,bb_,c,c,db,eb,db,c,f,eb,db,c,db])
      <>
    FlexDrones [gb, c, f_]

  -- TODO wrong rhythm!
  , section 48 $ up _M3 $
    -- TODO fl+cl
    _8va (Line (Just flutes) $ v [eb,db|/2,eb|/2,db,bb_,bb_,c,c,db,eb,db,c,f,eb,db,c,db])
      <>
    FlexDrones [gb, c, f_,bb__]

  , section 49 $
    _8va (Line Nothing $ v [f,eb|/2,db|/2,eb,f,eb,db,eb,f,eb,eb,gb,f,eb,db,eb])
      <>
    FlexDrones [gb, c, f_]
      <>
    Canon [bb__,a__] -- TODO delay switch from drone bb to canon [bb,ab]?

  -- TODO wrong rhythm!
  , section 50 $
    _8va (Line Nothing $ v [e,ds|/2,e|/2,ds,b_,b_,cs,cs,ds]) -- TODO etc
      <>
    FlexDrones [cs',fs, b_, e_, a__]

  , section 51 $
    FlexDrones [d', g, d, a__]
      <>
    Line (Just horns) (up _M2 $ v motALyd)

  , section 52 $
    FlexDrones [c',g]

  , section 53 $
    up _P12 (Line (Just ebClarinets) $ v motA) -- TODO longish
      <>
    FlexDrones [c,f,bb,d']

  , section 54 $
    up _P4 (Line Nothing $ v motALyd) -- TODO long
      <>
    FlexDrones [g,c,f_]

  -- TODO this the [bb,a] layer here should be *unison* (e.g. not a canon), but
  -- still alternate at a pace unsynchronized with the main line
  , section 55 $
    Line Nothing (v motCToI <> _8va (v motCVar))
      <>
    FlexDrones [c,f_,bb__]
  , section 55 $
    Line Nothing (v motCToI <> _8va (v motCVar))
      <>
    FlexDrones [c,f_,a__]
  , section 55 $
    Line Nothing (v motCToI <> _8va (v motCVar))
      <>
    FlexDrones [c,f_,bb__]
  , section 55 $
    Line Nothing (v motCToI <> _8va (v motCVar))
      <>
    FlexDrones [c,f_,a__]

  , section 56 $
    LineT Nothing (delaying (1/8)) (_8va $ v [{-TODO rest-}e,a,e',e',d',fs',e'])
      <>
    FlexDrones [g, c, d_]
  , section 56 $
    Canon [g, fs]
      <>
    FlexDrones [c, g_, d_]
  , section 56 $
    LineT Nothing (delaying (1/8)) (_8va $ v [{-TODO rest-}e,a,e',e',d',fs',e'])
      <>
    Canon [g, fs]
      <>
    FlexDrones [c, g_, d_]
  , section 56 $
    LineT Nothing (delaying (1/8)) (_8va $ v [{-TODO rest-}a_,d,a,d',c',e',d'])
      <>
    Canon [g, fs]
      <>
    Drones [bb_, d_]

  , section 57 $
    FlexDrones [g,cs,d_]
      <>
    (below _M3 $ Line Nothing $ v $
      let x = [b,a,a,f,f,g,g,a]
      in mconcat [x, x, [cs',b,b,g,g,a,a,b],
      [cs',b,b,g],
      [cs',b,b,g],
      [cs',b,b,g] |* 2
      ]) -- TODO etc

  , section 58 $
    Line Nothing (v $ concat [a,[g,e]|/2, e',d', a,g,g,d,a',g',g',d'])
      <>
    Drones [a,e,b_,f_]

  , section 59 $
    Line Nothing (v $ concat [d,[e,d]|/2,g,f,e,d,g_,e_])
      <>
    Drones [bb,f,c]

  , section 60 $
    LineT Nothing (stretching 4) (v [d,db])

  , section 61 $
    Drones [eb',bb,f]

  , section 62 $
    Line Nothing (down _P5 $ v motC)
      <>
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
    Line Nothing (down _P5 $ v motC)
      <>
    Drones [f__,f___]

  , section 67 $
    Drones [f__,f___]

  , section 68 $
    Line Nothing (down _P5 $ v motC)
      <>
    Drones [f__,f___]

  , section 69 $
    Drones [f__,f___]

  , section 70 $
    Drones [f__,f___]
  ]

section_C =
  -- C section
  [ section 71 $
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
    Line Nothing (v [cs,b_,a_,fs_,gs_])

  , section 80 $
    Drones [g', cs', fs, d, gs_]
  ]

section_B2 =
  -- B2
  [ section 100 $
    down _P4 (Line Nothing $ v $ motALyd ++ motALyd)
      <>
    Drones (down _P15 [a,d',g',c''])

  , section 101 $
    down _P4 (Line Nothing $ v $ motA)
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
    Line Nothing (v[a,g,g,d,d,d',d',cs', b,e',e',d',d',cs',cs',b]) -- TODO etc

  , section 104 $
    Line Nothing (v [e',d',d',a, c',b,b,g, g,a,a,b]) -- TODO etc

  , section 105 $
    down _P4 (LineHarm motBLongHarm)
      <>
    Drones [d__,d___]

  , section 106 $
    -- TODO line
    Drones [d__,d___]

  , section 107 $
    down m7 $ Line Nothing (v motCToV)

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
    Line Nothing (v motALyd)
      <>
    Drones [f,c,g_]

  , section 112 $
    Line Nothing (v motC)
      <>
    Drones [c_,c__]

  , section 113 $
    -- TODO line
    Drones [c_,c__]

  , section 114 $
    Line Nothing (v motC)
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
  ]

section_CODA =
  -- CODA
  [ section 120 $
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




sketch :: [(Natural, Material Interval Pitch)]
sketch =
  -- TODO temporary cuts for preview purposes
  -- Restore!
  cut section_A1
  <>
  cut section_A2A

  <>
  section_A1
  <>
  section_A2B

  <>
  section_B1
  <>
  cut section_C

  <>
  cut section_A1
  <>
  cut section_B2
  <>
  cut section_CODA




-- TODO use these everywhere applicable
motALyd :: [Note Pitch]
motALyd = concat [b, [a, b] |/ 2, a, f, f, a]

motA :: [Note Pitch]
motA    = concat [c, [b_,c] |/ 2, b_,g_,g_,a_,a_,b_] -- TODO etd

motB :: [Note Pitch]
motB = [a,g,g,d]

motBLong :: [Note Pitch]
motBLong = [a,g,g,d, d',cs',cs',b] -- TODO etc

motBLongHarm :: [(Voice Pitch, [Pitch])]
motBLongHarm = [(v[a,g,g,d], [d,b_]), (v[d',cs',cs',b], [a,e])] -- TODO etc

motC :: [Note Pitch]
motC = [f,e,a,g,e' |* 4]

motCToV:: [Note Pitch]
motCToV = [f,e,a,g,g' |* 4]
motCToI = [f,e,a,g,c' |* 4]
motCVar = [e,d,g,f,d' |* 4]

-- TODO a variant of motA
-- TODO "tie" first two notes (fuse does not work properly)
subjA :: Voice Pitch
subjA = ((cs' |* 4) <> v [e'|*2,d',e'|/2,d'|/2,cs'|*14] |/ 2)

subjX :: Voice Pitch
subjX = ((cs' |* 4) <> v [cs',d',cs',b,cs']|*(4/(5*2)) <> v[e'|*2,d'|*14] |/ 2)

divModDur :: Duration -> Duration -> (Integer, Duration)
divModDur x v = (n, r)
  where
    n = floor (x / v)
    -- r follows from (v*^n ^+^ r = x)
    r = x ^-^ (v *^ fromIntegral n)
{-# INLINE divModDur #-}
