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
  | Line [p]
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

render :: Material Interval Pitch -> Music
render (Drones xs) = ppar $ fmap fromPitch xs
render (Canon xs) =
  -- TODO use multiTempoCanon (using pitches composed in sequence as subject)
  ppar $ fmap fromPitch xs
render Empty = mempty
render (Sim a b) = render a <> render b
-- TODO proper rhythm:
render (Line xs) = level _f $ stretchTo 1 $ pseq $ fmap fromPitch xs
render _ = error "TODO"


section :: Natural -> Material v p -> (Natural, Material v p)
section = (,)

sketch :: [(Natural, Material Interval Pitch)]
sketch =
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

  , section 9 $
    Canon [a'', g'', fs'']
      <>
    Drones [fs', fs, b_] <> Line [cs', b, fs']
      <>
    Drones [b_, fs_]
      <>
    Canon [cs_, d_, b__]

  , section 10 $
    Line [fs', e']
      <>
    Drones [b, g, cs]
    -- TODO second (delayed) Line
      <>
    Drones [e__]

  -- TODO

  , section 41 $
    Drones [g, d', a']
      <>
    Line [fs, e, fs, e, c, c, e, fs, e, fs, e, c, b_]
  , section 41 $
    Drones [g, d', a']
      <>
    Line [b_,d_,c,b_,b_,d_]

  , section 41 $
    Drones [g, d', a']
      <>
    Line [fs, e, fs, e, c, c, e, fs, e, fs, e, c, b_]

  -- TODO
  , section 45 $
    up m2 (Drones [g, d', a'])
      <>
    up m2 (Line [fs, e, fs, e, c, c, e, fs, e, fs, e, c, b_])
  ]
