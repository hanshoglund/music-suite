
import Music.Prelude
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

data Material
  = Drones [Pitch]
  | Canon [Pitch]
  | Line [Pitch]
  | Empty
  | Sim Material Material
  -- TODO add other info from sketch, e.g. dynamics, orchestration

instance Semigroup Material where
  (<>) = Sim
instance Monoid Material where
  mempty = Empty

render :: Material -> Music
render (Drones xs) = ppar $ fmap fromPitch xs
render (Canon xs) =
  -- TODO use multiTempoCanon (using pitches composed in sequence as subject)
  ppar $ fmap fromPitch xs
render Empty = mempty
render (Sim a b) = render a <> render b
-- TODO proper rhythm:
render (Line xs) = stretchTo 1 $ pseq $ fmap fromPitch xs
render _ = error "TODO"


section :: Natural -> Material -> (Natural, Material)
section = (,)

sketch :: [(Natural, Material)]
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
  ]
