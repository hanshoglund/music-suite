
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
  tempo largo $
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
render _ = error "TODO"

sketch :: [(Natural, Material)]
sketch =
  [ (,) 1 $
    Drones [fs', g', d'', a'']
  , (,) 2 $
    Drones [fs', g', d'', a'']
      <>
    Canon [b, cs']
  ]
