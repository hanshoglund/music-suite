
{-
 - Orchestra piece
 -
 - TODO assemble material to be used
 -}

import Music.Prelude
import qualified Ex.StringTexture
import qualified P.SustainPunctuated.Score
import qualified P.WindsPhrasing.Score
import qualified P.BrassLargeEnsemble.Score
import qualified Ex.Sky
import qualified Ex.Voicing

main = defaultMain music

-- TODO make most/all of these into functions
-- Explore variations of *similar* material
-- Edit/manipulate parameters to turn into some kind of logical sequence
--
-- TODO transcribe more from manual notes. Try to make everything into functions
-- (find some parameter to vary)
music =
  times 1 $ pseq
  [ P.SustainPunctuated.Score.music

  , Ex.StringTexture.music

  , Ex.Sky.music

  -- TODO contrary motion of chrods, a la Tan Dun "Map"

  -- TODO make proper chords, orchestrate
  , Ex.Voicing.music

  , P.WindsPhrasing.Score.music

  , arrangeFor (divide 3 trumpets ++ divide 4 horns ++ divide 2 trombones)
    $ P.BrassLargeEnsemble.Score.music
  ]

