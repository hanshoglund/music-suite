
{-
 - Orchestra piece
 -
 - TODO assemble material to be used
 -}

import Music.Prelude
import qualified Ex.StringTexture
import qualified P.SustainPunctuated.Score

main = defaultMain music

music =
  pseq
  [ P.SustainPunctuated.Score.music
  , Ex.StringTexture.music
  ]
