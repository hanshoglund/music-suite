
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

music =
  times 1 $ pseq
  [ P.BrassLargeEnsemble.Score.music
  , P.SustainPunctuated.Score.music
  , Ex.StringTexture.music
  , Ex.Sky.music
  , Ex.Voicing.music
  , P.WindsPhrasing.Score.music
  ]
