
{-
 - Orchestra piece
 -
 - TODO assemble material to be used
 -}

import Music.Prelude
import qualified Ex.StringTexture

main = defaultMain music

music = stretch 0.5 $ Ex.StringTexture.music
