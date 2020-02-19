module Music.Time.Juxtapose
  ( module Music.Time.Split,
    module Music.Time.Reverse,

    -- * Align without composition
    lead,
    follow,

    -- * Standard composition
    after,
    before,
    during,
    (|>),
    (<|),

    -- ** More exotic
    sustain,
    palindrome,

    -- * Catenation
    pseq,
    ppar,

    -- * Repetition
    times,
    group,
  )
where

import Control.Lens hiding ((<|), (|>))
import Data.AffineSpace
import Data.AffineSpace.Point
import Data.Semigroup
import Data.Stream.Infinite hiding (group)
import Data.VectorSpace
import Music.Time.Reverse
import Music.Time.Split

-- |
-- @
-- (a `lead` b)^.'offset' = b^.'onset'
-- @
lead :: (HasPosition a, HasPosition b, Transformable a) => a -> b -> a
a `lead` b = placeAt 1 (b `_position` 0) a

-- |
-- @
-- a^.'offset' = (a `follow` b)^.'onset'
-- @
follow :: (HasPosition a, HasPosition b, Transformable b) => a -> b -> b
a `follow` b = placeAt 0 (a `_position` 1) b

after :: (Semigroup a, Transformable a, HasPosition a) => a -> a -> a
a `after` b = a <> (a `follow` b)

before :: (Semigroup a, Transformable a, HasPosition a) => a -> a -> a
a `before` b = (a `lead` b) <> b

-- |
-- A value followed by its reverse (retrograde).
palindrome :: (Semigroup a, Reversible a, HasPosition a) => a -> a
palindrome a = a `after` rev a

infixr 6 |>

infixr 6 <|

-- |
-- An infix alias for 'after'.
(|>) :: (Semigroup a, HasPosition a, Transformable a) => a -> a -> a
(|>) = after

-- |
-- An infix alias for 'before'.
(<|) :: (Semigroup a, HasPosition a, Transformable a) => a -> a -> a
(<|) = before

-- infixr 6 >|
-- infixr 6 |<

-- |
-- Compose a list of sequential objects, with onset and offset tangent to one another.
--
-- For non-positioned types, this is the often same as 'mconcat'
-- For positioned types, this is the same as 'afterAnother'
pseq :: (Semigroup a, Monoid a, HasPosition a, Transformable a) => [a] -> a
pseq = Prelude.foldr (|>) mempty

-- |
-- Compose a list of parallel objects, so that their local origins align.
--
-- This not possible for non-positioned types, as they have no notion of an origin.
-- For positioned types this is the same as 'mconcat'.
ppar :: (Semigroup a, Monoid a) => [a] -> a
ppar = Prelude.foldr (<>) mempty

-- |
-- Move a value so that its era is equal to the era of another value.
during :: (HasPosition a, HasPosition b, Transformable a, Transformable b) => a -> b -> a
y `during` x = set era (view era x) y

-- |
-- Like '<>', but scaling the second agument to the duration of the first.
sustain :: (Semigroup a, HasPosition a, Transformable a) => a -> a -> a
x `sustain` y = x <> y `during` x

-- |
-- Repeat the given music /n/ times.
--
-- @
-- 'Int' -> 'Score' a -> 'Score' a
-- @
times :: (Monoid a, HasPosition a, Transformable a) => Int -> a -> a
times n = pseq . replicate n

-- |
-- Repeat the given music /n/ times and stretch to the original duration.
--
-- @
-- 'Int' -> 'Score' a -> 'Score' a
-- @
group :: (Monoid a, Transformable a, HasPosition a) => Int -> a -> a
group n x = times n x |/ fromIntegral n


-- |
-- Compose sequentially by aligning the nominal position of each value to the
-- first available time value.
--
-- TODO this requires another constraint for nominal position. For (Aligned ((t,_),_))
-- the nominal position is t.
--
-- @
-- length xs = length (snapTo ts xs)
-- @
snapTo :: (HasPosition a, Transformable a) => Stream Time -> [a] -> [a]
snapTo = undefined
