
{-
Hi all,

A terminology question:

As we all know, the process that generates a chord by rotating its intervals (i.e. C E G -> E G C) is called *inversion*. What is the corresponding term for a mode? I.e. the process that transforms C major mode into D dorian, D dorian into E phrygian and so on.

Is it acceptable to use "invert" here as well (i.e. to talk of the church modes as "inversions" of major mode), or is there a better term?

Best,
Hans

-}
-- | Scales and chords.
module Music.Pitch.Scale
(
        Mode,
        Scale,
        Function,
        _intervals,
        _repeat,
        Chord,
        _tonic,

        complementInterval,
        characteristicInterval,
        invertChord,
        functionToChord,
        chordToList,

        leadingInterval,
        invertMode,
        modeToScale,
        scaleToList,

        -- * Common modes
        -- ** Classical modes
        majorScale,
        pureMinorScale,
        harmonicMinorScale,
        melodicMinorScaleUp,
        -- ** Church modes
        aeolian,
        locrian,
        ionian,
        dorian,
        phrygian,
        lydian,
        mixolydian,
        majorPentaTonic,
        minorPentaTonic,
        bluesMinor,
        bluesMajor,
        bebopScale,
        
        -- ** Miscellaneous modes
        wholeTone,
        octatonic,
        
        -- ** Modes of limited transposition
        firstMode,
        secondMode,
        thirdMode,
        fourthMode,
        fifthMode,
        sixthMode,
        seventhMode,

        -- * Common chords
        majorTriad,
        minorTriad,
        augmentedChord,
        diminishedChord,
        halfDiminishedChord,
        majorMinorSeventhChord,
        majorMajorSeventhChord,
  
) where

import Data.VectorSpace
import Data.AffineSpace
import Music.Pitch.Literal
import Control.Lens
import Music.Pitch hiding (Mode) -- Test

data Mode a = Mode [Diff a] (Diff a) -- intervals, repeat (usually octave)
data Scale a = Scale a (Mode a) -- root, mode

data Function a = Function [Diff a] (Diff a) -- intervals, repeat, repeat (usually octave)
data Chord a = Chord a (Function a) -- root, function 

_intervals :: Lens' (Function a) [Diff a]
_intervals f (Function is r) = fmap (\is -> Function is r) $ f is

_repeat :: Lens' (Function a) (Diff a)
_repeat f (Function is r) = fmap (\r -> Function is r) $ f r

{-
_pitches :: Lens' (Chord a) [a]
_pitches f (Chord t xs) = fmap (\xs -> Chord t (fmap (.-. t) xs)) $ f (fmap (.+^ t) $ xs) . _intervals
-}

_tonic :: Lens' (Chord a) a
_tonic f (Chord t xs) = fmap (\t -> Chord t xs) $ f t

{-
>>> complementInterval majorScale 
m2
>>> complementInterval harmonicMinorScale 
m2
>>> complementInterval pureMinorScale 
_M2
-}
complementInterval :: AffineSpace a => Function a -> Diff a
complementInterval (Function leaps repeating) = repeating ^-^ sumV leaps

-- TODO name?
characteristicInterval :: AffineSpace a => Function a -> Diff a
characteristicInterval (Function leaps repeating) = sumV leaps

invertChord :: AffineSpace a => Int -> Function a -> Function a
invertChord 0 = id
invertChord n = invertChord (n-1) . invertChord1
  where
    invertChord1 :: AffineSpace a => Function a -> Function a
    invertChord1 function@(Function leaps repeating) = Function (tail leaps ++ [complementInterval function]) repeating

functionToChord :: AffineSpace a => a -> Function a -> Chord a
functionToChord = Chord

chordToList :: AffineSpace a => Chord a -> [a]
chordToList (Chord tonic mode@(Function leaps repeating)) = offsetPoints tonic leaps
-- TODO inversion?






majorScale :: Mode Pitch
majorScale = Mode [_M2,_M2,m2,_M2,_M2,_M2] _P8

pureMinorScale :: Mode Pitch
pureMinorScale = Mode [_M2,m2,_M2,_M2,m2,_M2] _P8

harmonicMinorScale :: Mode Pitch
harmonicMinorScale = Mode [_M2,m2,_M2,_M2,m2,_A2] _P8

melodicMinorScaleUp :: Mode Pitch
melodicMinorScaleUp = Mode [_M2,m2,_M2,_M2,_M2,_M2] _P8


{-
>>> leadingInterval majorScale 
m2
>>> leadingInterval harmonicMinorScale 
m2
>>> leadingInterval pureMinorScale 
_M2
-}
leadingInterval :: AffineSpace a => Mode a -> Diff a
leadingInterval (Mode steps repeating) = repeating ^-^ sumV steps

invertMode :: AffineSpace a => Int -> Mode a -> Mode a
invertMode 0 = id
invertMode n = invertMode (n-1) . invertMode1
  where
    invertMode1 :: AffineSpace a => Mode a -> Mode a
    invertMode1 mode@(Mode steps repeating) = Mode (tail steps ++ [leadingInterval mode]) repeating

modeToScale :: AffineSpace a => a -> Mode a -> Scale a
modeToScale = Scale

scaleToList :: AffineSpace a => Scale a -> [a]
scaleToList (Scale tonic mode@(Mode steps repeating)) = offsetPoints tonic steps

  
ionian :: Mode Pitch
ionian = invertMode 0 majorScale

dorian :: Mode Pitch
dorian = invertMode 1 majorScale

phrygian :: Mode Pitch
phrygian = invertMode 2 majorScale

lydian :: Mode Pitch
lydian = invertMode 3 majorScale

mixolydian :: Mode Pitch
mixolydian = invertMode 4 majorScale

aeolian :: Mode Pitch
aeolian = invertMode 5 majorScale

locrian :: Mode Pitch
locrian = invertMode 6 majorScale

majorPentaTonic :: Mode Pitch
majorPentaTonic = Mode [_M2,_M2,m3,_M2] _P8

bluesMinor :: Mode Pitch
bluesMinor = invertMode 2 majorPentaTonic

bluesMajor :: Mode Pitch
bluesMajor = invertMode 3 majorPentaTonic

minorPentaTonic :: Mode Pitch
minorPentaTonic = invertMode 4 majorPentaTonic



bebopScale :: Mode Pitch
bebopScale = undefined

wholeTone :: Mode Pitch
wholeTone = firstMode

octatonic :: Mode Pitch
octatonic = secondMode

firstMode :: Mode Pitch
firstMode = Mode [_M2,_M2,_M2,_M2,_M2] _P8

secondMode :: Mode Pitch
secondMode = Mode [m2,_M2,_A1,_M2,m2,_M2,m2] _P8

thirdMode :: Mode Pitch
thirdMode = Mode [_M2,m2,m2,_M2,m2,m2,_M2,m2] _P8

fourthMode :: Mode Pitch
fourthMode = Mode [m2,m2,m3,m2,m2,m2,m3] _P8

fifthMode :: Mode Pitch
fifthMode = Mode [m2,_M3,m2,m2,_M3] _P8

sixthMode :: Mode Pitch
sixthMode = Mode [_M2,_M2,m2,m2,_M2,_M2,m2] _P8

seventhMode :: Mode Pitch
seventhMode = Mode [m2,m2,m2,_M2,m2,m2,m2,m2,_M2] _P8



majorTriad :: Function Pitch
majorTriad = Function [_M3,m3] _P8

minorTriad :: Function Pitch
minorTriad = Function [m3,_M3] _P8

augmentedChord :: Function Pitch
augmentedChord = Function [_M3,_M3] _P8

diminishedChord :: Function Pitch
diminishedChord = Function [m3,m3,_A2] _P8

halfDiminishedChord :: Function Pitch
halfDiminishedChord = Function [m3,m3,_M3] _P8

majorMinorSeventhChord :: Function Pitch
majorMinorSeventhChord = Function [_M3,m3,m3] _P8

majorMajorSeventhChord :: Function Pitch
majorMajorSeventhChord = Function [_M3,m3,_M3] _P8

-- 




-- TODO consolidate
offsetPoints :: AffineSpace p => p -> [Diff p] -> [p]
offsetPoints = scanl (.+^)

