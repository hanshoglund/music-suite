<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD

<<<<<<< HEAD
# music-articulation

Abstract representation of musical articulation (staccato, legato etc).

This library is part of the Haskell Music Suite, see <http://music-suite.github.io>.
=======
# music-dynamics

Abstract representation of musical dynamic levels.

This library is part of the Music Suite, see <http://music-suite.github.io>.
>>>>>>> 4bd0d2e995ac89e3d5c617310582c3cbdc51baaf
=======
[![Build Status](https://travis-ci.org/music-suite/music-parts.png)](https://travis-ci.org/music-suite/music-parts)

# music-parts

Abstract representation of musical parts and instruments.

This library is part of the Music Suite, see <http://music-suite.github.io>.
>>>>>>> 3bfd65ed77c4469493a75fd571295c2c599bcae0
=======
[![Build Status](https://travis-ci.org/music-suite/music-pitch.png)](https://travis-ci.org/music-suite/music-pitch)

# music-pitch

This package provides various representations of musical pitch.

This library is part of the Music Suite, see <http://music-suite.github.io>.
>>>>>>> 544f0092b250c06af51a1e4da03bc39cc3b15b24
=======

# music-score

Musical score and part representation.

This library is part of the Music Suite, see <http://music-suite.github.io>.
>>>>>>> ae6508d8028ed17823820241472dfe7fcbfe0a50
=======

# music-sibelius

Exporting of Sibelius scores to a compatible music representations. This export retains most features of the original scores and does not require a MusicXML export plugin.

This functionality is still experimental: you need to manually install the Sibelius plugin (see below), and run it as *Plugins > JSON > Export JSON*.

This library an exterimental addition to the Music Suite, see <http://music-suite.github.io>.
>>>>>>> d8479b761bb709ffe7c2043012c017b93613c933
=======
[![Build Status](https://travis-ci.org/music-suite/lilypond.png)](https://travis-ci.org/music-suite/lilypond)

# Lilypond

This package contains a terse Haskell representation of (a subset of) Lilypond 
expressions and a pretty-printer for such expressions. They can be used to generate 
musical notation.
>>>>>>> ea85e3198157777b8eeb259a2b38586b0dbd6bcd
=======

# musicxml2

MusicXML library for Haskell, including a concise, strongly typed music representation 
isomorphic to MusicXML 3.0.

The library is called `musicxml2` because there is already a `musicxml` library on Hackage.

## Supported features

* Part- and timewise scores
* Instruments lists and part groups
* Pitched and unpitched notes, with enharmonic pitch spelling
* Cue and grace notes
* Time signatures, Key signatures and clefs
* Custom note heads
* Beaming, cross-beams and tremolo
* Ornaments and articulation
* Dynamics

The following features are currently missing or incomplete:

* Lyrics
* Chord symbols
* Fingered bass
* Tablature
* Percussion grids
* Scordatura
* Layouts 

>>>>>>> c086f22272f5084b187870203a8584d0736eaa04

## Requirements

* [Haskell Platform](http://www.haskell.org/platform)

## Installation

<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
    cabal configure
    cabal install
=======
### Haskell library 

    cabal configure
    cabal install

### Sibelius plugin 

    make install-plugin
>>>>>>> d8479b761bb709ffe7c2043012c017b93613c933
=======

# music-suite

This library is part of the Music Suite, see <http://music-suite.github.io>.
>>>>>>> e9f60cb927908806e8d70e94fcb7ef3699429cf7
=======
    cabal configure
    cabal install
>>>>>>> ea85e3198157777b8eeb259a2b38586b0dbd6bcd
=======
    cabal configure
    cabal install
>>>>>>> c086f22272f5084b187870203a8584d0736eaa04
