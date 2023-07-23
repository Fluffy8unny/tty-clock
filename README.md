# TTYClock

Simple clone of the famous tty-clock C program.

## installation

```
cabal install
```

will build, copy and add it to the path

## usage

```
TTYClock
```

will run the program with the default config.yml that was provided with the repository and installed via cabal

```
TTYClock -c config2.yml
TTYClock --config config3.yml
```

will run the program with a custom config

## config.yml

This file is used to configure the behaivor of the clock. The config consits of two parts:

* general settings
* glyph definitions

Each digit in the clock display is displayed by a glyph. Here each line of the glyph is encoded by an integer. 1s in the binary representation of these numbers are rendered by "symbolOn", 0s are rendered by displaying "symbolOff"

### general settings

* glyphHeight: Height of each glyph. Each glyph definition will need to contain this may numbers, which define the bits of the glpyh.

* glyphWidth : Width of each glyph. Each number for each glyph needs to be in [0,2^glyphWidth]
* symbolOff : If a bit in a glyph is 0, this will be displayed
* symbolOn : If a bit in a glyph is 1, this will be displayed
* timeDisplay: how the time string is supposed to be rendered. All characters used here have to be defined in glyphs. An explanation of the display format is given  [here](https://hackage.haskell.org/package/time-1.12.2/docs/Data-Time-Format.html)
* dateDisplay: How the date is displayed below the clock. "" to display no date
* border: If you are using automatic zoom, you might want to define a number of rows and colums that are always empty
* zoom: Can be either "off","auto" or an integer 
    * off : no zoom, each pixel of the glyph is rendered by a single character
    * interger(n) : each pixel of the glpyh will be represented by a square of n*n integers
    * auto: the maximum zoom is applied, taking the border into consideration
* centerX : should the clock be centered horizontally
* centerY : should the clock be centered vertically
* updateDelay : time in mus till an updated time is pulled
* foreground: Glyph color. This is can either be "Color", or "Modifier Color". Valid Colors are:
[ "Black", "Red", "Green", "Yellow", "Blue", "Magenta", "Cyan", "White" ] Valid Modifiers are:  [ "Dull", "Vivid"]

### glyphs
A glyph consists of a list of integers of length glyph height. Each value represents a line in the glyph. Each value has to be smaller than 2^glyphWidth, so [0,7] for a glyphWidth of 3, or [0,15] for a glyphWidth of 4. Each line needs to contain exactly glyphHeight values.

#### syntax:
* char : which character in the string defined by timeString is replaced
* bits : list of integer values that represent the glyph

#### example:
char       : "0"
bits       : [7,5,5,5,7]
| Integers | Bit 2|Bit 1|Bit 0 |
 | ----------- | -|-|-|
| 7 | #|#|#|
| 5 | #||# |
| 5 | #||# |
| 5 | #||# |
| 7 | #|#|#|

## example conifg

```
glyphHeight  : 5
glyphWidth   : 3
symbolOff    : " " 
symbolOn     : "█"
timeDisplay  : "%H:%M:%S"
dateDisplay  : "%Y-%b-%y"
border       : 10
zoom         : "auto"
centerX      : "center"
centerY      : "center" 
updateDelay  : 10000
foreground   : Vivid Green
background   : Dull Black

glyphs:
  - char       : "0"
    bits       : [7,5,5,5,7]

  - char       : "1"
    bits       : [4,4,4,4,4]

  - char       : "2"
    bits       : [7,4,7,1,7]

  - char       : "3"
    bits       : [7,4,7,4,7]

  - char       : "4"
    bits       : [5,5,7,4,4]

  - char       : "5"
    bits       : [7,1,7,4,7]

  - char       : "6"
    bits       : [7,1,7,5,7]

  - char       : "7"
    bits       : [7,4,4,4,4]

  - char       : "8"
    bits       : [7,5,7,5,7]

  - char       : "9"
    bits       : [7,5,7,4,7]

  - char       : ":"
    bits       : [0,2,0,2,0]

  - char       : "."
    bits       : [0,0,0,2,0]
```
