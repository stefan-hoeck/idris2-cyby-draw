||| Measuring the visible bounds of text is incredibly hard
||| and tons of material have been written about this.
|||
||| In this module, we take a pragmatic approach that produces
||| reasonable results without having to load and parse font files.
||| The drawback of this: The steps described below have to be repeated
||| for every new font we'd like to support.
|||
||| A detailed introduction to typography and how fonts are specified
||| can be found [here](https://learn.microsoft.com/en-us/typography/opentype/spec/otff).
|||
||| In general, we need to know the height and width of a piece of printed text
||| to properly align it with the rest of the drawing.
|||
||| Text height:
|||   there are different types of "height" when it comes to text, and I won't go
|||   into the details here. Suffice to say that we are interested in vertically aligning
|||   atom labels, charges, implicit hydrogen count and mass numbers in a
|||   way that feels natural. For this, wir are mostly interested in "capHeight" of
|||   a font: The height of capital letters (without descenders). This, together
|||   with the Em-square size can be read from font files.
|||
||| Text width:
|||   While computing the height of a piece of text is non-trivial, computing its
|||   width is insane. Every glyph has its own specific width, sometimes depending
|||   on its neighbouring glyphs (see ligatures and kerning). Fortunately, there
|||   is a quite simple method to get good approximations without being overly
|||   complicated. It is described on [stackoverflow](https://stackoverflow.com/questions/16007743/roughly-approximate-the-width-of-a-string-of-text-in-python)
|||   We use Python (because it has support for almost everything) to parse
|||   the true type font file we are interested in and generate a dictionary
|||   of the glyphs and their widths we are interested in. Using this to compute
|||   the width of a piece of text at a given font size is efficient and simple
|||   but not perfectly exact because it ignores kerning. It also requires large
|||   dictionaries if we want to support lots of unicode characters.
|||
||| Note: The Python script used to extract the glyph widths can be found in the
|||       `resources` directory.
module Text.Measure

import Data.SortedMap

%default total

-- based on [stackoverflow](https://stackoverflow.com/questions/16007743/roughly-approximate-the-width-of-a-string-of-text-in-python)
widths : List (Char,Bits32)
widths = [('0',278),('1',278),('2',278),('3',278),('4',278),('5',278),('6',278),('7',278),('8',278),('9',278),('a',279),('b',278),('c',250),('d',278),('e',278),('f',140),('g',278),('h',278),('i',111),('j',124),('k',251),('l',111),('m',417),('n',278),('o',278),('p',278),('q',278),('r',167),('s',250),('t',139),('u',278),('v',250),('w',364),('x',250),('y',250),('z',250),('A',334),('B',334),('C',361),('D',361),('E',334),('F',305),('G',389),('H',361),('I',139),('J',250),('K',334),('L',278),('M',417),('N',361),('O',389),('P',334),('Q',389),('R',361),('S',334),('T',305),('U',361),('V',334),('W',472),('X',334),('Y',334),('Z',305),('!',139),('"',177),('#',278),('$',278),('%',445),('&',334),('\'',95),('(',167),(')',167),('*',195),('+',292),(',',139),('-',167),('.',139),('/',139),(':',139),(';',139),('<',292),('=',292),('>',292),('?',278),('@',508),('[',139),('\\',139),(']',139),('^',235),('_',292),('`',167),('{',167),('|',130),('}',167),('~',292),(' ',139)]

widthMap : SortedMap Char Bits32
widthMap = fromList widths

averageWidth : Bits32
averageWidth = sum widthMap `div` cast (length widths)

charWidth : Bits32 -> Char -> Bits32
charWidth n c = maybe (n + averageWidth) (n+) $ lookup c widthMap

-- text was measure at a font size of 500, so we divide by that
-- when computing the width at a different font size.
textWidth : Nat -> String -> Double
textWidth fs s = cast (foldl charWidth 0 (unpack s)) * (cast fs) / 500.0

||| Metrics of a piece of text.
public export
record TextDims where
  constructor TD
  lineHeight : Double
  capHeight  : Double
  txtWidth   : Double

||| Utility for measuring text metrics.
public export
record Measure where
  [noHints]
  constructor M
  measure : Nat -> (font, txt : String) -> TextDims

||| This is a primitive but efficient implementation of `Measure`, which
||| is described in the module docs. A more exact implementation could
||| make use of a browser canvas and use text metrics from the DOM, but
||| this is not available when we are not drawing molecules in the browser.
|||
||| This implementation assumes a typical roman font similar to Arial.
||| It is based on the metrics of "Liberation Sans", which should have the
||| same layout as Arial or Helvetica.
|||
||| About magic numbers: 2048 is the Em square size, 1409 the cap height,
||| 307 the line height. These have to be multiplied with the font size.
||| Text widths are approxiamted by summing up glyph width stored in a
||| dictionary.
export
defaultMeasure : Measure
defaultMeasure =
  M $ \fs,f,s => case length s of
    0 => TD 0 0 0
    n =>
      let fsd := cast {to = Double} fs
       in TD (fsd * 307.0 / 2048.0) (fsd * 1409.0 / 2048.0) (textWidth fs s)
