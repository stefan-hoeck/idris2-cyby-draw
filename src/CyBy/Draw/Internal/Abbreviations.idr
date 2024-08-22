module CyBy.Draw.Internal.Abbreviations

import CyBy.Draw.Internal.Atom
import CyBy.Draw.Internal.Graph
import Derive.Prelude
import Text.Molfile

%default total
%language ElabReflection

public export
record Abbreviation where
  constructor A
  label  : String
  ||| Label to use when printing the abbreviation from right to left
  revlbl : String
  graph  : CDGraph

%runElab derive "Abbreviation" [Show,Eq]

abbr : String -> String -> String -> Abbreviation
abbr x y = A x y . readMolfile

export
reverseLabel : String -> List Abbreviation -> String
reverseLabel s []              = s
reverseLabel s (A l r _ :: xs) = if s == l then r else reverseLabel s xs

ph, cy : String
ac, oAc, bn, bz : String

export
phenyl : CDGraph
phenyl = readMolfile ph

export
abbreviations : List Abbreviation
abbreviations =
  [ abbr "Ac" "Ac" ac
  , abbr "Bn" "Bn" bn
  , abbr "Bz" "Bz" bz
  , abbr "Cy" "Cy" cy
  , abbr "OAc" "AcO" oAc
  , abbr "Ph" "Ph" ph
  ]

ac =
  """

  created by cyby-draw 1.0
  
    3  2     0                      V2000
     -2.3125    2.9375    0.0000   C
     -2.3125    4.1875    0.0000   O
     -3.3950    2.3124    0.0000   C
    1  2  2  0
    1  3  1  0
  M  END
  """

oAc =
  """

  created by cyby-draw 1.0

    4  3     0                      V2000
     -2.1875    1.6875    0.0000   O
     -1.1049    2.3125    0.0000   C
     -0.0223    1.6875    0.0000   C
     -1.1049    3.5625    0.0000   O
    1  2  1  0
    2  3  1  0
    2  4  2  0
  M  END
  """

ph =
  """
  
  created by cyby-draw 1.0
  
    6  6     0                      V2000
     -0.3125    1.5625    0.0000   C
     -1.3949    0.9375    0.0000   C
     -1.3949   -0.3125    0.0000   C
     -0.3125   -0.9375    0.0000   C
      0.7699   -0.3125    0.0000   C
      0.7699    0.9375    0.0000   C
    1  2  1  0
    1  6  2  0
    2  3  2  0
    3  4  1  0
    4  5  2  0
    5  6  1  0
  M  END
  """

cy =
  """
  
  created by cyby-draw 1.0
  
    6  6     0                      V2000
     -0.3125    1.5625    0.0000   C
     -1.3949    0.9375    0.0000   C
     -1.3949   -0.3125    0.0000   C
     -0.3125   -0.9375    0.0000   C
      0.7699   -0.3125    0.0000   C
      0.7699    0.9375    0.0000   C
    1  2  1  0
    1  6  1  0
    2  3  1  0
    3  4  1  0
    4  5  1  0
    5  6  1  0
  M  END
  """

bn =
  """

  created by cyby-draw 1.0
  
    7  7     0                      V2000
     -5.8125    2.0625    0.0000   C
     -3.6476    4.5625    0.0000   C
     -4.7299    3.9375    0.0000   C
     -4.7299    2.6875    0.0000   C
     -3.6476    2.0625    0.0000   C
     -2.5650    2.6875    0.0000   C
     -2.5650    3.9375    0.0000   C
    1  4  1  0
    2  3  1  0
    2  7  2  0
    3  4  2  0
    4  5  1  0
    5  6  2  0
    6  7  1  0
  M  END
  """

bz =
  """

  created by cyby-draw 1.0

    8  8     0                      V2000
     -5.8125    2.0625    0.0000   C
     -3.6476    4.5625    0.0000   C
     -4.7299    3.9375    0.0000   C
     -4.7299    2.6875    0.0000   C
     -3.6476    2.0625    0.0000   C
     -2.5650    2.6875    0.0000   C
     -2.5650    3.9375    0.0000   C
     -5.8125    0.8125    0.0000   O
    1  4  1  0
    1  8  2  0
    2  3  1  0
    2  7  2  0
    3  4  2  0
    4  5  1  0
    5  6  2  0
    6  7  1  0
  M  END
  """
