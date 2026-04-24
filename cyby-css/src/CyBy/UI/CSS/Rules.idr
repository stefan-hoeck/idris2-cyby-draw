module CyBy.UI.CSS.Rules

import Text.CSS
import Text.HTML.Ref
import Text.HTML.Tag
import CyBy.UI.CSS.Classes

%default total

public export
0 Rules0 : Type
Rules0 = List (Rule 0)

public export
0 Rules : Type
Rules = List (Rule 1)

--------------------------------------------------------------------------------
-- Colours
--------------------------------------------------------------------------------

public export
record Colours where
  [noHints]
  constructor C
  fg : Color
  bg : Color

export %hint
defaultColours : Colours
defaultColours =
  C {
    fg = black
  , bg = rgb 200 200 200
  }

--------------------------------------------------------------------------------
-- General
--------------------------------------------------------------------------------

export
general : (c : Colours) => Rules
general =
  [ elem Html
      [ height 100.perc
      , fontSize 10.px
      ]

  , elem Body
      [ display Flex
      , height 100.perc
      , width 100.perc
      , backgroundColor c.bg
      , color c.fg
      ]

  , elem Label
      [ display Flex
      , alignItems Center
      ]
  ]

export
all : Rules
all = general

main : IO ()
main = traverse_ (putStrLn . interpolate) all
