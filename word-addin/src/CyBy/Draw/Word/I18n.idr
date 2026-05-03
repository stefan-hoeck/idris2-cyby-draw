module CyBy.Draw.Word.I18n

import public CyBy.Draw.I18n
import public CyBy.Draw.Word.DomBindings

%default total

public export
interface DrawLocal => WordLocal where
  emptyMol       : JSErr
  emptySel       : JSErr
  emptySelection : JS es ()
  fromWord       : String
  imageReplaced  : JS es ()
  insertingImage : JS es ()
  loadErr        : String -> JSErr
  loadingImage   : JS es ()
  logDimensions  : (w,h : EMU) -> JS es ()
  molLoaded      : JS es ()
  newInserted    : JS es ()
  noMolfile      : JSErr
  toWord         : String
