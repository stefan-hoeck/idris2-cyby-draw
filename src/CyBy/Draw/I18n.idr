module CyBy.Draw.I18n

import public CyBy.Draw.Event
import public IO.Async.Logging
import public Web.Async

%default total

public export
interface DrawLocal where
  abbreviations : String
  angle         : String
  atomType      : String
  benzene       : String
  boron         : String
  bromine       : String
  carbon        : String
  centerTxt     : String
  charge        : String
  chlorine      : String
  clearTxt      : String
  cyclobutane   : String
  cycloheptane  : String
  cyclohexane   : String
  cyclooctane   : String
  cyclopentane  : String
  cyclopropane  : String
  detailsTxt    : String
  doubleBond    : String
  element       : String
  eraseTxt      : String
  fluorine      : String
  fromWord      : String
  isotope       : String
  length        : String
  loadTxt       : String
  mix           : String
  nitrogen      : String
  oxygen        : String
  phosphorous   : String
  pse           : String
  pseLong       : String
  redoTxt       : String
  saveTxt       : String
  selectTxt     : String
  setCharge     : String
  setElement    : String
  setMass       : String
  singleBnd     : String
  singleDown    : String
  singleEither  : String
  singleUp      : String
  sulfur        : String
  toWord        : String
  tripleBond    : String
  undoTxt       : String
  xcoord        : String
  ycoord        : String
  zoomInTxt     : String
  zoomOutTxt    : String

  copied        : JS es ()
  readErr       : String -> JS es ()

  logJSErr      : JSErr -> JS es ()
  logDrawEvent  : DrawEvent -> JS es ()

export %inline
DrawLocal => Loggable JS JSErr where logLoggable = logJSErr

export %inline
DrawLocal => Loggable JS DrawEvent where logLoggable = logDrawEvent
