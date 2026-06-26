module CyBy.Draw.I18n.DE

import Web.Async.Extra.I18n.EN
import public CyBy.Draw.I18n

%default total

parameters {auto log : Logger JS}
  export
  [DrawDE] DrawLocal using DOMEN ExtraEN where
    abbreviations = "--abbreviations--"
    angle         = "Angle"
    atomType      = "Type"
    benzene       = "benzene"
    boron         = "boron"
    bromine       = "bromine"
    carbon        = "carbon"
    centerTxt     = "center"
    charge        = "Charge"
    chlorine      = "chlorine"
    cyclobutane   = "cyclobutane"
    cycloheptane  = "cycloheptane"
    cyclohexane   = "cyclohexane"
    cyclooctane   = "cyclooctane"
    cyclopentane  = "cyclopentane"
    cyclopropane  = "cyclopropane"
    detailsTxt    = "Details"
    doubleBond    = "double bond"
    element       = "Element"
    eraseTxt      = "erase"
    fluorine      = "fluorine"
    isotope       = "Isotope"
    length        = "Length"
    loadTxt       = "Load..."
    mix           = "Mix"
    nitrogen      = "nitrogen"
    oxygen        = "oxygen"
    phosphorous   = "phosphorous"
    pse           = "PSE"
    pseLong       = "periodic system"
    redoTxt       = "redo"
    saveTxt       = "Save..."
    selectTxt     = "select"
    setCharge     = "select charge"
    setElement    = "select element"
    setMass       = "select mass number"
    singleBnd     = "single bond"
    singleDown    = "single bond down"
    singleEither  = "single bond up or down"
    singleUp      = "single bond up"
    sulfur        = "sulfur"
    tripleBond    = "triple bond"
    undoTxt       = "undo"
    xcoord        = "x-Coord."
    ycoord        = "y-Coord."
    zoomInTxt     = "zoom in"
    zoomOutTxt    = "zoom out"

    copied        = info "structure copied to clipboard"
    readErr x     = error "error when reading structure: \{x}"

    logOpened p     = info "file opened: \{p}"
    noMetadata p    = error "File \{p} does not contain any CyBy metadata. It might have been created or edited by another program."
    wrongFileType p = error "unsupported file type: \{p}"


    logDrawEvent x =
      case x of
        SelAbbr {}  => trace "DrawEvent: \{show x}"
        SetTempl {} => trace "DrawEvent: \{show x}"
        Load {}     => trace "DrawEvent: \{show x}"
        Move {}     => trace "DrawEvent: \{show x}"
        _           => debug "DrawEvent: \{show x}"

