module CyBy.Draw.I18n.DE

import Web.Async.Extra.I18n.EN
import public CyBy.Draw.I18n

%default total

parameters {auto log : Logger JS}
  export
  [DrawDE] DrawLocal using DOMEN ExtraEN where
    abbreviations = "--Abkürzungen--"
    angle         = "Winkel"
    atomType      = "Typ"
    benzene       = "Benzol"
    boron         = "Bor"
    bromine       = "Brom"
    carbon        = "Kohlenstoff"
    centerTxt     = "Zentrum"
    charge        = "Ladung"
    chlorine      = "Chlor"
    cyclobutane   = "Cyclobutan"
    cycloheptane  = "Cycloheptan"
    cyclohexane   = "Cyclohexan"
    cyclooctane   = "Cyclooctan"
    cyclopentane  = "Cyclopentan"
    cyclopropane  = "Cyclopropan"
    detailsTxt    = "Details"
    doubleBond    = "Doppelbindung"
    element       = "Element"
    eraseTxt      = "löschen"
    fluorine      = "Fluor"
    isotope       = "Isotop"
    length        = "Länge"
    loadTxt       = "Laden..."
    mix           = "Mix"
    nitrogen      = "Stickstoff"
    oxygen        = "Sauerstoff"
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

