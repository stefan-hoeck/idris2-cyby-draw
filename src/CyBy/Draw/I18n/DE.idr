module CyBy.Draw.I18n.DE

import Web.Async.Extra.I18n.DE
import Web.Async.I18n.DE
import public CyBy.Draw.I18n

%default total

parameters {auto log : Logger JS}
  export
  [DrawDE] DrawLocal using DOMDE ExtraDE where
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
    phosphorous   = "Phosphor"
    pse           = "PSE"
    pseLong       = "Periodensystem"
    redoTxt       = "wiederholen"
    saveTxt       = "Speichern..."
    selectTxt     = "auswählen"
    setCharge     = "Ladung auswählen"
    setElement    = "Element auswählen"
    setMass       = "Massenzahl auswählen"
    singleBnd     = "Einfachbindung"
    singleDown    = "Einfachbindung unten"
    singleEither  = "Einfachbindung unten oder oben"
    singleUp      = "Einfachbindung oben"
    sulfur        = "Schwefel"
    tripleBond    = "Dreifachbindung"
    undoTxt       = "rückgängig"
    xcoord        = "x-Koord."
    ycoord        = "y-Koord."
    zoomInTxt     = "Vergrössern"
    zoomOutTxt    = "Verkleinern"

    copied        = info "Struktur in die Zwischenablage kopiert"
    readErr x     = error "Fehler beim Lesen der Struktur: \{x}"

    logOpened p     = info "Datei geöffnet: \{p}"
    noMetadata p    = error "Datei \{p} enthält keine CyBy-Metadaten. Sie wurde möglicherweise von einem anderen Programm erstellt oder bearbeitet."
    wrongFileType p = error "Nicht unterstützter Dateityp: \{p}"


    logDrawEvent x =
      case x of
        SelAbbr {}  => trace "Zeichenereignis: \{show x}"
        SetTempl {} => trace "Zeichenereignis: \{show x}"
        Load {}     => trace "Zeichenereignis: \{show x}"
        Move {}     => trace "Zeichenereignis: \{show x}"
        _           => debug "Zeichenereignis: \{show x}"

