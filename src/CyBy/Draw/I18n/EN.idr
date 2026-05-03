module CyBy.Draw.I18n.EN

import public CyBy.Draw.I18n

%default total

parameters {auto log : Logger JS}
  export
  DrawLocal where
    logDrawMsg Copied        = info "structure copied to clipboard"
    logDrawMsg (ReadErr str) = warn "error when pasting structure: \{str}"

    logJSErr x =
      error
        """
        An error occurred in the user interface. This is a CyBy bug. Please
        contact your CyBy admin and send them this error message.
        
        Error details: \{dispErr x}
        """

    logDrawEvent x =
      case x of
        SelAbbr {}  => trace "DrawEvent: \{show x}"
        SetTempl {} => trace "DrawEvent: \{show x}"
        Load {}     => trace "DrawEvent: \{show x}"
        Move {}     => trace "DrawEvent: \{show x}"
        _           => debug "DrawEvent: \{show x}"

