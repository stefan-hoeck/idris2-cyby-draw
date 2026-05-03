module CyBy.Draw.I18n

import public CyBy.Draw.Event
import public IO.Async.Logging
import public Web.Async

%default total

public export
interface DrawLocal where
  logJSErr      : JSErr -> JS es ()
  logDrawMsg    : DrawMsg -> JS es ()
  logDrawEvent  : DrawEvent -> JS es ()

export %inline
DrawLocal => Loggable JS JSErr where logLoggable = logJSErr

export %inline
DrawLocal => Loggable JS DrawMsg where logLoggable = logDrawMsg

export %inline
DrawLocal => Loggable JS DrawEvent where logLoggable = logDrawEvent
