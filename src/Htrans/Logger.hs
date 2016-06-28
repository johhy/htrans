module Htrans.Logger (
  -- types
  Priority(..),
  -- funcs
  setAppLogger,
  appName,
  logStartAppDebug,
  logStopAppDebug,
  logConfigDebug,
  logInOutInfo

) where

import System.Log.Logger
import System.Log.Handler.Simple (fileHandler)
import System.Log.Handler (setFormatter, LogHandler)
import System.Log.Formatter (simpleLogFormatter)

appName :: String
appName = "htrans"

setAppLogger :: FilePath -> Priority -> IO ()
setAppLogger logPath priority = do
    fHandler <- fileHandler logPath priority
    let cFormatter = setCustomFormatter fHandler
    updateGlobalLogger appName (addHandler cFormatter . setLevel priority)

setCustomFormatter :: System.Log.Handler.LogHandler a => a -> a
setCustomFormatter h = setFormatter h f
    where f = simpleLogFormatter "[$time : $prio] : $msg "


logStartAppDebug :: IO ()
logStartAppDebug = debugM appName "---- Start translation! ----"

logConfigDebug :: Show a => a -> IO ()
logConfigDebug cfg = debugM appName ("Get configuration:" ++ show cfg)

logInOutInfo :: String -> String -> IO ()
logInOutInfo input output = infoM appName ("input:"  ++ input ++
                                          " output:" ++ output)

logStopAppDebug :: IO ()
logStopAppDebug = debugM appName "---- Stop translation! -----"

