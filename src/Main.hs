{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Exit (ExitCode(..), exitWith)
import Htrans.Logger (setAppLogger, logStartAppDebug,
                      logStopAppDebug, logConfigDebug, logInOutInfo)
import Htrans.Cli (Config(..), cli)
import Htrans.YandexTranslator (getTranslate)
import Htrans.XSelector (xselect)
import Htrans.EndPoint (showResult)

main :: IO ()
main = cli >>= xselect >>= doTrans >>= exitWith

doTrans :: Config -> IO ExitCode
doTrans cfg = do

  setAppLogger (logPath cfg) (logLevel cfg)
  logStartAppDebug
  logConfigDebug cfg

  res <- getTranslate (key cfg) (from cfg) (to cfg) (text cfg)

  logInOutInfo (text cfg) res
  logStopAppDebug

  showResult cfg res
  




