{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T 
import qualified Data.Text.IO as I
import System.Exit (ExitCode(..), exitWith)
import Htrans.Logger (setAppLogger, logStartAppDebug,
                      logStopAppDebug, logConfigDebug, logInOutInfo)
import Htrans.Cli (Config(..), cli)
import Htrans.YandexTranslator (getTranslate)

main :: IO ()
main = cli >>= doTrans >>= exitWith

doTrans :: Config -> IO ExitCode
doTrans cfg = do

  setAppLogger (logPath cfg) (logLevel cfg)
  logStartAppDebug
  logConfigDebug cfg

  res <- getTranslate (key cfg) (from cfg) (to cfg) (text cfg)

  logInOutInfo (unpackMaybe (validateText (text cfg))) (unpackMaybe res)
  logStopAppDebug

  maybe (return $ ExitFailure 1)
        ((>> return ExitSuccess) . I.putStrLn) res

  where unpackMaybe Nothing  = "No text"
        unpackMaybe (Just x) = T.unpack x


validateText :: [T.Text] -> Maybe T.Text
validateText []    = Nothing
validateText (x:_) = if T.null x then Nothing else Just x


