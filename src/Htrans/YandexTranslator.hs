{-# LANGUAGE OverloadedStrings #-}

module Htrans.YandexTranslator (
-- func
  getTranslate

) where

import Network.Yandex.Translate
import Control.Monad.IO.Class
import Data.Default.Class
import qualified Data.Text as T

getTranslate :: APIKey -> Language -> Language -> [T.Text] -> IO (Maybe T.Text)
getTranslate key' from' to' text' =
  runYandexApiSession (configureApi key') $
  do (result,_,_) <-translate (Just from') to' def text'
     liftIO $ return (validateText result) 

validateText :: [T.Text] -> Maybe T.Text
validateText []    = Nothing
validateText (x:_) = if T.null x then Nothing else Just x
