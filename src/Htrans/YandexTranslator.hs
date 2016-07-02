module Htrans.YandexTranslator (
-- func
  getTranslate

) where

import Network.Yandex.Translate
import Control.Monad.IO.Class
import Data.Default.Class
import qualified Data.Text as T
import Htrans.Types (Lang, convertLang)

getTranslate :: APIKey -> Lang -> Lang -> Maybe T.Text -> IO (Maybe T.Text)
getTranslate key' from' to' text' =
  liftIO $ case text' of
    Nothing -> return Nothing
    Just x  -> runYandexApiSession (configureApi key') $
       do (result,_,_) <-translate (Just (convertLang from')) (convertLang to') def [x]
          return (validateText result) 

validateText :: [T.Text] -> Maybe T.Text
validateText []    = Nothing
validateText (x:_) = if T.null x then Nothing else Just x
