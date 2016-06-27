module Htrans.Screener (

  notify

) where

import Libnotify

notify :: String -> String -> IO Libnotify.Notification 
notify txt res = 
  display $
       summary txt
    <> body res
    <> icon "accessories-dictionary"
    <> timeout Infinite
