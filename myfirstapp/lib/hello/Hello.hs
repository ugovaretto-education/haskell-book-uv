module Hello where
import HaskellSay (haskellSay)
sayHello :: String -> IO ()
sayHello name = 
  haskellSay ("Hello " ++ name ++ "!")
