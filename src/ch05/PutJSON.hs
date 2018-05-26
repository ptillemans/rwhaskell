module PutJSON where

import Data.List (intercalate)
import SimpleJSON

renderJValue :: JValue -> String

renderJValue (JString s) = show s
renderJValue (JNumber n) = show n
renderJValue (JBool True) = "true"
renderJValue (JBool False) = "false"
renderJValue JNull = "null"

renderJValue (JObject o) = "{" ++ pairs o ++ "}"
  where
    pairs [] = ""
    pairs ps = intercalate ", " $ map renderPair ps
    renderPair (k, v) = show k ++ ": " ++ renderJValue v

renderJValue (JArray as) = "[" ++ vs ++ "]"
  where
    vs = intercalate ", " $ map renderJValue as

putJValue :: JValue -> IO ()
putJValue j = putStrLn $ renderJValue j
