module SimpleJSON where


data JValue = JString String
            | JNumber Double
            | JBoolean Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
            deriving (Eq, Ord, Show)
