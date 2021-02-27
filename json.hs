module JSON where
import Data.List (intercalate)

data Value = JString String
            | JNumber Double
            | JObject [(String, Value)]
            | JArray [Value]
            | JBool Bool
            | JNull

show' singleObj = show (fst singleObj) ++ ":" ++ show (snd singleObj)

instance Show Value where
  show (JString str) = show str
  show (JNumber num) = show num
  show (JObject obj) = "{" ++ intercalate "," (map show' obj) ++ "}"
  show (JArray val) = show val
  show (JBool True) = "true"
  show (JBool False) = "false"
  show JNull = "null"
