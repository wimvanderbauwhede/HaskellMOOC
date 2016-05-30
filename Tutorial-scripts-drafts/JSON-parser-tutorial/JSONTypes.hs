module JSONTypes (
        JValue(..),
        mkJPair,
        mkJObj
         )
    where

import Data.Map hiding ( map )

type JMap = Data.Map.Map String JValue
data JValue = JString String
            | JNumber Integer
            | JObject JMap
            | JArray [JValue]
            | JBool Bool
            | JNull 
    deriving (Show)

mkJPair k v = JObject (Data.Map.singleton k v)

mkJObj :: [JValue] -> JValue
mkJObj j_vals = 
    let
        list_of_maps = map (\(JObject pair) -> pair) j_vals
        combined_map = Data.Map.unions list_of_maps
    in
        JObject combined_map
