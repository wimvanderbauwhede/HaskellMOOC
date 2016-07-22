module JSONParser where
import JSONTypes
import Text.Parsec
import Text.Parsec.String (Parser)

import JSONParserCommon

json_parser :: Parser JValue
json_parser = do
        whiteSpace
        j_top <- ( json_array_parser <|> json_obj_parser)
        return j_top

json_array_parser :: Parser JValue
json_array_parser = do    
    j_vals <- brackets $ commaSep json_value_parser 
    return $ JArray j_vals

json_obj_parser :: Parser JValue
json_obj_parser = do
    j_vals <- braces $ commaSep json_pair_parser -- a list of pairs
    return $ mkJObj j_vals

json_pair_parser = do
    k <- stringLiteral
    colon
    v <- json_value_parser
    return $ mkJPair k v

json_value_parser = json_array_parser <|> json_obj_parser <|> json_string_parser <|> json_number_parser <|> json_bool_parser <|> json_null_parser


json_string_parser = do
    str <- stringLiteral
    return $ JString str

json_number_parser = do
    num <- integer
    return $ JNumber num

json_bool_parser = do
    bstr <- ( symbol "true" <|> symbol "false" )
    let
        bval = if bstr == "true" then True else False
    return $ JBool bval

json_null_parser = do
    symbol "null"
    return JNull

    
