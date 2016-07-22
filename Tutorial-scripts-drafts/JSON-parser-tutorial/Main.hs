module Main where
import JSONSrcIO ( read_JSON_src, write_JSON_src )
import JSONParser ( json_parser )
import JSONParserCommon ( run_parser_print )
import JSONTypes

import System.Environment ( getArgs )

templ_src_name_default =  "example.json"

main = do
    args <- getArgs
    let
        templ_src_name = if null args then templ_src_name_default else head args
    templ_src_str <- read_JSON_src templ_src_name 
    run_parser_print json_parser templ_src_str
