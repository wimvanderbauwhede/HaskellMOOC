module Main where
import JSONSrcIO ( read_JSON_src, write_JSON_src )
import JSONParser ( json_parser )
import JSONParserCommon ( run_parser_print )
import JSONTypes

-- TOD: make this a command line argument
templ_src_name=  "example.json"

main = do
    templ_src_str <- read_JSON_src templ_src_name 
    run_parser_print json_parser templ_src_str
