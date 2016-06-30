module JSONSrcIO (
		read_JSON_src,
        write_JSON_src
) where
read_JSON_src :: String -> IO String
read_JSON_src src_name = do
    src <- readFile src_name
    return src

write_JSON_src :: String -> String -> IO ()
write_JSON_src src_name src_str =  do
    writeFile src_name src_str

    
