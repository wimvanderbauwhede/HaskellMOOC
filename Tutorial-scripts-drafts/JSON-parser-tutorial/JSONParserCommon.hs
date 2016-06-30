module JSONParserCommon
where
import JSONTypes
{-
import Text.ParserCombinators.Parsec 
import qualified Text.ParserCombinators.Parsec.Token as P
import qualified Text.ParserCombinators.Parsec.Language as L
-}
import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as P
import qualified Text.Parsec.Language as L


run_parser_print :: Show a => Parser a -> String -> IO ()
run_parser_print p str = do
      case parse p "" str of
           Left err -> do
               putStr "parse error at "
               print err
           Right x  -> putStrLn $ "    "++(show x)++","
                                                                                                                                                         
run_parser :: Parser a -> String -> a
run_parser p str =  case parse p "" str of
    Left err -> error $ "parse error at " ++ (show err)
    Right val  -> val  

lexer       = P.makeTokenParser jsonDef

jsonDef = L.emptyDef
    { P.commentStart = ""
    , P.commentEnd = ""
    , P.commentLine = ""
    , P.nestedComments = False
    , P.identLetter = alphaNum 
    , P.opStart = P.opLetter jsonDef
    , P.opLetter = oneOf "=+-*/.:%&<>" 
    , P.reservedOpNames= []
    , P.reservedNames = []
    , P.caseSensitive = True
    }


parens          = P.parens lexer    
brackets        = P.brackets lexer    
braces          = P.braces lexer    
commaSep        = P.commaSep lexer
commaSep1       = P.commaSep1 lexer
whiteSpace      = P.whiteSpace lexer    
symbol          = P.symbol lexer    
word            = P.identifier lexer
identifier      = P.identifier lexer
reserved        = P.reserved lexer    
reservedOp      = P.reservedOp lexer
integer         = P.integer lexer    
charLiteral     = P.charLiteral lexer    
stringLiteral   = P.stringLiteral lexer    
comma           = P.comma lexer
colon            = P.colon lexer
natural         = P.natural lexer
-- letter          = P.letter lexer

