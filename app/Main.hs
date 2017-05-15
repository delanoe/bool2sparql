module Main where

import System.Environment (getArgs)

import Options.Applicative
import Data.Semigroup ((<>))

import Safe (headMay)
import qualified Text.Parsec as TP
import Data.BoolExpr
import Data.BoolExpr.Parser
import Data.BoolExpr.Printer



data Options = Options { query :: String
                       , count :: Bool
                       , limit :: Int
                       } deriving (Show)

options :: Parser Options
options = Options
        <$> strOption ( long "query"
                      <> short 'q'
                      <> metavar "TARGET"
                      <> help "Query as boolean language."
                       )
        
        <*> switch    ( long "count"
                      <> short 'c'
                      <> help "Count (True) or False get"
                       )
        
        <*> option auto ( long "Limit of results"
                        <> short 'l'
                        <> value 1000
                        <> metavar "INT"
                         )

queryTest0 :: String
queryTest0 = "(a OR b) AND (n OR m) NOT (x OR y)"

queryTest1 :: String
queryTest1 = "a OR b"


contains :: Show a => a -> ShowS
contains  x = showString ("( bif:contains(?title, "++ show x ++")"
                       ++ " OR bif:contains(?abstract, "++ show x ++"))"
                         )

begin_query :: String
begin_query = unlines ["select ?resource ?id ?title ?date ?abstract ?source where {"
                      ,"?resource a isidore:BibliographicalResource;"
                      ,"dcterms:title ?title;"
                      ,"dcterms:date ?date;"
                      ,"dc:description ?abstract;"
                      ,"dc:description ?id;"
                      ,"ore:isAggregatedBy ?s."
                      ,"?s dcterms:title ?source."
                      ,"FILTER"
                      ]

end_query :: Int -> String
end_query n = unlines ["}"
                    ,"limit " ++ show n
                    ]

createQuery :: [Char] -> Int -> [Char]
createQuery query n = begin_query ++ cnfPrinter (contains) (fromCNF (boolTreeToCNF query')) (end_query n)
    where
        query' = case TP.runParser (parseBoolExpr identifier) () "" query of
          Right query'' -> query''
          Left  bad     -> error $ "BUG at Parsing" ++ (show bad)



bool2sparql :: Options -> IO ()
bool2sparql (Options q False n) = putStrLn $ createQuery q n
bool2sparql (Options q True  _) = putStrLn $ "count only" ++ q


main :: IO ()
main = bool2sparql =<< execParser opts
  where
    opts = info (options <**> helper)
      ( fullDesc
     <> progDesc "Input: your boolean query. Output: your query in SPARQL language"
     <> header "bool2sparql - DSL to translate boolean query to Sparql query." )


