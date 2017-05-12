module Main where

import System.Environment (getArgs)

import Safe (headMay)
import Text.Parsec
import Data.BoolExpr
import Data.BoolExpr.Parser
import Data.BoolExpr.Printer

queryTest0 = "(a OR b) AND (n OR m) NOT (x OR y)"
queryTest1 = "a OR b"

contains  x = showString ("( bif:contains(?title, "++ show x ++")"
                       ++ " OR bif:contains(?abstract, "++ show x ++"))"
                         )
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

end_query = unlines ["}"
                    ,"limit 100"
                    ]

create query = begin_query ++ cnfPrinter (contains) (fromCNF (boolTreeToCNF query')) end_query
    where
        query' = case runParser (parseBoolExpr identifier) () "" query of
          Right query'' -> query''
          Left n       -> error "BUG at Parsing"

main :: IO ()
main = do
    q <- getArgs
    case headMay q of
      Nothing    -> putStrLn "Enter your query please"
      Just query -> putStrLn (create query)
