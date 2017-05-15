module Data.Bool2sparql where


import Text.Parsec (runParser)

import Data.BoolExpr
import Data.BoolExpr.Parser
import Data.BoolExpr.Printer

data Query = Count | Get deriving (Show, Read, Eq)

contains :: Show a => a -> ShowS
contains  x = showString (    "("
                         ++   "bif:contains(?title, "     ++ show x ++ ")"
                         ++   " OR "
                         ++   "bif:contains(?abstract, "  ++ show x ++ ")"
                         ++   ")"
                         )

begin :: Query -> String
begin query = unlines (selectOrcount ++ body)
    where
        body = [ "?resource a isidore:BibliographicalResource;"
               , "dcterms:title ?title;"
               , "dcterms:date ?date;"
               , "dc:description ?abstract;"
               , "dc:description ?id;"
               , "ore:isAggregatedBy ?s."
               , "?s dcterms:title ?source."
               , "FILTER"
               ]
        
        selectOrcount = case query of
                          Get   -> ["select ?resource ?id ?title ?date ?abstract ?source where {"]
                          Count -> ["select count(?id) where {"]

end_query :: Maybe Int -> String
end_query n = unlines $ ["}"] ++ limit
        where
            limit = case n of
                      Nothing -> [""]
                      Just n' -> ["limit " ++ (show n')]


create :: Query -> [Char] -> Maybe Int -> [Char]
create queryType query n = (begin queryType) ++ cnfPrinter (contains) (fromCNF (boolTreeToCNF query')) (end_query n)
    where
        query' = case runParser (parseBoolExpr identifier) () "" query of
          Right query'' -> query''
          Left  error'     -> error $ "BUG at Parsing: " ++ (show error')



