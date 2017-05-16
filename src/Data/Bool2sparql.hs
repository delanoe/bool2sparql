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

end_query :: Maybe Int -> Maybe Int -> String
end_query offset limit = unlines $ ["}"] ++ offset' ++ limit'
        where
            limit' = case limit of
                      Nothing -> [""]
                      Just l -> ["LIMIT " ++ (show l)]
            offset' = case offset of
                      Nothing -> [""]
                      Just o -> ["OFFSET " ++ (show o)]


create :: Query -> [Char] -> Maybe Int -> Maybe Int -> [Char]
create queryType query o l = (begin queryType) ++ cnfPrinter (contains) (fromCNF (boolTreeToCNF query')) (end_query o l)
    where
        query' = case runParser (parseBoolExpr identifier) () "" query of
          Right query'' -> query''
          Left  error'     -> error $ "BUG at Parsing: " ++ (show error')



