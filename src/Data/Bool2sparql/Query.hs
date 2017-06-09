module Data.Bool2sparql.Query where


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


contains'' :: String -> String
contains''  x = (    "("
                 ++   "bif:contains(?title, \""     ++ (Prelude.map clean x) ++ "\")"
                 ++   " OR "
                 ++   "bif:contains(?abstract, \""  ++ (Prelude.map clean x) ++ "\")"
                 ++   ")"
                         )

clean :: Char -> Char
clean '\"' = '\''
clean x    = x

contains' :: Show a => a -> ShowS
contains'  x =     (  show x ++  )

begin :: Query -> String
begin query = unlines body
    where
        body = case query of 
                 Get -> [ "select ?resource ?title ?date ?abstract ?source where {"
                        , "?resource a isidore:BibliographicalResource;"
                        , "dcterms:title ?title;"
                        , "dcterms:date ?date;"
                        , "dc:description ?abstract;"
                        , "ore:isAggregatedBy ?s."
                        , "?s dcterms:title ?source."
                        , "FILTER"
                        ]

                 Count -> [ "select count(distinct ?resource) where {"
                          , "?resource a isidore:BibliographicalResource;"
                          , "dcterms:title ?title;"
                          , "dcterms:date ?date;"
                          , "dc:description ?abstract;"
                          , "ore:isAggregatedBy ?s."
                          , "?s dcterms:title ?source."
                          , "FILTER"
                          ]

        selectOrcount = case query of
                          Get   -> ["select ?resource ?id ?title ?date ?abstract ?source where {"]
                          Count -> ["select count(?id) where {"]

end_query :: Bool -> Maybe Int -> Maybe Int -> String
end_query order offset limit = unlines $ ["}"] ++ order' ++ offset' ++ limit'
        where
            order'  = case order of
                      True  -> ["ORDER BY DESC(?date)"]
                      False -> [""]
            limit'  = case limit of
                      Nothing -> [""]
                      Just l -> ["LIMIT " ++ (show l)]
            offset' = case offset of
                      Nothing -> [""]
                      Just o -> ["OFFSET " ++ (show o)]


create :: Query -> [Char] -> Maybe Int -> Maybe Int -> [Char]
create queryType query o l = (begin queryType) ++ contains'' query ++ end_query'
    where
        end_query' = case queryType of
                      Get   -> end_query True o l
                      Count -> end_query False o l


create' :: Query -> [Char] -> Maybe Int -> Maybe Int -> [Char]
create' queryType query o l = (begin queryType) ++ (contains'' body) ++ end_query'
    where
        -- body = cnfPrinter (showString) (fromCNF (boolTreeToCNF query')) ""
        body = boolExprPrinter (showString) (query') ""
        query' = case runParser (parseBoolExpr parseConstant) () "" query of
          Right query'' -> query''
          Left  error'     -> error $ "BUG at Parsing: " ++ (show error')
        end_query' = case queryType of
                      Get   -> end_query True o l
                      Count -> end_query False o l


