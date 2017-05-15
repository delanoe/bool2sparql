module Main where

import System.Environment (getArgs)

import Options.Applicative
import Data.Semigroup ((<>))

-- import Safe (headMay)
import Data.Bool2sparql



data Options = Options { query :: String
                       , count :: Bool
                       , limit :: Int
                       } deriving (Show)

options :: Parser Options
options = Options
        <$> strOption ( long "query"
                      <> short 'q'
                      <> metavar "TARGET"
                      <> help "Enter your query with boolean language."
                       )
        
        <*> switch    ( long "count"
                      <> short 'c'
                      <> help "Option to count number of result only."
                       )
        
        <*> option auto ( long "Limit of results"
                        <> short 'l'
                        <> value 1000
                        <> metavar "INT"
                        <> help "Maximum of document is the limit added to the query."
                         )


bool2sparql :: Options -> IO ()
bool2sparql (Options q True  _) = putStrLn $ create Count q Nothing
bool2sparql (Options q False n) = putStrLn $ create Get   q (Just n)


main :: IO ()
main = bool2sparql =<< execParser opts
  where
    opts = info (options <**> helper)
      ( fullDesc
     <> progDesc "Input: your boolean query. Output: your query in SPARQL language"
     <> header "bool2sparql - DSL to translate boolean query to Sparql query." )


