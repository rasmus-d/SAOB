module Parser where

    bufferMaxSize :: Int
    bufferMaxSize = 8

    -- Gå igenom en omvänd sträng och försök hitta den största delsträngen i en uppslagstabell.
    -- EX: tabellen innehåller ett uppslag "abc" och "abcde" och indata är "gfedcba"
    -- backtrack kommer vara lika med (m, rest) där m är uppslaget för "abcde" och rest är "fg" 
    backtrack :: String -> [(String,String)] -> String -> Maybe (String,String)
    backtrack l@(c:str) table rest = case lookup (reverse l) table of
        Nothing -> backtrack str table (c:rest)
        Just m -> Just (m, rest)
    backtrack [] table rest = Nothing

    -- Översätter alla delsträngar i en sträng som finns med i given associationslista med motsvarande uppslag. 
    -- Fyller en buffer med avlästa tecken. När den är full anropas backtrack för att hitta största delsträngen
    -- som finns i buffern.
    parse :: String -> String -> [(String,String)] -> String
    parse str buffer table 
        | length buffer > bufferMaxSize || null str && not (null buffer) = case backtrack buffer table [] of
            Nothing -> last buffer : parse str (init buffer) table
            Just (found, rest) -> found ++ parse str (reverse rest) table 
    parse (h:t) buffer table = parse t (h:buffer) table
    parse [] buffer _ = []
            
