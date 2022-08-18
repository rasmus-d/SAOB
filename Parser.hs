module Parser where
    -- Största storleken på en nyckel i "table" + 2 (för att tillåta seperationstecken).
    bufferMaxSize :: Int
    bufferMaxSize = 16

    body::[a]->[a]
    body [] = []
    body [e] = []
    body (h:t) = init t

    isSeperator :: Char -> Bool
    isSeperator c
        | c `elem` " \"\n\t\r()" = True
        | otherwise = False

    -- Gå igenom en omvänd sträng och försök hitta den största delsträngen i en uppslagstabell.
    -- EX: tabellen innehåller ett uppslag "abc" och "abcde" och indata är "gfedcba"
    -- backtrack kommer vara lika med (m, rest) där m är uppslaget för "abcde" och rest är "fg" 
    backtrack :: String -> [(String,String)] -> String -> Maybe (String,String)
    backtrack (sep:c:str) table rest 
        | isSeperator sep = 
            case lookup (reverse (c:str)) table of
                Nothing -> backtrack (c:str) table (sep:rest)
                Just m -> Just (m, sep:rest)
        | otherwise = backtrack (c:str) table (sep:rest)
    backtrack l table rest = Nothing

    -- Översätter alla delsträngar i en sträng som finns med i given associationslista med motsvarande uppslag. 
    -- Fyller en buffer med avlästa tecken. När den är full anropas backtrack för att hitta största delsträngen
    -- som finns i buffern.
    parse :: String -> String -> [(String,String)] -> String
    parse str buffer table 
        | length buffer == bufferMaxSize || null str && not (null buffer) = 
            if isSeperator (last buffer) then 
                case backtrack (init buffer) table [] of
                    Nothing -> last buffer : parse str (init buffer) table
                    Just (found, rest) -> last buffer : found ++ parse str (reverse rest) table 
            else
                last buffer : parse str (init buffer) table
    parse (h:t) buffer table = parse t (h:buffer) table
    parse [] buffer _ = []
            
