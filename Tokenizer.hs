module Tokenizer where
    import Data.Map (Map, empty)
    import Text.Parsec.Char (alphaNum)
    import Data.Char (isAlphaNum)
    data Token = TokOrd String | TokEjOrd Char deriving(Show)
    
    
    -- O(n) Läser tills nyrad, returnerar läst data samt återstående text i en tupel
    getWords :: [Char] -> ([Char], [Char])
    getWords (h:t)
        | h == '\n' = ([],t)
        | otherwise = (h:v,t') where
            (v,t') = getWords t
    getWords [] = ([],[])
    -- O(n) Skapar en associativ lista av en csv sträng sepererat av tabb
    createTable :: [Char] -> [Char] -> [([Char], [Char])]
    createTable (h:t) k
        | h == '\t' = (k, v') : createTable t' []
        | otherwise = createTable t (k++[h])
        where (v',t') = getWords t
    createTable [] [] = []
    createTable _ _ = error "ERROR!"
    -- O(n) Skapar en map av en csv sträng sepererat av tabb
    --createMap :: String -> Map String String
    --createMap text = fromList (createTable text [])

    
    seperator :: Char -> Bool
    seperator c 
        | c `elem` " \n\r\t" = True
        | otherwise = False

    tokenizeWord :: String -> String -> (Token,String) 
    tokenizeWord l@(h:t) w   
        | isAlphaNum h = tokenizeWord t (w++[h])
        | otherwise = (TokOrd w, l)  
    tokenizeWord [] w = (TokOrd w, [])

    -- Tokeniserar en sträng 
    tokenize :: String -> [Token]
    tokenize l@(h:t) 
        | isAlphaNum h = tok:tokenize t' 
        | seperator h = tokenize t
        | otherwise = TokEjOrd h : tokenize t
        where 
            (tok, t') = tokenizeWord l []
    tokenize [] = []

    translate :: [Token] -> [(String,String)] -> String
    translate (TokEjOrd eo:toks) table = ' ':eo : translate toks table
    translate (TokOrd ord1 : TokOrd ord2 : toks) table = ' ':ord1 ++ ' ':ord2 ++ translate toks table
    translate (TokOrd ord : TokEjOrd '.' : toks) table = case lookup (ord++".") table of
        Just v -> ' ':v ++ translate toks table
        Nothing -> ' ':ord ++ '.':translate toks table
    translate (TokOrd o:toks) table = o ++ translate toks table
    translate [] table = []
    
    
    