module Tokenizer where
    import Data.Map (Map, empty)
    import Text.Parsec.Char (alphaNum)
    import Data.Char (isAlphaNum)
    data Token = TokOrd String | TokEjOrd Char | TokSep Char
    instance Show Token where
        show (TokOrd s) = s
        show (TokEjOrd c) = [c]
        show (TokSep c) = [c]
    
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
        | seperator h = TokSep h : tokenize t
        | otherwise = TokEjOrd h : tokenize t
        where 
            (tok, t') = tokenizeWord l []
    tokenize [] = []    
    
    