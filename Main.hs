module Main where
    import Tokenizer (createTable, tokenize, Token, translate)
    import GHC.IO.Encoding (getLocaleEncoding)
    
    main :: IO ()
    
    main = do
        f1 <- readFile "kortisar.txt" 
        f2 <- readFile "tjenare.txt"
        let table = createTable f1 []
        let tokens = tokenize f2
        let parsed = translate tokens table
        writeFile "res.txt" parsed
        