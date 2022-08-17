module Main where
    import Parser (parse)

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

    main :: IO ()
    main = do
        f1 <- readFile "kortisar.txt" 
        f2 <- readFile "tests/tjenare.txt"
        let table = createTable f1 []
        let parsed = parse f2 [] table
        writeFile "tests/res.txt" parsed
        