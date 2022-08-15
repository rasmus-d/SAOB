module Parser where
    import Tokenizer ( Token(..) )
    translate :: [Token] -> [(String,String)] -> String
    translate (TokSep sep : toks) table = sep : translate toks table
    translate (TokEjOrd eo : toks) table = eo : translate toks table
    translate (TokOrd ord1 : TokOrd ord2 : toks) table = ord1 ++ ord2 ++ translate toks table
    translate (TokOrd ord : TokEjOrd '.' : toks) table = case lookup (ord++".") table of
        Just v -> ord ++ "." ++ '[':v++"]" ++ translate toks table
        Nothing -> ord ++ "." ++ translate toks table
    translate (TokOrd o:toks) table = o ++ translate toks table
    translate [] table = []

    -- Längsta förkortningen: i st. f.
    -- Tokenizerad blir denna: TokOrd "i", TokSeperator ' ', TokOrd "st", TokEjOrd '.', TokSep ' ' TokOrd "f", TokEjOrd '.'
    -- Vilket är 7 tokens och 8 chars

    parse :: [Token] -> String -> [(String,String)] -> String
    parse toks buffer table 
        | length buffer > 8 = parse toks [] table -- spola buffern.
