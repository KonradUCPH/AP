module Parser.Impl where

import SubsAst
import Text.Parsec.String 
import Text.Parsec (parse)
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec.Error
import Text.Parsec.Prim hiding (token)
import Control.Monad (void)
import Data.Char (isPrint, isAlphaNum)

-- eager parse
munch :: (Char -> Bool) -> Parser String
munch p = do
            as <- many $ satisfy p
            notFollowedBy (satisfy p)
            return as

-- parses a given token, including whitespace
token :: Parser a -> Parser a
token p = do
            a <- p
            pOptWhitespace
            return a

--parses the given symbol, including whitespace
symbol :: String -> Parser ()
symbol s = do
            token $ string s
            return ()
               

-- parses the given keyword, including whitespace
keyword :: String -> Parser ()
keyword s = token $ do 
                        string s
                        notFollowedBy alphaNum

reservedWords :: [String]
reservedWords = ["true", "false", "undefined", "for", "of", "if"]

parseString :: String -> Either ParseError Expr
parseString s = 
    case parse pToEof "Parsing Error" s of
        Right a -> Right a
        Left e -> Left e

pToEof :: Parser Expr
pToEof = do 
            pOptWhitespace -- handle whitespace at start of file 
            expr <- pExpr
            eof -- check for eof after parsing succeeded
            return expr

pExpr :: Parser Expr
pExpr = do
            e1 <- pExpr1
            eCombined <- pExpr' e1
            return $ eCombined

pExpr' :: Expr -> Parser Expr
pExpr' e1 = do
                symbol ","
                e2 <- pExpr
                return $ Comma e1 e2 -- non epsilon: build comma
            <|>
                return e1 -- epslilon expression

pExpr1 :: Parser Expr
pExpr1 =    
        try ( do ident <- pIdent  -- try used for backtracking. Required because of it is.
                 symbol "="
                 e <- pExpr1
                 return $ Assign ident e )
        <|>
            pExpr2

pExpr2 :: Parser Expr
pExpr2 = do
            e1 <- pExpr3
            eCombined <- pExpr2' e1
            return eCombined

pExpr2' :: Expr -> Parser Expr
pExpr2' e1 = do
                symbol "==="
                e2 <- pExpr3 
                return $ Call "===" [e1, e2]
             <|>
             do
                symbol "<"
                e2 <- pExpr3
                return $ Call "<" [e1, e2]
             <|>
                return e1

pExpr3 :: Parser Expr
pExpr3 = do
            e1 <- pExpr4 
            eCombined <- pExpr3Opt e1
            return eCombined

pExpr3Opt :: Expr -> Parser Expr
pExpr3Opt e1 = do
                symbol "+"
                e2 <- pExpr4
                let eCombined = Call "+" [e1, e2] 
                pExpr3Opt eCombined 
             <|>
             do
                symbol "-"
                e2 <- pExpr4
                let eCombined = Call "-" [e1, e2] 
                pExpr3Opt eCombined 
             <|>
                return e1

pExpr4 :: Parser Expr
pExpr4 = do
            e1 <- pExpr5 
            eCombined <- pExpr4Opt e1
            return eCombined

pExpr4Opt :: Expr -> Parser Expr
pExpr4Opt e1 = do
                symbol "*"
                e2 <- pExpr5 
                let eCombined = Call "*" [e1, e2] 
                pExpr4Opt eCombined 
             <|>
             do
                symbol "%"
                e2 <- pExpr5 
                let eCombined = Call "%" [e1, e2] 
                pExpr4Opt eCombined 
             <|>
                return e1

pExpr5 :: Parser Expr
pExpr5 =    pNum 
        <|> pString
        <|> try pTrue
        <|> try pFalse
        <|> try pUndefined
        <|> pBracketedExpr
        <|> pExprIdent
        <|> pExprArray

pNum :: Parser Expr
pNum = do
        symbol "-"
        num <- pDigits 
        return (Number ( - num))
       <|>
       do
        num <- pDigits 
        return (Number (num))

pString :: Parser Expr
pString = token $ do
                    string "'"
                    s <- pStringR ""
                    return (String s)

pTrue :: Parser Expr
pTrue = do 
           keyword "true"
           return TrueConst 

pFalse :: Parser Expr
pFalse = do 
           keyword "false"
           return FalseConst 

pUndefined :: Parser Expr
pUndefined = do 
                keyword "undefined"
                return Undefined 

pBracketedExpr :: Parser Expr
pBracketedExpr = do 
                    symbol "("
                    expr <- pExpr
                    symbol ")"
                    return expr

pExprIdent :: Parser Expr
pExprIdent = do 
                ident <- pIdent 
                pExprIdent' ident

pExprIdent' :: Ident -> Parser Expr
pExprIdent' i = do
                    symbol "("
                    exprs <- pExprs
                    symbol ")"
                    return (Call i exprs)
                <|>
                    return (Var i)

pExprs :: Parser [Expr]
pExprs = do
            e <- pExpr1
            es <- pExprs'
            return (e:es)
         <|>
            return []

pExprs' :: Parser [Expr]
pExprs' = do
            symbol ","
            e <- pExpr1
            es <- pExprs'
            return (e:es)
         <|>
            return []

pExprArray :: Parser Expr
pExprArray = do 
                symbol "["
                pExprArray'

pExprArray' :: Parser Expr 
pExprArray' = try (do 
                    exprs <- pExprs
                    symbol "]"
                    return (Array exprs))
              <|>
              do
                arrayFor <- pArrayFor
                symbol "]"
                return (Compr arrayFor)


pArrayFor :: Parser ArrayCompr 
pArrayFor = do
              keyword "for"
              symbol "("
              ident <- pIdent
              keyword "of"
              expr <- pExpr1
              symbol ")"
              ac <- pArrayCompr
              return (ACFor ident expr ac)

pArrayIf :: Parser ArrayCompr 
pArrayIf = do 
               keyword "if"
               symbol "(" 
               expr <- pExpr1
               symbol ")"
               ac <- pArrayCompr
               return (ACIf expr ac)

pArrayCompr :: Parser ArrayCompr 
pArrayCompr =   try pArrayFor  
              <|>
                try pArrayIf
              <|>  
                do
                e <- pExpr1
                return (ACBody e) 

pIdent :: Parser Ident
pIdent = token $ do
                    c <- letter
                    cs <- many $ satisfy isAlphaNumOrUnderscore 
                    let ident = c:cs
                    if ident `notElem` reservedWords
                        then return $ ident
                        else unexpected "A variable name can't be a reserved word!"


isAlphaNumOrUnderscore :: Char -> Bool
isAlphaNumOrUnderscore '_' = True
isAlphaNumOrUnderscore c = isAlphaNum c

-- parses parts of a string recursively
pStringR :: String -> Parser String
pStringR s1 = do
                -- read string until terminator
                s2 <- munch stringInterruptor
                -- read terminator
                interruptor1 <- anyToken
                case interruptor1 of
                    '\'' -> return $ s1 ++ s2 -- end of String
                    '\\' -> -- read backslash, check for next char
                            do
                                s3 <- pBackshlashChar
                                pStringR $ s1 ++ s2 ++ s3
                    c -> fail $ "Invalid character in string: \"" ++ [c] ++ "\""

-- called after a backslash has ben read   
pBackshlashChar :: Parser String  
pBackshlashChar = do 
                    interruptor2 <- anyToken
                    case interruptor2 of
                        'n' -> return "\n"
                        '\'' -> return "'"
                        't' -> return "\t"
                        '\\' -> return "\\"
                        '\n' -> do  -- skip whitespace - not 
                                    pOptWhitespace
                                    return ""
                        _ -> fail "invalid backslash-sequence in a string"

-- returns false if the string contains an invalid or special caracter
stringInterruptor :: Char -> Bool
stringInterruptor '\'' = False -- normal string termination
stringInterruptor '\\' = False -- a single backslash
stringInterruptor c = isPrint c

pDigits :: Parser Int
pDigits = token $ do
                    c <- many1 digit
                    if length c <= 8 then 
                        return $ read c
                    else
                        fail "Too many digits!"

-- parses optional whitespace
pOptWhitespace :: Parser ()
pOptWhitespace = do
                    a <- many $ oneOf " \n\t"
                    b <- many pComment
                    if (length a) > 0 || (length b) > 0 then
                        do 
                          pOptWhitespace 
                          return ()
                    else 
                        return ()

--parses a comment
pComment :: Parser String
pComment = do
            string "//" -- deliberately not used symbol to allow empty comments
            s <- manyTill anyChar newline
            return s




