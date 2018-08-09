module Lib where

import Text.Parsec.Char
import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.Token
import Text.Read (readMaybe)

type TypeName = String
type MemberName = String
type StructName = String
type ArrayCount = Int
data Member = Member TypeName MemberName ArrayCount deriving (Eq, Show)
data Struct = Struct StructName [Member] deriving (Eq, Show)

name :: Parsec String () String
name = many1 $ alphaNum <|> oneOf "_"

arrayMember :: Parsec String () (String, Int)
arrayMember = do
    memberName <- name
    char '['
    count <- many1 digit
    char ']'
    case readMaybe count of
        Nothing -> parserFail "invalid array number"
        Just c -> return (memberName, c)


skipPadding :: Parsec String () ()
skipPadding = skipMany $ oneOf " \t"

line :: Parsec String () Member
line = do
    spaces
    typeName <- name
    skipPadding
    (memberName, count) <- try arrayMember <|> (name >>= \n -> return (n, 0))
    skipPadding
    char ';'
    spaces
    return $ Member typeName memberName count

struct :: Parsec String () (Maybe String, [Member])
struct = do
    spaces
    string "struct"
    skipPadding
    headerName <- optionMaybe name
    skipPadding
    members <- between (char '{') (char '}') (many1 line)
    return (headerName, members)


typedefStruct :: Parsec String () Struct
typedefStruct = do
    skipPadding
    string "typedef"
    (_, members) <- struct
    skipPadding
    structName <- name
    return $ Struct structName members
    






