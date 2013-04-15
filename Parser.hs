{-# LANGUAGE NoMonomorphismRestriction #-}

module Parser (program) where

import IR
import Text.Parsec

type Parser = Parsec String ()

sign = option 1 (char '-' >> return (-1))
integer = (sign >>= \s -> many1 digit >>= \n -> return (s * (read n))) <?> "integer"
parens = between (char '(') (char ')')
brackets = between (char '[') (char ']')

hash = char '#' <?> "#"
colon = char ':' <?> ":"
at = char '@' <?> "@"
aspace = char ' ' <?> "a space"

identifierWithSuf suf = (start >>= \a -> manyTill rest (try suf) >>= \as -> return $ a:as) <?> "identifier-suf"
    where start = letter <|> char '_'
	  rest = start <|> digit
identifier = (start >>= \a -> many rest >>= \as -> return $ a:as) <?> "identifier"
    where start = letter <|> char '_'
	  rest = start <|> digit

operand = choice
    [ try $ string "GP" >> return GP
    , try $ string "FP" >> return FP
    , integer >>= return . C
    , try $ identifierWithSuf (string "_base#") >>= \s -> integer >>= \i -> return $ A s i
    , try $ identifierWithSuf (string "_offset#") >>= \s -> integer >>= \i -> return $ SF s i
    , try $ identifierWithSuf (string "_offset#?") >>= return . DF
    , try $ identifier >>= \s -> hash >> integer >>= \i -> return $ SV s i
    , parens integer >>= return . R
    , try $ identifierWithSuf (string "_type#") >>= \t -> integer >>= \i -> return $ T t i
    , brackets integer >>= return . L
    ] <?> "operand"

zop = choice
    [ string "wrl" >> return Wrl
    , string "entrypc" >> return Entrypc
    , string "nop" >> return Nop
    ] <?> "nullary op"

uop = choice
    [ string "br" >> return Br
    , try $ string "call" >> return Call
    , try $ string "checknull" >> return Checknull
    , string "enter" >> return Enter
    , string "isnull" >> return Isnull
    , string "load" >> return Load
    , try $ string "neg" >> return Neg
    , try $ string "new" >> return New
    , try $ string "newlist" >> return Newlist
    , string "param" >> return Param
    , string "ret" >> return Ret
    , string "write" >> return Write
    ] <?> "unary op"

bop = choice
    [ string "add" >> return Add
    , try $ string "blbc" >> return Blbc
    , try $ string "blbs" >> return Blbs
    , try $ string "checkbounds" >> return Checkbounds
    , try $ string "checktype" >> return Checktype
    , try $ string "cmpeq" >> return Cmpeq
    , try $ string "cmple" >> return Cmple
    , try $ string "cmplt" >> return Cmplt
    , string "div" >> return Div
    , string "istype" >> return Istype
    , string "lddynamic" >> return Lddynamic
    , try $ string "mod" >> return Mod
    , try $ string "move" >> return Move
    , try $ string "mul" >> return Mul
    , try $ string "stdynamic" >> return Stdynamic
    , try $ string "store" >> return Store
    , string "sub" >> return Sub
    ] <?> "binary op"

opcode = choice
    [ try $ zop >>= return . Z
    , try $ uop >>= \u -> aspace >> operand >>= return . U u 
    , bop >>= \b -> aspace >> operand >>= \a -> aspace >> operand >>= return . B b a
    ] <?> "opcode"

typ = (choice
    [ try $ string "int" >> return UInt
    , try $ string "bool" >> return UBool
    , try $ string "Integer" >> return BInt
    , try $ string "Boolean" >> return BBool
    , try $ string "List" >> return List
    , try $ string "dynamic" >> return Dynamic
    , try $ identifier >>= return . Class
    ] >>= \t -> 
    many (char '*') >>= \stars ->
    return $ foldl1 (.) (id:(map (const Pointer) stars)) t)
    <?> "type"

var = (identifier >>= \v -> hash >> integer >>= \i -> colon >> typ >>= \t -> return (v,i,t)) <?> "var"
varList = sepBy1 var aspace

userType = string "type" >> aspace >> 
	   identifier >>= \t -> 
	   colon >> aspace >>
	   varList >>= \vs -> 
	   return $ UserType t vs

method = string "method" >> aspace >>
	 identifier >>= \m ->
	 at >>
	 integer >>= \l ->
	 colon >> aspace >>
	 varList >>= \ps ->
	 return $ Method m l ps

global = string "global" >> aspace >>
	 identifier >>= \v ->
	 hash >>
	 integer >>= \s ->
	 colon >>
	 typ >>= \t ->
	 return $ Global v s t

instruction = string "instr" >> aspace >>
	      integer >>= \r ->
	      colon >> aspace >>
	      opcode >>= \op ->
	      optionMaybe (aspace >> colon >> typ) >>= \mt ->
	      return $ Instruction r op mt

program = sepEndBy userType newline >>= \uts ->
	  sepEndBy method newline >>= \ms ->
	  sepEndBy global newline >>= \gs ->
	  sepEndBy instruction newline >>= \is ->
	  return $ Program uts ms gs is
