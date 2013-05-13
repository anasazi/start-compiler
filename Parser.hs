{-# LANGUAGE NoMonomorphismRestriction #-}

module Parser (parseProgram) where

import IR
import Text.Parsec
import Control.Monad

parseProgram = parse program "" 

type Parser = Parsec String ()

sign = option 1 (char '-' >> return (-1))
integer = (sign >>= \s -> many1 digit >>= \n -> return (s * read n)) <?> "integer"
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
    [ try $ string "GP" >> return (Const GP)
    , try $ string "FP" >> return (Const FP)
    , liftM (Const . C) integer
    {-
    , try $ identifierWithSuf (string "_base#") >>= \s -> integer >>= \i -> return $ Var (A s i)
    , try $ identifierWithSuf (string "_offset#") >>= \s -> integer >>= \i -> return $ Var (SF s i)
    , try $ identifierWithSuf (string "_offset#?") >>= \s -> return $ Var (DF s)
    -}
    , try $ identifierWithSuf (string "_base#") >>= \s -> integer >>= \i -> return $ Const (A s i)
    , try $ identifierWithSuf (string "_offset#") >>= \s -> integer >>= \i -> return $ Const (SF s i)
    , try $ identifierWithSuf (string "_offset#?") >>= \s -> return $ Const (DF s)

    , try $ identifierWithSuf (string "_type#") >>= \t -> integer >>= \i -> return . Const $ T t i
    , try $ identifier >>= \s -> hash >> integer >>= \i -> return $ Var (SV s i) 
    , liftM (Const . R) (parens integer)
    , liftM (Const . L) (brackets integer)
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
    , try $ string "newlist" >> return Newlist
    , try $ string "new" >> return New
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
    , try $ string "store" >> return Store
    , string "sub" >> return Sub
    ] <?> "binary op"

top = choice
    [ string "stdynamic" >> return Stdynamic
    ]

opcode = choice
    [ try $ liftM Z zop 
    , try $ uop >>= \u -> liftM (U u) (aspace >> operand)
    , try $ bop >>= \b -> aspace >> operand 
		>>= \a -> liftM (B b a) (aspace >> operand)
    , top >>= \t -> aspace >> operand 
	  >>= \a -> aspace >> operand 
	  >>= \b -> liftM (Ter t a b) (aspace >> operand)
    ] <?> "opcode"

typ = (choice
    [ try $ string "int" >> return UInt
    , try $ string "bool" >> return UBool
    , try $ string "Integer" >> return BInt
    , try $ string "Boolean" >> return BBool
    , try $ string "List" >> return List
    , try $ string "dynamic" >> return Dynamic
    , try $ liftM Class identifier
    ] >>= \t -> 
    many (char '*') >>= \stars ->
    return $ foldl1 (.) (id : map (const Pointer) stars) t)
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
	 colon >> option [] (aspace >>
	 varList) >>= \ps ->
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

program = spaces >>
	  sepEndBy userType (newline >> spaces) >>= \uts ->
	  sepEndBy method (newline >> spaces) >>= \ms ->
	  sepEndBy global (newline >> spaces) >>= \gs ->
	  sepEndBy instruction (newline >> spaces) >>= \is ->
	  return $ Program uts ms gs is
