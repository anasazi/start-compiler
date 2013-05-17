{-# LANGUAGE NoMonomorphismRestriction #-}
{- Parse input SIF into the SIF data structure -}
module Parser (program, readProgram) where

import SIF
import Text.Parsec
import Control.Monad

-- parser function
readProgram = parse program ""

sign	  = option 1 (char '-' >> return (-1))
num	  = liftM2 (*) sign (liftM read $ many1 digit) <?> "integer"
offset	  = num <?> "offset"
size	  = num <?> "size"
location  = num <?> "location"

hash	= char '#'
colon	= char ':'
at	= char '@'
aspace	= char ' '
star	= char '*'

idFirst = letter <|> char '_'
idRest	= idFirst <|> digit

identiferSuffix suf = liftM2 (:) idFirst (manyTill idRest $ try suf)

identifier = liftM2 (:) idFirst (many idRest)

varWithOffset suf v = liftM2 v (identiferSuffix suf) offset

-- operands
global	      = string "GP" >> return Global
frame	      = string "FP" >> return Frame
constant      = liftM Constant num
address	      = varWithOffset (string "_base#") Address
staticField   = varWithOffset (string "_offset#") StaticField
dynamicField  = liftM DynamicField $ identiferSuffix (string "_offset#?")
stack	      = liftM2 Stack identifier (hash >> offset)
register      = liftM Register $ between (char '(') (char ')') location 
typeuse	      = liftM2 Type (identiferSuffix $ string "_type#") size
codelabel     = liftM Label $ between (char '[') (char ']') location

operand = choice (map try [global, frame, constant, address, staticField, dynamicField, stack, register, typeuse, codelabel]) <?> "operand"

-- SIF types
unboxInt    = string "int" >> return UnboxInt
unboxBool   = string "bool" >> return UnboxBool
boxInt	    = string "Integer" >> return BoxInt
boxBool	    = string "Boolean" >> return BoxBool
boxList	    = string "List" >> return List
boxClass    = liftM Class identifier
boxDynamic  = string "dynamic" >> return Dynamic
baseType = choice . map try $ [unboxInt, unboxBool, boxInt, boxBool, boxList, boxClass, boxDynamic]
pointer = liftM2 f baseType (many star) <?> "type"
  where f t s = foldl1 (.) (id : map (const Pointer) s) t
typesig = colon >> pointer 

spaceOp = aspace >> operand
spaceType = aspace >> typesig

-- SIF side effect opcodes
call = liftM Call (string "call" >> spaceOp)
store = liftM2 Store (string "store" >> spaceOp) spaceOp
move = liftM2 Move (string "move" >> spaceOp) spaceOp
checkbounds = liftM2 Checkbounds (string "checkbounds" >> spaceOp) spaceOp
storedynamic = liftM3 StoreDynamic (string "stdynamic" >> spaceOp) spaceOp spaceOp
write = liftM Write (string "write" >> spaceOp)
wrl = string "wrl" >> return Newline
enter = liftM Enter (string "enter" >> spaceOp)
ret = liftM Ret (string "ret" >> spaceOp)
param = liftM Param (string "param" >> spaceOp)
entrypc = string "entrypc" >> return Entrypc
sideeffect = choice . map try $ [call, store, move, checkbounds, storedynamic, write, wrl, enter, ret, param, entrypc]

-- SIF unary opcodes
neg = string "neg" >> return Neg
isnull = string "isnull" >> return Isnull
load = string "load" >> return Load
new = string "new" >> notFollowedBy alphaNum >> return New
newlist = string "newlist" >> return Newlist
checknull = string "checknull" >> return Checknull
unary = choice . map try $ [neg, isnull, load, new, newlist, checknull]

-- SIF binary opcodes
add = string "add" >> return Add
sub = string "sub" >> return Sub
mul = string "mul" >> return Mul
divide = string "div" >> return Div
modulo = string "mod" >> return Mod
eq = string "cmpeq" >> return Equal
leq = string "cmple" >> return LessEqual
lt = string "cmplt" >> return Less
istype = string "istype" >> return Istype
checktype = string "checktype" >> return Checktype
loaddynamic = string "lddynamic" >> return LoadDyanmic
binary = choice . map try $ [add, sub, mul, divide, modulo, eq, leq, lt, istype, checktype, loaddynamic]

-- SIF branch opcodes
jump = string "br" >> return Jump
ifzero = liftM IfZero $ string "blbc" >> spaceOp
ifset = liftM IfSet $ string "blbs" >> spaceOp
branch = choice . map try $ [jump, ifzero, ifset]

-- SIF opcodes
nop = string "nop" >> return NOP
sideeffectopcode = liftM SideEffect sideeffect
unaryopcode = liftM3 Unary unary spaceOp spaceType
binaryopcode = liftM4 Binary binary spaceOp spaceOp spaceType
branchopcode = liftM2 Branch branch spaceOp
opcode = choice (map try [sideeffectopcode, unaryopcode, binaryopcode, branchopcode, nop]) <?> "opcode"

-- variable list
var = liftM3 (,,) identifier (hash >> size) typesig
varDecl = sepBy1 var aspace

-- top level stuff
typeDecl = liftM2 SIFTypeDecl (string "type" >> aspace >> identifier) (colon >> aspace >> varDecl) <?> "type declaration"
methodDecl = liftM3 SIFMethodDecl (string "method" >> aspace >> identifier) (at >> location) (colon >> option [] (aspace >> varDecl)) <?> "method declaration"
globalDecl = liftM3 SIFGlobalDecl (string "global" >> aspace >> identifier) (hash >> offset) typesig <?> "global declaration"
instruction = liftM2 SIFInstruction (string "instr" >> aspace >> location) (colon >> aspace >> opcode) <?> "instruction"

-- the whole program
lots x = sepEndBy x (newline >> spaces)
program = liftM4 SIFProgram (spaces >> lots typeDecl) (lots methodDecl) (lots globalDecl) (lots instruction)
