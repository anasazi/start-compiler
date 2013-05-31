{-# LANGUAGE NoMonomorphismRestriction #-}
{- Parse input SIF into the SIF data structure -}
module Parser (program, readProgram) where

import SIF
import Text.Parsec
import Control.Applicative hiding (Alternative(..))

-- parser function
readProgram = parse program ""

sign	  = option 1 (char '-' >> return (-1))
num	  = (*) <$> sign <*> (read <$> many1 digit) <?> "integer"
offset	  = num <?> "offset"
size	  = num <?> "size"
location  = num <?> "location"

hash	= char '#'
colon	= char ':'
at	= char '@'
aspace	= char ' '
star	= char '*'
parens = between (char '(') (char ')')
brackets = between (char '[') (char ']')

idFirst = letter <|> char '_'
idRest	= idFirst <|> digit

identiferSuffix suf = (:) <$> idFirst <*> manyTill idRest (try suf)

identifier = (:) <$> idFirst <*> many idRest

varWithOffset suf v = v <$> identiferSuffix suf <*> offset

-- operands
global	      = string "GP" >> return Global
frame	      = string "FP" >> return Frame
constant      = Constant <$> num
address	      = varWithOffset (string "_base#") Address
staticField   = varWithOffset (string "_offset#") StaticField
dynamicField  = DynamicField <$> identiferSuffix (string "_offset#?")
stack	      = Stack <$> identifier <*> (hash >> offset)
register      = Register <$> parens location 
typeuse	      = Type <$> identiferSuffix (string "_type#") <*> size
codelabel     = Label <$> brackets location

operand = choice (map try [global, frame, constant, address, staticField, dynamicField, register, typeuse, stack, codelabel]) <?> "operand"

-- SIF types
unboxInt    = string "int" >> return UnboxInt
unboxBool   = string "bool" >> return UnboxBool
boxInt	    = string "Integer" >> return BoxInt
boxBool	    = string "Boolean" >> return BoxBool
boxList	    = string "List" >> return List
boxClass    = Class <$> identifier
boxDynamic  = string "dynamic" >> return Dynamic
baseType = choice . map try $ [unboxInt, unboxBool, boxInt, boxBool, boxList, boxClass, boxDynamic]
pointer = f <$> baseType <*> many star <?> "type"
  where f t s = foldl1 (.) (id : map (const Pointer) s) t
typesig = colon >> pointer 

spaceOp = aspace >> operand
spaceType = aspace >> typesig

-- SIF side effect opcodes
call = Call <$> (string "call" >> spaceOp)
store = Store <$> (string "store" >> spaceOp) <*> spaceOp
move = Move <$> (string "move" >> spaceOp) <*> spaceOp
checkbounds = Checkbounds <$> (string "checkbounds" >> spaceOp) <*> spaceOp
storedynamic = StoreDynamic <$> (string "stdynamic" >> spaceOp) <*> spaceOp <*> spaceOp
write = Write <$> (string "write" >> spaceOp)
wrl = string "wrl" >> return Newline
enter = Enter <$> (string "enter" >> spaceOp)
ret = Ret <$> (string "ret" >> spaceOp)
param = Param <$> (string "param" >> spaceOp)
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
ifzero = IfZero <$> (string "blbc" >> spaceOp)
ifset = IfSet <$> (string "blbs" >> spaceOp)
branch = choice . map try $ [jump, ifzero, ifset]

-- SIF opcodes
nop = string "nop" >> return NOP
sideeffectopcode = SideEffect <$> sideeffect
unaryopcode = Unary <$> unary <*> spaceOp <*> spaceType
binaryopcode = Binary <$> binary <*> spaceOp <*> spaceOp <*> spaceType
branchopcode = Branch <$> branch <*> spaceOp
opcode = choice (map try [sideeffectopcode, unaryopcode, binaryopcode, branchopcode, nop]) <?> "opcode"

-- variable list
varDecl = SIFVarDecl <$> identifier <*> (hash >> size) <*> typesig
varDeclList = sepBy1 varDecl aspace

-- top level stuff
typeDecl = SIFTypeDecl <$> (string "type" >> aspace >> identifier) <*> (colon >> aspace >> varDeclList) <?> "type declaration"
methodDecl = SIFMethodDecl <$> (string "method" >> aspace >> identifier) <*> (at >> location) <*> (colon >> option [] (aspace >> varDeclList)) <?> "method declaration"
globalDecl = SIFGlobalDecl <$> (string "global" >> aspace >> identifier) <*> (hash >> offset) <*> typesig <?> "global declaration"
instruction = SIFInstruction <$> (string "instr" >> aspace >> location) <*> (colon >> aspace >> opcode) <?> "instruction"

-- the whole program
lots x = sepEndBy x (newline >> spaces)
program = SIFProgram <$> (spaces >> lots typeDecl) <*> lots methodDecl <*> lots globalDecl <*> lots instruction
