\begin{code}
module Cprogram where

import ParseLib

cShow :: Cprogram -> String
cShow = show

parseC = CP . fst . head . papply toplevel . fst . head . papply deComment

--
-- let us think a little and make a proper data structure.
--  (I will have to rethink all of this crap.  Oh well.)

newtype Cprogram = CP [Cconstructs]

instance Show Cprogram where
	show (CP x) = concat (map show x)

-- parsing C:
toplevel = junk >> many ext_decl

data Cconstructs = Decl Cdecl | Func Cfunc | Typedef Cdecl
		   | Structdef String [Cdecl]

instance Show Cconstructs where
	show (Decl x) = show x ++ ";\n"
	show (Func x) = show x ++ "\n"
	show (Typedef x) = "typedef " ++ show x ++ ";\n"
	show (Structdef x y) = "struct " ++ x ++ "{\n" ++ show y ++ "\n};\n"

ext_decl = typedef +++ structdef +++ function +++ ext2_var_decl

typedef = do	symbol "typedef"
		t <- decl
		symbol ";"
		return (Typedef t)

structdef = do	symbol "union"
		name <- token cIdent
		decls <- brace (decl `sepby` token (char ';'))
		symbol ";"
		return (Structdef name decls)

function = do	t <- ftype
		n <- fname
		symbol "("
		a <- fargs
		symbol ")"
		b <- statement_block
		return (Func (Cfunc t n (Args a) b))

data Cdecl = Cdecl Type String
data Cfunc = Cfunc Type String Args StmntBlock

data Type =
	  IntType | LongType | ShortType | CharType
	| DoubleType | FloatType | LongLongType
	| UnsignedType Type | FuncType Type [Cdecl]
	| SizeType | VoidType
	| PtrType Type
	| ConstType Type
	| SignedType Type

instance Show Type where
	show x = showtype x ""

types = [	(IntType, symbol "int"),
		(LongLongType, symbol "long" >> symbol "long" >> (symbol "int" +++ symbol "")),
		(LongType, symbol "long" >> (symbol "int" +++ symbol "")),
		(ShortType, symbol "short" >> (symbol "int" +++ symbol "")),
		(DoubleType, symbol "double"),
		(FloatType, symbol "float"),
		(CharType, symbol "char"),
		(VoidType, symbol "void"),
		(SizeType, symbol "size_t")
	]

decl = typequal >>= (\x -> gtype >>= (\y -> return (x y))) >>= (\x -> decl_rest >>= (\y -> return (y x)))
gtype = foldr (+++) mzero (map p types)
  where	p (x,y) = y >> return (Cdecl x "")

typequal =	(symbol "const" >> return addConst) +++
		(symbol "unsigned" >> return addUnsigned) +++
		(symbol "signed" >> return addSigned) +++
		return id

addUnsigned (Cdecl t n) = Cdecl (UnsignedType t) n
addSigned (Cdecl t n) = Cdecl (SignedType t) n

decl_rest = decl_choice >>= (\x -> decl_func >>= (\y -> return (y . x)))
decl_choice = decl_paren +++ ptr +++ constT +++ getName +++ nothing id
decl_paren = paren decl_rest >>= (\x -> decl_func >>= (\y -> return (x . y)))
ptr = symbol "*" >> (decl_rest >>= (\x -> return (x . addPtr))) --- +++ return PtrType)
constT = symbol "const" >> (decl_rest >>= (\x -> return (x . addConst)))
decl_func = (paren gargs >>= (\x -> return (flip addFunc x))) +++ (return id)
gargs = decl `sepby` token (char ',')

addPtr  (Cdecl t n) = Cdecl (PtrType t) n
addFunc (Cdecl t n) x = Cdecl (FuncType t x) n
addConst (Cdecl t n) = Cdecl (ConstType t) n

getName = token cIdent >>= return . putName
putName n (Cdecl t _) = Cdecl t n

nothing x = symbol "" >> return x
eat x y = x >> return y

ftype = foldr (+++) mzero (map p types)
  where	p (x,y) = y >> return x

newtype Args = Args [Cdecl]

fargs = decl `sepby` token (char ',')
fname = token cIdent

showtype SizeType     n = space_if "size_t" n
showtype VoidType     n = space_if "void" n
-- the above are simple hacks.
showtype CharType     n = space_if "char" n
showtype ShortType    n = space_if "short" n
showtype IntType      n = space_if "int" n
showtype LongType     n = space_if "long" n
showtype LongLongType n = space_if "long long" n
showtype FloatType    n = space_if "float" n
showtype DoubleType   n = space_if "double" n
showtype (UnsignedType t) n = "unsigned " ++ showtype t n
showtype (SignedType t) n = "signed " ++ showtype t n
showtype (FuncType t xs) n = showtype t (n ++ "(" ++
				concatWith ", " (map show xs) ++ ")")
showtype (PtrType t@(FuncType t' xs)) n = showtype t ("(*" ++ n ++ ")")
showtype (PtrType t) n	= showtype t ("*" ++ n)
showtype (ConstType t@(PtrType _)) n = showtype t ("const " ++ n)
showtype (ConstType t) n = "const " ++ showtype t n

space_if x y	| x /= "" && y /= ""	= x ++ " " ++ y
		| x /= ""		= x
		| otherwise		= y

instance Show (Cdecl) where
	show (Cdecl t n) = showtype t n

instance Show Args where
	show (Args x) = concatWith ", " $ map show x

concatWith x [] = []
concatWith x y = foldr1 (\a b -> a ++ x ++ b)  y

instance Show Cfunc where
	show (Cfunc t n a b) = show t ++ "\n" ++ n ++
				"(" ++ show a ++ ")\n" ++
				show b

data StmntBlock = SB [Cdecl] [Stmnt]

instance Show StmntBlock where
	show (SB x y) = "{\n" ++ concat (map (++";\n") (map show x)) ++ concatWith "\n" (map show y) ++ "\n}"

statement_block = do	symbol "{"
			vars <- var_block
			statements <- stmnts
			symbol "}"
			return (SB vars statements)

data Stmnt =
	  IfStmnt Expr Stmnt
	| WhileStmnt Expr Stmnt
	| DoWhileStmnt Stmnt Expr
	| GotoStmnt String
	| Label String
	| ExprStmnt Expr
	| ReturnStmnt Expr
	| BlockStmnt StmntBlock
	| ForStmnt (Expr,Expr,Expr) Stmnt

instance Show Stmnt where
	show (IfStmnt e s)	= "if (" ++ show e ++ ") " ++ show s ++ ";"
	show (WhileStmnt e s)	= "while (" ++ show e ++ ") " ++ show s ++ ";"
	show (DoWhileStmnt s e) = "do " ++ show s ++ " while (" ++ show e ++ ")" ++ ";"
	show (GotoStmnt s)	= "goto " ++ s ++ ";"
	show (Label s)		= s ++ ":"
	show (ExprStmnt e)	= show e ++ ";"
	show (ReturnStmnt e)	= "return " ++ show e ++ ";"
	show (BlockStmnt s)	= show s
	show (ForStmnt e s)	= "for (" ++ forExprShow e ++ ") " ++ show s

forExprShow (e,f,g) = show e ++ "; " ++ show f ++ "; " ++ show g

data Expr =
 -- the `terms'.  Maybe I should actually define them as simply
 -- a `Term' type?  I think that would be good in a while.
	  Num		Int
	| Ident		String
	| StringLit	String

 -- the unary operators
	| LogicalNot	Expr
	| BitwiseNot	Expr
	| MathNegate	Expr
	| AssignPreInc	Expr
	| AssignPostInc	Expr
	| AssignPreDec	Expr
	| AssignPostDec	Expr
	| Cast		Type Expr
	| Sizeof	Expr
	| Reference	Expr
	| DeReference	Expr

 -- the binary operators
	| LogicalAnd	Expr Expr
	| LogicalOr	Expr Expr
	| BitwiseAnd	Expr Expr
	| BitwiseOr	Expr Expr
	| BitwiseXor	Expr Expr
	| MathAdd	Expr Expr
	| MathSubt	Expr Expr
	| MathMult	Expr Expr
	| MathDiv	Expr Expr
	| MathMod	Expr Expr
	| ShiftLeft	Expr Expr
	| ShiftRight	Expr Expr
	| Assign	Expr Expr

	| AssignPlus	Expr Expr
	| AssignSubt	Expr Expr
	| AssignMult	Expr Expr
	| AssignDiv	Expr Expr
	| AssignMod	Expr Expr
	| AssignAnd	Expr Expr
	| AssignOr	Expr Expr
	| AssignXor	Expr Expr
	| AssignRShift	Expr Expr
	| AssignLShift	Expr Expr

	| FuncCall	String [Expr]
	| CompareEq	Expr Expr
	| CompareL	Expr Expr
	| CompareLeq	Expr Expr
	| CompareG	Expr Expr
	| CompareGeq	Expr Expr
	| CompareNeq	Expr Expr

instance Show Expr where
	show (Num n)		= "(" ++ show n ++ ")"
	show (Ident s)		= "(" ++ s ++ ")"
	show (StringLit s)	= "(" ++ show s ++ ")"
	show (LogicalNot x)	= "!(" ++ show x ++ ")"
	show (BitwiseNot x)	= "~(" ++ show x ++ ")"
	show (MathNegate x)	= "-(" ++ show x ++ ")"
	show (AssignPreInc x)	= "(++" ++ show x ++ ")"
	show (AssignPreDec x)	= "(--" ++ show x ++ ")"
	show (Sizeof x)		= "(sizeof(" ++ show x ++ ")"
	show (Reference x)	= "(*" ++ show x ++ ")"
	show (DeReference x)	= "(&" ++ show x ++ ")"
	show (LogicalAnd x y)	= "(" ++ show x ++ "&&" ++ show y ++ ")"
	show (LogicalOr x y)	= "(" ++ show x ++ "||" ++ show y ++ ")"
	show (BitwiseAnd x y)	= "(" ++ show x ++ "&" ++ show y ++ ")"
	show (BitwiseOr x y)	= "(" ++ show x ++ "|" ++ show y ++ ")"
	show (BitwiseXor x y)	= "(" ++ show x ++ "^" ++ show y ++ ")"
	show (MathAdd x y)	= "(" ++ show x ++ "+" ++ show y ++ ")"
	show (MathSubt x y)	= "(" ++ show x ++ "-" ++ show y ++ ")"
	show (MathMult x y)	= "(" ++ show x ++ "*" ++ show y ++ ")"
	show (MathDiv x y)	= "(" ++ show x ++ "/" ++ show y ++ ")"
	show (MathMod x y)	= "(" ++ show x ++ "%" ++ show y ++ ")"
	show (ShiftLeft x y)	= "(" ++ show x ++ "<<" ++ show y ++ ")"
	show (ShiftRight x y)	= "(" ++ show x ++ ">>" ++ show y ++ ")"
	show (Assign x y)	= "(" ++ show x ++ "=" ++ show y ++ ")"
	show (AssignPlus x y)	= "(" ++ show x ++ "+=" ++ show y ++ ")"
	show (AssignSubt x y)	= "(" ++ show x ++ "-=" ++ show y ++ ")"
	show (AssignMult x y)	= "(" ++ show x ++ "*=" ++ show y ++ ")"
	show (AssignDiv x y)	= "(" ++ show x ++ "/=" ++ show y ++ ")"
	show (AssignMod x y)	= "(" ++ show x ++ "%=" ++ show y ++ ")"
	show (AssignAnd x y)	= "(" ++ show x ++ "&=" ++ show y ++ ")"
	show (AssignOr x y)	= "(" ++ show x ++ "|=" ++ show y ++ ")"
	show (AssignXor x y)	= "(" ++ show x ++ "^=" ++ show y ++ ")"
	show (AssignRShift x y)	= "(" ++ show x ++ ">>=" ++ show y ++ ")"
	show (AssignLShift x y)	= "(" ++ show x ++ "<<=" ++ show y ++ ")"
	show (FuncCall x y)	= x ++ "(" ++ concatWith "," (map show y) ++ ")"
	show (CompareEq x y)	= "(" ++ show x ++ "==" ++ show y ++ ")"
	show (CompareL x y)	= "(" ++ show x ++ "<" ++ show y ++ ")"
	show (CompareLeq x y)	= "(" ++ show x ++ "<=" ++ show y ++ ")"
	show (CompareG x y)	= "(" ++ show x ++ ">" ++ show y ++ ")"
	show (CompareGeq x y)	= "(" ++ show x ++ ">=" ++ show y ++ ")"
	show (CompareNeq x y)	= "(" ++ show x ++ "!=" ++ show y ++ ")"

--
--
-- Let's start trying to output assembly from this gunk
--
--

asmOut :: Cprogram -> String
asmOut (CP x) = header ++ (concat $ map asmOut' x) ++ footer
  where	header =   "\t.file   1 \"test3.c\"\n"
		 ++"\t.version        \"01.01\"\n"
		 ++"\t.set noat\n"
		 ++".text\n"
	footer = ".ident \"LCC: (Lambda C) lcc-1.0\"\n"

asmOut' (Decl d)	= "	.comm	" ++ name d ++ "," ++ size d ++ "\n"
  where	name (Cdecl _ s) = s
	size _		 = "8,8"
asmOut' (Func f)	=  "\t.align 5\n"
			++ "\t.globl " ++ name f ++ "\n"
			++ "\t.ent " ++ name f ++ "\n"
			++ decode_in_full f
			++ "\t.end " ++ name f ++ "\n"
  where	name (Cfunc _ n _ _) = n

data Intruct =	  Inst1
		| Inst2

decode_in_full (Cfunc _ _ _ (SB _ [(ReturnStmnt (Num x))])) = "\t.bis $31,"++show x++",$0\n" ++ "\tret $31,($26),1\n"
decode_in_full _ = ""

{-
asmOut :: Cprogram -> String
asmOut (CP x) = header ++ (concat $ map asmOut' x)
  where header =	   "	.file	\"foo.c\"\n"
			++ "	.version	\"01.01\"\n"
			++ ".rolandcc_compiled.:\n"
			++ ".text\n"
			++ "	.align 4\n"

asmOut' (Decl d)	= "	.comm	" ++ name d ++ "," ++ size d ++ "\n"
  where	name (Cdecl _ s) = s
	size _		 = "8,8"
asmOut' (Func f)	= "	.align 4\n"
			++".globl "++name f++"\n"
			++"	.type	" ++ name f ++ ",@function\n"
			++name f ++ ":\n"
			++"\tpushl %ebp\n"
			++"\tmovl %esp,%ebp\n"
			++ decode_in_full f
			++"\tleave\n"
			++"\tret\n"
			++end_mark ++ ":\n"
			++"\t.size\t" ++ name f ++ "," ++ end_mark
			++"-" ++ name f ++ "\n"
  where name (Cfunc _ n _ _) = n
	end_mark = ".INT_funcexit_" ++ name f

decode_in_full f = "What?\n"
-}


--
--  Parser Helpers:

cComment = bracket (string "/*") (many' item) (string "*/")
cxxComment = bracket (string "//") (many' item) (string "\n")
deComment = many ((cComment >> return ' ') +++ (cxxComment >> return ' ')
	    +++ item)

many'              :: Parser a -> Parser [a]
--many' p             = (many1' p `mplus` return [])
many' p             = (return [] `mplus` many1' p)

many1'             :: Parser a -> Parser [a]
many1' p            = do {x <- p; xs <- many' p; return (x:xs)}

paren x = bracket lparen x rparen
lparen = token (char '(')
rparen = token (char ')')

brace x = bracket lbrace x rbrace
lbrace = token (char '{')
rbrace = token (char '}')


ext2_var_decl = ext_var_decl >>= \t -> return (Decl t)

var_block = many ext_var_decl
ext_var_decl = decl >>= eat (symbol ";")

stmnts = many stmnt

stmntlist = [block_stmnt, label_stmnt, goto_stmnt, if_stmnt, for_stmnt,
	     while_stmnt, dowhile_stmnt, return_stmnt, expr_stmnt]

stmnt = foldr (+++) mzero stmntlist

block_stmnt = statement_block >>= return . BlockStmnt

if_stmnt = do	symbol "if"
		e <- paren expr
		s <- stmnt
		return (IfStmnt e s)

for_stmnt = do	symbol "for"
		e <- paren for_expr
		s <- stmnt
		return (ForStmnt e s)

for_expr = do	e <- expr
		token (char ';')
		f <- expr
		token (char ';')
		g <- expr
		return (e,f,g)

while_stmnt = do	symbol "while"
			e <- paren expr
			s <- stmnt
			return (WhileStmnt e s)

dowhile_stmnt = do	symbol "do"
			s <- stmnt
			symbol "while"
			e <- paren expr
			symbol ";"
			return (DoWhileStmnt s e)

goto_stmnt = do	symbol "goto"
		t <- token cIdent		-- XXX: hack
		symbol ";"
		return (GotoStmnt t)

label_stmnt = do	i <- token cIdent	-- XXX: hack
			symbol ":"
			return (Label i)

return_stmnt = do	symbol "return"
			e <- expr
			symbol ";"
			return (ReturnStmnt e)

expr_stmnt = do	e <- expr
		symbol ";"
		return (ExprStmnt e)

expr = expr' 15
expr' n = term >>= rest
  where	rest t = ops t +++ return t
	ops t  = operands n t >>= rest

 -- unary operator?  Unfortunately, these guys do _not_ always come
 -- in front of their arguments.  sigh.  but this will have to work
 -- for now.
operands n t = foldr (+++) mzero (map (\x->x t) (concat (take n meta_oplist)))

--
-- what are all of the C operators?
--	assignment:	=, +=, -=, *=, /=, &=, |=, ^=
--	arithmetic:	+, -, *, /
--	bitwise logic:	&, |, ^, ~
--	strict logic:	&&, ||, !
--	incrementers:	++, --
--	oddballs:	sizeof
--	ptrs:		[], ->,
cast = "cast"

op1list = ["->", "."]
op2list = ["!", "~", "++", "--", "-", cast, "*", "&", "sizeof"]
op7list = ["==", "!="]
--	map (op 5 t) [("<<", ShiftLeft), (">>", ShiftRight)],
op14list = ["=", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", ">>=", "<<="]
op15list = [","]

meta_oplist = map (\(n,p) -> map (op n) p) (zip [1..] mo)
  where mo = [
		[],			-- these are unary, so I defer them.
		[],			-- same with these.
		[("*", MathMult), ("/", MathDiv), ("%", MathMod)],
		[("+", MathAdd), ("-", MathSubt)],
		[("<<", ShiftLeft), (">>", ShiftRight)],
		[("<", CompareL), ("<=", CompareLeq), (">", CompareG),
			(">=", CompareGeq)],
		[("==", CompareEq), ("!=", CompareNeq)],
		[("&", BitwiseAnd)],
		[("^", BitwiseXor)],
		[("|", BitwiseOr)],
		[("&&", LogicalAnd)],
		[("||", LogicalOr)],
		[],			-- ?: ternary operator.  wtf?
		[("=", Assign), ("+=", AssignPlus), ("-=", AssignSubt),
		 ("*=", AssignMult), ("/=", AssignDiv), ("%=", AssignMod),
		 ("&=", AssignAnd), ("|=", AssignOr), ("^=", AssignXor),
		 (">>=", AssignRShift), ("<<=", AssignLShift)],
		[]] :: [[(String, Expr -> Expr -> Expr)]]			-- comma.

op :: Int -> (String, Expr -> Expr -> Expr) -> Expr -> Parser Expr
op n (s,c) t = do	symbol s
			a <- expr' n
			return (c t a)

number = integer >>= return . Num
term  = (unop >>= (\x -> term >>= (\y -> return (x y)))) +++ func_call +++ cConst +++
	token icIdent +++ paren (expr' 15)

func_call = token cIdent >>= (\x -> paren (func_args x))
func_args x = (expr `sepby` token (char ',')) >>= (\y -> return (FuncCall x y))

{-
unop = (symbol "*" +++ symbol "++" +++ symbol "!" +++ symbol "~")
	>> return (Num 1)
-}
unop = 	(symbol "!" >> return LogicalNot) +++
	(symbol "~" >> return BitwiseNot) +++
	(symbol "-" >> return MathNegate) +++
	(symbol "++" >> return AssignPreInc) +++
	(symbol "--" >> return AssignPreDec) +++
	(symbol "sizeof" >> return Sizeof) +++
	(symbol "*" >> return Reference) +++
	(symbol "&" >> return DeReference)

icIdent = cIdent >>= return . Ident
cIdent = many1 (foldr (+++) mzero (map char cChars))

cChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"

data Number = IntT Integer | DoubleT Double
	deriving Show

cConst = number +++ stringlit

iNT = int >>= return . fromIntegral

parsenum = iNT >>= (\x -> (fracpart x >>= return . DoubleT) +++ return (IntT x))
  where fracpart x = do	char '.'
			y <- many digit
			return (fromIntegral x + foldr catdig 0 y)
	catdig m n = (n + fromIntegral (digitToInt m)) / 10

stringlit = bracket (char '"') inside (char '"') >>= return . StringLit
  where inside = many ((char '\\' >> char '"') +++ sat (/='"'))

 --
 -- helpers

 --
 -- old but interesting code?
{-

ptr x = many1 (symbol "*") >>= (\y -> return (ptrWlen x (length y))) >>= decl_rest
ptrWlen x 0 = x
ptrWlen x n = PtrType (ptrWlen x (n-1))

-}
\end{code}
