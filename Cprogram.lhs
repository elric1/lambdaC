\begin{code}
module Cprogram where

import ParseLib

parseC = CP . fst . head . papply toplevel . fst . head . papply deComment

--
-- let us think a little and make a proper data structure.
--  (I will have to rethink all of this crap.  Oh well.)

newtype Cprogram = CP [Cconstructs]
	deriving Show

{-
instance Show Cprogram where
	show (CP x) = concat (map show x)
-}

-- parsing C:
toplevel = junk >> many ext_decl

data Cconstructs = Decl Cdecl | Func Cfunc | Typedef Cdecl
		   | Structdef String [Cdecl] | DeclVar Cdecl String
	deriving Show

{-
instance Show Cconstructs where
	show (Decl x) = show x ++ ";\n"
	show (Func x) = show x ++ "\n"
	show (Typedef x) = "typedef " ++ show x ++ ";\n"
	show (Structdef x y) = "struct " ++ x ++ "{\n" ++ show y ++ "\n};\n"
	show (DeclVar x y) = show x ++ " = " ++ show y ++ ";\n"
-}

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
	deriving Show
data Cfunc = Cfunc Type String Args StmntBlock
	deriving Show

data Type =
	  IntType | LongType | ShortType | CharType
	| DoubleType | FloatType | LongLongType
	| UnsignedType Type | FuncType Type [Cdecl]
	| SizeType | VoidType
	| PtrType Int Type -- size of array is Int?  Integer would be better
	| ConstType Type
	| SignedType Type
	deriving Show

{-
instance Show Type where
	show x = showtype x ""
-}

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

decl_rest = decl_choice >>= (\x -> decl_post >>= (\y -> return (y . x)))
decl_choice = decl_paren +++ ptr +++ constT +++ getName +++ nothing id
decl_paren = paren decl_rest >>= (\x -> decl_post >>= (\y -> return (x . y)))
ptr = symbol "*" >> (decl_rest >>= (\x -> return (x . addPtr))) --- +++ return PtrType)
constT = symbol "const" >> (decl_rest >>= (\x -> return (x . addConst)))

decl_post = many (decl_func +++ decl_arry) >>= (\x -> return (foldr (.) id x))
decl_func = (paren gargs >>= (\x -> return (flip addFunc x)))
decl_arry = sbracket arry_brkt >>= (\x -> return (addArry x))
arry_brkt = int +++ (nothing "" >> return 0)
gargs = decl `sepby` token (char ',')

sbracket x = bracket lbracket x rbracket
  where	lbracket = token (char '[')
	rbracket = token (char ']')

addPtr = addArry 0
addArry  s (Cdecl t n) = Cdecl (PtrType s t) n
addFunc  (Cdecl t n) x = Cdecl (FuncType t x) n
addConst (Cdecl t n)   = Cdecl (ConstType t) n

getName = token cIdent >>= return . putName
putName n (Cdecl t _) = Cdecl t n

nothing x = symbol "" >> return x
eat x y = x >> return y

ftype = foldr (+++) mzero (map p types)
  where	p (x,y) = y >> return x

newtype Args = Args [Cdecl]
	deriving Show

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
showtype (PtrType 0 t@(FuncType t' xs)) n = showtype t ("(*" ++ n ++ ")")
showtype (PtrType s t@(FuncType t' xs)) n = showtype t ("(" ++ n ++ "[" ++ show s ++ "])")
showtype (PtrType 0 t) n	= showtype t ("*" ++ n)
showtype (PtrType s t) n	= showtype t (n ++ "[" ++ show s ++ "]")
showtype (ConstType t@(PtrType _ _)) n = showtype t ("const " ++ n)
showtype (ConstType t) n = "const " ++ showtype t n

space_if x y	| x /= "" && y /= ""	= x ++ " " ++ y
		| x /= ""		= x
		| otherwise		= y

{-
instance Show (Cdecl) where
	show (Cdecl t n) = showtype t n

instance Show Args where
	show (Args x) = concatWith ", " $ map show x
-}

concatWith x [] = []
concatWith x y = foldr1 (\a b -> a ++ x ++ b)  y

{-
instance Show Cfunc where
	show (Cfunc t n a b) = show t ++ "\n" ++ n ++
				"(" ++ show a ++ ")\n" ++
				show b
-}

data StmntBlock = SB [Cdecl] [Stmnt]
	deriving Show

{-
instance Show StmntBlock where
	show (SB x y) = "{\n" ++ concat (map (++";\n") (map show x)) ++ concatWith "\n" (map show y) ++ "\n}"
-}

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
	deriving Show

{-
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
-}

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

	| CompareEq	Expr Expr
	| CompareL	Expr Expr
	| CompareLeq	Expr Expr
	| CompareG	Expr Expr
	| CompareGeq	Expr Expr
	| CompareNeq	Expr Expr

	| FuncCall	Expr [Expr]

 -- the ternary operator
	| IfThenElse	Expr Expr Expr
	deriving Show

{-
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
	show (DeReference x)	= "(*" ++ show x ++ ")"
	show (Reference x)	= "(&" ++ show x ++ ")"
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
	show (FuncCall x y)	= show x++"("++concatWith "," (map show y)++")"
	show (CompareEq x y)	= "(" ++ show x ++ "==" ++ show y ++ ")"
	show (CompareL x y)	= "(" ++ show x ++ "<" ++ show y ++ ")"
	show (CompareLeq x y)	= "(" ++ show x ++ "<=" ++ show y ++ ")"
	show (CompareG x y)	= "(" ++ show x ++ ">" ++ show y ++ ")"
	show (CompareGeq x y)	= "(" ++ show x ++ ">=" ++ show y ++ ")"
	show (CompareNeq x y)	= "(" ++ show x ++ "!=" ++ show y ++ ")"
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

func_call = token icIdent >>= (\x -> paren (func_args x))
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
	(symbol "*" >> return DeReference) +++
	(symbol "&" >> return Reference)

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
 -- so, let's now munge up our data a little bit...
 --  (right now we just pull out the string literals)

munge (CP xs) = CP (concat (map munge' xs))

munge' (Func x) = fmunge x
munge' x = [x]

fmunge (Cfunc t s a (SB d sb)) = moredecls ++ [(Func (Cfunc t s a (SB d nsb)))]
  where	(moredecls, nsb) = unstringlit sb

unstringlit xs =foldr (\(x,y) (x',y')->(x++x',y:y')) ([],[]) (map us' xs)
  where us' t = traverseS t

traverseS (IfStmnt e s) = (e'++s', (IfStmnt e'' s''))
  where	(e',e'') = traverseE e
	(s',s'') = traverseS s
traverseS (WhileStmnt e s) = (e'++s', (WhileStmnt e'' s''))
  where	(e',e'') = traverseE e
	(s',s'') = traverseS s
traverseS (ForStmnt es s) = (es'++s', (ForStmnt es'' s''))
  where	(es',es'') = traverseEs es
	(s',s'') = traverseS s
traverseS (DoWhileStmnt s e) = (e'++s', (DoWhileStmnt s'' e''))
  where	(e',e'') = traverseE e
	(s',s'') = traverseS s
traverseS (ExprStmnt e) = (e', (ExprStmnt e''))
  where	(e',e'') = traverseE e
traverseS (ReturnStmnt e) = (e', (ReturnStmnt e''))
  where	(e',e'') = traverseE e
traverseS t = ([],t)

traverseEs (e1,e2,e3) = (e1'++e2'++e3', (e1'',e2'',e3''))
  where (e1',e1'') = traverseE e1
	(e2',e2'') = traverseE e2
	(e3',e3'') = traverseE e3

traverseE (LogicalNot e)	= (e', LogicalNot e'')
  where (e',e'') = traverseE e
traverseE (BitwiseNot e)	= (e', BitwiseNot e'')
  where (e',e'') = traverseE e
traverseE (MathNegate e)	= (e', MathNegate e'')
  where (e',e'') = traverseE e
traverseE (AssignPreInc	 e)	= (e', AssignPreInc e'')
  where (e',e'') = traverseE e
traverseE (AssignPostInc e)	= (e', AssignPostInc e'')
  where (e',e'') = traverseE e
traverseE (AssignPreDec	 e)	= (e', AssignPreDec e'')
  where (e',e'') = traverseE e
traverseE (AssignPostDec e)	= (e', AssignPostInc e'')
  where (e',e'') = traverseE e
traverseE (Cast	t e)		= (e', Cast t e'')
  where (e',e'') = traverseE e
traverseE t@(Sizeof _)		= ([],t)
traverseE (Reference e)		= (e', Reference e'')
  where (e',e'') = traverseE e
traverseE (DeReference e)	= (e', Reference e'')
  where (e',e'') = traverseE e

traverseE (FuncCall s es)	= (es', FuncCall s es'')
  where (es', es'') = foldr (\(x,y) (x',y')->(x++x',y:y')) ([],[]) nexus
	nexus = map traverseE es

traverseE (StringLit s)		= ([d], i)
  where d = (DeclVar (Cdecl (PtrType ((length s)+1) CharType) "globalString") s)
	i = (Ident "globalString")

traverseE t			= ([], t)

 -- okay, should I figure out how to make the above crap into an
 -- instance of class functor?  How would I do that, given that I'd
 -- need to define an `fmap'...


 -- Storage Class Specifiers
tTypedef	= symbol "typedef"	>> return id
tExtern		= symbol "extern"	>> return id
tStatic		= symbol "static"	>> return id
tAuto		= symbol "auto"		>> return id
tRegister	= symbol "register"	>> return id

storage_specifier = tTypedef +++ tExtern +++ tStatic +++ tAuto +++ tRegister

 -- Type Qualifiers
tConst		= symbol "const"	-- >> return id
tRestrict	= symbol "restrict"	-- >> return id
tVolatile	= symbol "volatile"	-- >> return id

typequal' = many (tConst +++ tRestrict +++ tVolatile)

 -- Non-terminal Type Specifiers
tLong		= symbol "long"		-- >> return id
tShort		= symbol "short"	-- >> return id
tSigned		= symbol "signed"	-- >> return id
tUnsigned	= symbol "unsigned"	-- >> return id

nontermtypes = many (tLong +++ tShort +++ tSigned +++ tUnsigned)

 -- Terminal Type Specifiers
kVoid		= symbol "void"		>> return id
kChar		= symbol "char"		>> return id
kInt		= symbol "int"		>> return id
kFloat		= symbol "float"	>> return id
kDouble		= symbol "double"	>> return id
k_Bool		= symbol "_Bool"	>> return id
k_Complex	= symbol "_Complex"	>> return id
k_Imaginary	= symbol "_Imaginary"	>> return id

termtypes = kVoid +++ kChar +++ kInt +++ kFloat +++ kDouble
	+++ k_Bool +++ k_Complex +++ k_Imaginary

 -- struct/union, etc.
kStruct		= symbol "struct"	>> return id
kUnion		= symbol "union"	>> return id

declaration = storage_specifier >> typequal' >> nontermtypes >> termtypes


 --
 -- old but interesting code?
{-

ptr x = many1 (symbol "*") >>= (\y -> return (ptrWlen x (length y))) >>= decl_rest
ptrWlen x 0 = x
ptrWlen x n = PtrType (ptrWlen x (n-1))

-}

\end{code}
