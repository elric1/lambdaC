\begin{code}
module AlphaASM where

import Cprogram

--
--
-- Let's start trying to output assembly from this gunk
--
--

alphaAsmOut :: Cprogram -> String
alphaAsmOut (CP x) = header ++ (concat $ map asmOut' x) ++ footer
  where	header =   "\t.file   1 \"test3.c\"\n"
		 ++"\t.version        \"01.01\"\n"
		 ++"\t.set noat\n"
		 ++".text\n"
	footer = "\t.ident \"LCC: (Lambda C) lcc-1.0\"\n"

asmOut' (DeclVar d s)	= name d ++ ":\n"
			  ++ "\t.ascii \"" ++ quote s ++ "\\0\"\n"
  where name (Cdecl _ s) = s
	quote = id
asmOut' (Decl d)	= "	.comm	" ++ name d ++ "," ++ size d ++ "\n"
  where	name (Cdecl _ s) = s
	size _		 = "8,8"
asmOut' (Func f)	=  "\t.align 5\n"
			++ "\t.globl " ++ name f ++ "\n"
			++ "\t.ent " ++ name f ++ "\n"
			++ name f ++ ":\n"
			++ "\t.frame $30," ++ show stacksize ++ ",$26,0\n"
			++ "\tldgp $29,0($27)\n"
			++ setupstack
			++ "\tstq $26,0($30)\n"
			++ "$" ++ name f ++ "..ng:\n"
			++ "\t.prologue 0\n"
			++ decode f
			++ {-name f-}"main" ++ "_bail:\n"
			++ "\tldgp $29,0($26)\n"
			++ "\tldq $26,0($30)\n"
			++ killstack
			++ "\tret $31,($26),1\n"
			++ "\t.end " ++ name f ++ "\n"
  where	name (Cfunc _ n _ _) = n
	stacksize = 16
	setupstack | stacksize == 0 = ""
		   | otherwise	    = "\tsubq $30," ++show stacksize ++ ",$30\n"
	killstack  | stacksize == 0 = ""
		   | otherwise	    = "\taddq $30," ++show stacksize ++ ",$30\n"

decode (Cfunc _ _ _ (SB _ xs)) = concat (map decodeS xs)

decodeS (ExprStmnt x) = decodeE 0 x
decodeS (ReturnStmnt x) = decodeE 0 x ++ "\tbr main_bail\n"
decodeS _ = "unknown statement\n"

decodeE reg (Num x)		= "\tbis $31," ++ show x ++ ",$" ++ show reg
				  ++ "\n"
decodeE reg (Ident s)		= "\tlda $" ++ show reg
				  ++ "," ++ s ++ "\n"
-- dereferencing really requires a little more, well, size information
-- than I am using here, doesn't it?
decodeE reg (DeReference (Reference e)) = decodeE reg e
decodeE reg (DeReference e)	= decodeE reg e
				  ++ "\tldq $" ++ show reg ++ ",0($"
				  ++ show reg ++ ")\n"
-- hmmm, this one is rather dependent on the underlying data, isn't
-- it?..  sigh.
decodeE reg (Reference (DeReference e)) = decodeE reg e
decodeE reg (Reference e)	= decodeE reg e
				  ++ "\tlda $" ++ show reg ++ ",0($"
				  ++ show reg ++ ")\n"
decodeE reg (MathAdd x y)	= decodeE reg x ++ decodeE (reg+1) y
				  ++ "\taddl $" ++ show reg ++ ",$"
				  ++ show (reg+1) ++ ",$" ++ show reg ++ "\n"
decodeE reg (MathSubt x y)	= decodeE reg x ++ decodeE (reg+1) y
				  ++ "\tsubl $" ++ show reg ++ ",$"
				  ++ show (reg+1) ++ ",$" ++ show reg ++ "\n"
-- this FuncCall definition only allows a small number of
-- function arguments, so far.
decodeE reg (FuncCall (Ident x) y)	= concat (map (uncurry decodeE) (zip [16..21] y))
				  ++ "\tjsr $26," ++ x ++ "\n"
				  ++ "\tldgp $29,0($26)\n"
decodeE reg (Assign e@(Ident x) f) = decodeE reg f
				     ++ decodeE (reg+1) e
				     ++ "\tstq $" ++ show reg ++ ",0($"
				     ++ show (reg+1) ++ ")\n"
decodeE _ _			= "unknown expression\n"

\end{code}


        .align 5
        .globl main
        .ent main
main:
        .frame $30,16,$26,0
        .mask 0x4000000,-16
        ldgp $29,0($27)
$main..ng:
        subq $30,16,$30
        stq $26,0($30)
        .prologue 1
        lda $16,$LC0
        jsr $26,printf
        ldgp $29,0($26)
        ldq $26,0($30)
        addq $30,16,$30
        ret $31,($26),1
        .end main

