module I386ASM(i386asmOut) where

import Cprogram

i386asmOut :: Cprogram -> String
i386asmOut (CP x) = header ++ (concat $ map asmOut' x)
  where header =	   "	.file	\"foo.c\"\n"
			++ "	.version	\"01.01\"\n"
			++ ".text\n"
			++ "	.align 4\n"

asmOut' (DeclVar d s)	= name d ++ ":\n"
			  ++ "\t.ascii " ++ show s ++ "\n"
  where name (Cdecl _ s) = s
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
