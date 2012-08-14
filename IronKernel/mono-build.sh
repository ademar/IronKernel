cp ../libs/mono/*.dll build
fsharpc Monads.fs Ast.fs Errors.fs Parser.fs SymbolTable.fs Eval.fs Arithmetic.fs Interop.fs Runtime.fs Repl.fs Program.fs --lib:'../libs/mono' --reference:FParsec.dll --out:build/IronKernel.exe
cp *.scm build
	