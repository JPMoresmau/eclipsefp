@echo off

ghc -fglasgow-exts -c ParserImpl.hs 
ghc -I%JAVA_HOME%/include -I%JAVA_HOME%/include/win32 -optc -mno-cygwin -c bridge.c
ghc --mk-dll -optdll --add-stdcall-alias -o nativeparser.dll ParserImpl.o ParserImpl_stub.o bridge.o -package base -package haskell-src
copy nativeparser.dll ..\os\win32\x86\
del *.dll *.o *.hi *stub.c *stub.h
echo Done