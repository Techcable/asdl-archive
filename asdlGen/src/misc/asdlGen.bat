@echo off
REM a script to run SML/NJ on Windows NT/95

%ASDL_HOME%\runtimes\run.x86-win32.exe @SMLload=%ASDL_HOME%\heaps\asdlGen.x86-win32 %1 %2 %3 %4 %5 %6 %7 %8 %9

