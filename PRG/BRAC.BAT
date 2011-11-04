@ECHO OFF
set obj=c:\dtp\lib
set lib=c:\dtp\lib
cd..\obj
BLINKER @BRAC
copy dtp.exe c:\dtp\GM\dtp
DEL dtp.EXE
cd..\prg

