clipper dtpsu -l -o..\obj
if errorlevel 1 goto exit 
cd ..\obj
blinker @dtpsu
copy dtpsu.exe h:\dtp\vw\dtp
del dtpsu.exe
cd ..\PRG
