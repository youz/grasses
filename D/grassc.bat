@echo off

if "x%1" == "x" (
  echo usage: %0 [-p] source.grass
  exit /b
)

setlocal
set srcdir=%~dp0
set dsrc=%srcdir%grassctc.d
set grasssrc=%srcdir%source.grass

set dflag=-J%srcdir% -O
if "%1" == "-p" (
  set opt=%opt% -debug
  shift
)
set output=%~n1.exe

if exist %grasssrc% del %grasssrc%
copy /Y %1 %grasssrc% >NUL


dmd %dflag% -of%output% %dsrc%

endlocal
