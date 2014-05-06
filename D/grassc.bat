@echo off

if "x%1" == "x" (
  echo usage: %0 [-p] source.grass
  exit /b
)

setlocal
set dflag=-O

set srcdir=%~dp0
set dsrc=%srcdir%grassctc.d
set grasssrc=%srcdir%source.grass

if "%1" == "-p" (
  set dflag=%dflag% -debug
  shift
)
set output=%~n1.exe

if exist %grasssrc% del %grasssrc%
copy /Y %1 %grasssrc% >NUL


dmd %dflag% -J%srcdir% -of%output% %dsrc%

endlocal
