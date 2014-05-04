@echo off

if "x%1" == "x" (
  echo usage: %0 [-p] source.grass
  exit /b
)

setlocal
set mixinfile=%~dp0source.grass
set opt=-J%~dp0 -O
if "%1" == "-p" (
  set opt=%opt% -debug
  shift
)
set output=%~n1.exe

if exist %mixinfile% del %mixinfile%
copy %1 %mixinfile% >NUL


dmd %opt% -of%output% grassctc.d

endlocal
