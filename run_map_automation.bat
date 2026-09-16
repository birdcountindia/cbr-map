@echo off
REM run_map_automation.bat — standalone launcher for the CBR map's Google
REM Sheet -> git push automation (automate_map_standalone.R). That script
REM loops internally on its own UPDATE_INTERVAL, so this is meant to be
REM started once and left running, not scheduled repeatedly.
setlocal
set "RSCRIPT=C:\Program Files\R\R-4.4.1\bin\Rscript.exe"
set "PROJDIR=E:\Abhinandan\BCI\eBird-projects\CBR\CBR-map"
set "RCODE=%PROJDIR%\automate_map_standalone.R"

cd /d "%PROJDIR%"
"%RSCRIPT%" "%RCODE%" >> "%PROJDIR%\map_automation_log.txt" 2>&1
endlocal
