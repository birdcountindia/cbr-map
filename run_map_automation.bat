@echo off
REM run_map_automation.bat
REM Runs the R automation and shows the R terminal output
REM while simultaneously saving it to the log file.

setlocal

set "RSCRIPT=C:\Program Files\R\R-4.4.1\bin\Rscript.exe"
set "PROJDIR=E:\Abhinandan\BCI\eBird-projects\CBR\CBR-map"
set "RCODE=%PROJDIR%\automate_map_standalone.R"
set "LOGFILE=%PROJDIR%\map_automation_log.txt"

cd /d "%PROJDIR%"

powershell -NoProfile -Command ^
  "& '%RSCRIPT%' '%RCODE%' 2>&1 | Tee-Object -FilePath '%LOGFILE%' -Append"

endlocal