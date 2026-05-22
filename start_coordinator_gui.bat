@echo off
setlocal enabledelayedexpansion
title Federated Statistics - Coordinator GUI

cd /d "%~dp0"

echo ========================================
echo  Federated Statistics - Coordinator GUI
echo ========================================
echo.

:: ---- Find Rscript ---------------------------------------------------
set "RSCRIPT="

where Rscript >nul 2>&1
if !errorlevel! equ 0 (
    set "RSCRIPT=Rscript"
    goto :r_found
)

for /d %%D in ("C:\Program Files\R\R-*") do (
    if exist "%%D\bin\Rscript.exe" (
        set "RSCRIPT=%%D\bin\Rscript.exe"
        set "PATH=%%D\bin;!PATH!"
        echo [INFO] R found at %%D  - added to PATH for this session.
        goto :r_found
    )
)
for /d %%D in ("C:\Program Files (x86)\R\R-*") do (
    if exist "%%D\bin\Rscript.exe" (
        set "RSCRIPT=%%D\bin\Rscript.exe"
        set "PATH=%%D\bin;!PATH!"
        echo [INFO] R found at %%D  - added to PATH for this session.
        goto :r_found
    )
)

echo [ERROR] R is not installed or not found on this computer.
echo.
echo Please download and install R from:
echo   https://cran.r-project.org/bin/windows/base/
echo.
pause
exit /b 1

:r_found
for /f "tokens=* usebackq" %%V in (`!RSCRIPT! --version 2^>^&1`) do (
    echo [OK] %%V
    goto :r_ver_done
)
:r_ver_done
echo.

:: ---- Install required R packages ------------------------------------
echo Checking required R packages (shiny, httr, jsonlite)...
!RSCRIPT! -e "pkgs<-c('shiny','httr','jsonlite'); need<-pkgs[!pkgs %%in%% rownames(installed.packages())]; if(length(need)==0) cat('  All packages already installed.\n') else { cat('  Installing:',paste(need,collapse=', '),'\n'); install.packages(need,repos='https://cloud.r-project.org'); cat('  Done.\n') }"
if !errorlevel! neq 0 (
    echo [ERROR] Package installation failed. Check your internet connection.
    pause
    exit /b 1
)
echo.

:: ---- Check Tailscale ------------------------------------------------
set "MY_IP="
for /f "tokens=* usebackq" %%I in (`tailscale ip -4 2^>nul`) do (
    set "MY_IP=%%I"
    goto :ts_done
)
:ts_done

if not "!MY_IP!"=="" (
    echo [OK] Your Tailscale IP: !MY_IP!
) else (
    echo [WARNING] Tailscale not connected - make sure all sites are reachable.
)
echo.

:: ---- Start the Shiny coordinator app --------------------------------
echo ========================================
echo  Starting coordinator interface...
echo.
echo  Your browser will open automatically.
echo  Keep this window open while running the analysis.
echo  Close this window to stop the server.
echo ========================================
echo.

!RSCRIPT! -e "shiny::runApp('coordinator_app.R', launch.browser = TRUE)"

echo.
pause
