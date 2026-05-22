@echo off
setlocal enabledelayedexpansion
title Federated Statistics - Site Server

cd /d "%~dp0"

echo ========================================
echo  Federated Statistics - Site Server
echo ========================================
echo.

:: ---- Find Rscript ---------------------------------------------------
set "RSCRIPT="

where Rscript >nul 2>&1
if !errorlevel! equ 0 (
    set "RSCRIPT=Rscript"
    goto :r_found
)

:: Search the standard R install locations
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

:: R not found
echo [ERROR] R is not installed or not found on this computer.
echo.
echo Please download and install R from:
echo   https://cran.r-project.org/bin/windows/base/
echo.
echo After installing R, close this window and double-click this file again.
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
echo Checking required R packages (plumber, jsonlite)...
!RSCRIPT! -e "pkgs<-c('plumber','jsonlite'); need<-pkgs[!pkgs %%in%% rownames(installed.packages())]; if(length(need)==0) cat('  All packages already installed.\n') else { cat('  Installing:',paste(need,collapse=', '),'\n'); install.packages(need,repos='https://cloud.r-project.org'); cat('  Done.\n') }"
if !errorlevel! neq 0 (
    echo [ERROR] Package installation failed. Check your internet connection.
    pause
    exit /b 1
)
echo.

:: ---- Check Tailscale ------------------------------------------------
echo Checking Tailscale...
set "TAILSCALE_IP="
for /f "tokens=* usebackq" %%I in (`tailscale ip -4 2^>nul`) do (
    set "TAILSCALE_IP=%%I"
    goto :ts_done
)
:ts_done

if not "!TAILSCALE_IP!"=="" (
    echo [OK] Your Tailscale IP: !TAILSCALE_IP!
    echo      Tell the coordinator your address: http://!TAILSCALE_IP!:8000
    echo.
    echo Running network connectivity check ^(tailscale netcheck^)...
    echo This refreshes Tailscale routing and helps avoid connection problems.
    tailscale netcheck
    if !errorlevel! neq 0 (
        echo [WARNING] Network check reported issues above.
        echo   If the coordinator cannot connect, check that Windows Firewall
        echo   allows the Tailscale app and that UDP port 41641 is not blocked.
    )
) else (
    echo [WARNING] Tailscale is not running or not connected.
    echo   Please open the Tailscale app, log in, and connect.
    echo   Download Tailscale: https://tailscale.com/download
)
echo.

:: ---- Find the data CSV ----------------------------------------------
:: Look in data\ first; fall back to the current directory.
set "CSV_DIR=."
set "CSV_COUNT=0"
if exist "data\" (
    for %%F in ("data\*.csv") do set /a CSV_COUNT+=1
    if !CSV_COUNT! gtr 0 set "CSV_DIR=data"
)
if !CSV_COUNT! equ 0 (
    for %%F in ("*.csv") do set /a CSV_COUNT+=1
)

if !CSV_COUNT! equ 0 (
    echo [ERROR] No .csv file found in data\ or the current folder.
    echo   Copy your registry data CSV into the data\ subfolder,
    echo   then double-click this file again.
    pause
    exit /b 1
)

set "CSV_NUM=0"
echo Available data files:
for %%F in ("!CSV_DIR!\*.csv") do (
    set /a CSV_NUM+=1
    echo   !CSV_NUM!^) %%F
    set "CSV_!CSV_NUM!=%%F"
)
echo.

if !CSV_COUNT! equ 1 (
    set "DATA_FILE=!CSV_1!"
    echo Auto-selected: !DATA_FILE!
) else (
    set /p "CHOICE=Enter the number of your data file: "
    if not defined CSV_!CHOICE! (
        echo [ERROR] Invalid selection.
        pause
        exit /b 1
    )
    set "DATA_FILE=!CSV_%CHOICE%!"
)
echo.

:: ---- Port -----------------------------------------------------------
set /p "PORT=Port to listen on (press Enter for default 8000): "
if "!PORT!"=="" set "PORT=8000"

:: ---- Security token -------------------------------------------------
echo.
echo Security token: a shared password the coordinator must also enter.
echo (Press Enter to run without a password.)
set /p "TOKEN=Security token [press Enter for none]: "
echo.

:: ---- Start the server -----------------------------------------------
echo ========================================
echo  Starting server...
echo  Data file : !DATA_FILE!
echo  Port      : !PORT!
if not "!TAILSCALE_IP!"=="" echo  Your URL  : http://!TAILSCALE_IP!:!PORT!
echo.
echo  IMPORTANT: Keep this window open the entire time the
echo  coordinator is running the analysis.
echo  To stop the server, close this window.
echo ========================================
echo.

set "FED_DATA_FILE=!DATA_FILE!"
set "FED_PORT=!PORT!"
if not "!TOKEN!"=="" set "FED_TOKEN=!TOKEN!"

!RSCRIPT! api_server.R

echo.
echo Server stopped.
pause
