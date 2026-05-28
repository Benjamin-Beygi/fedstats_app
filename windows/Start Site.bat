@echo off
setlocal enabledelayedexpansion
title Federated Statistics — Site Server

:: Go to project root (parent of this windows\ folder)
cd /d "%~dp0.."

echo.
echo  +--------------------------------------------+
echo  ^|   Federated Statistics — Site Server       ^|
echo  +--------------------------------------------+
echo.
echo   No individual patient records will leave this computer.
echo   Keep this window open for the duration of the analysis.
echo.

:: ── Step 1 of 4: Find R ─────────────────────────────────────
echo [ Step 1 of 4 ]  Checking R...

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
        goto :r_found
    )
)
for /d %%D in ("C:\Program Files (x86)\R\R-*") do (
    if exist "%%D\bin\Rscript.exe" (
        set "RSCRIPT=%%D\bin\Rscript.exe"
        set "PATH=%%D\bin;!PATH!"
        goto :r_found
    )
)

echo.
echo   X  R is not installed on this computer.
echo.
echo      Please download and install R from:
echo        https://cran.r-project.org/bin/windows/base/
echo.
echo      After installing R, close this window and
echo      double-click this file again.
echo.
start https://cran.r-project.org/bin/windows/base/
pause
exit /b 1

:r_found
for /f "tokens=* usebackq" %%V in (`!RSCRIPT! --version 2^>^&1`) do (
    echo   OK  %%V & goto :r_ver_done
)
:r_ver_done
echo.

:: ── Step 2 of 4: R packages ──────────────────────────────────
echo [ Step 2 of 4 ]  Checking required R packages (plumber, jsonlite)...
!RSCRIPT! -e "pkgs<-c('plumber','jsonlite'); need<-pkgs[!pkgs %%in%% rownames(installed.packages())]; if(length(need)==0) cat('  OK  All packages already installed.\n') else { cat('  Installing:',paste(need,collapse=', '),'...\n'); install.packages(need,repos='https://cloud.r-project.org',quiet=TRUE); cat('  OK  Done.\n') }"
if !errorlevel! neq 0 (
    echo.
    echo   X  Package installation failed.
    echo      Check your internet connection and try again.
    pause & exit /b 1
)
echo.

:: ── Step 3 of 4: Tailscale ───────────────────────────────────
echo [ Step 3 of 4 ]  Checking Tailscale (secure network connection)...
set "TAILSCALE_IP="
for /f "tokens=* usebackq" %%I in (`tailscale ip -4 2^>nul`) do (
    set "TAILSCALE_IP=%%I" & goto :ts_done
)
:ts_done

if not "!TAILSCALE_IP!"=="" (
    echo   OK  Tailscale connected.  Your IP: !TAILSCALE_IP!
    echo       The coordinator will reach you at:  http://!TAILSCALE_IP!:8000
    echo.
    echo   Running a quick network check...
    tailscale netcheck >nul 2>&1
    if !errorlevel! equ 0 (
        echo   OK  Network looks good.
    ) else (
        echo   ^!  Network check reported issues.
        echo       If the coordinator cannot connect, check Windows Firewall
        echo       settings for the Tailscale app.
    )
) else (
    echo.
    echo   ^!  Tailscale is not running or not connected.
    echo       Please open the Tailscale app, log in, and connect —
    echo       then close this window and double-click this file again.
    echo       Download Tailscale:  https://tailscale.com/download
    echo.
    set /p "CONT=   Press Enter to continue anyway (not recommended)..."
)
echo.

:: ── Step 4 of 4: Configure and start ─────────────────────────
echo [ Step 4 of 4 ]  Configure your server
echo.

:: Find the data CSV
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
    echo   X  No data file (.csv) found in the data\ folder.
    echo.
    echo      Copy your registry data file (e.g. my_registry.csv)
    echo      into the data\ subfolder, then double-click this file again.
    echo.
    pause & exit /b 1
)

set "CSV_NUM=0"
if !CSV_COUNT! equ 1 (
    for %%F in ("!CSV_DIR!\*.csv") do (
        set "DATA_FILE=%%F"
        echo   Data file:  %%F  ^(only one found — selected automatically^)
    )
) else (
    echo   Several data files were found. Which one should this server use?
    echo.
    for %%F in ("!CSV_DIR!\*.csv") do (
        set /a CSV_NUM+=1
        echo     !CSV_NUM!^)  %%F
        set "CSV_!CSV_NUM!=%%F"
    )
    echo.
    set /p "CHOICE=  Enter number [1-!CSV_COUNT!]: "
    if not defined CSV_!CHOICE! (
        echo   X  Invalid selection.
        pause & exit /b 1
    )
    call set "DATA_FILE=%%CSV_!CHOICE!%%"
)
echo.

set /p "PORT=  Port number (press Enter for default 8000): "
if "!PORT!"=="" set "PORT=8000"

echo.
echo   Security token — a shared password between you and the coordinator.
echo   Both sides must use the same token. Leave blank if not needed.
set /p "TOKEN=  Security token [Enter for none]: "
echo.

:: ── Launch ───────────────────────────────────────────────────
echo  +----------------------------------------------------+
echo  ^|  Server starting...                               ^|
echo  ^|                                                   ^|
echo  ^|  Data file :  !DATA_FILE!
echo  ^|  Port      :  !PORT!
if not "!TAILSCALE_IP!"=="" (
echo  ^|                                                   ^|
echo  ^|  ** Tell the coordinator your address: **         ^|
echo  ^|     http://!TAILSCALE_IP!:!PORT!
)
echo  ^|                                                   ^|
echo  ^|  Keep this window open during the analysis.       ^|
echo  ^|  Close this window to stop the server.            ^|
echo  +----------------------------------------------------+
echo.

set "FED_DATA_FILE=!DATA_FILE!"
set "FED_PORT=!PORT!"
if not "!TOKEN!"=="" set "FED_TOKEN=!TOKEN!"

!RSCRIPT! api_server.R

echo.
echo   Server stopped.
pause
