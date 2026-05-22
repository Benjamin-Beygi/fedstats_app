@echo off
setlocal enabledelayedexpansion
title Federated Statistics - Coordinator

cd /d "%~dp0"

echo ========================================
echo  Federated Statistics - Coordinator
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
echo Checking required R packages (httr, jsonlite, readxl)...
!RSCRIPT! -e "pkgs<-c('httr','jsonlite','readxl'); need<-pkgs[!pkgs %%in%% rownames(installed.packages())]; if(length(need)==0) cat('  All packages already installed.\n') else { cat('  Installing:',paste(need,collapse=', '),'\n'); install.packages(need,repos='https://cloud.r-project.org'); cat('  Done.\n') }"
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

:: ---- Collect site URLs ----------------------------------------------
echo You need the Tailscale URLs of ALL participating sites.
echo Each site operator should have started their server already.
echo.
echo Format: http://100.x.x.x:8000
echo Multiple sites - separate with commas, no spaces:
echo   Example: http://100.74.226.3:8000,http://100.86.16.91:8000
echo.
set /p "SITE_URLS=Site URLs: "

if "!SITE_URLS!"=="" (
    echo [ERROR] No site URLs entered.
    pause
    exit /b 1
)

:: ---- Security token -------------------------------------------------
echo.
echo Security token: must match the token entered on each site server.
echo (Press Enter if the sites were started without a token.)
set /p "TOKEN=Security token [press Enter for none]: "
echo.

:: ---- Run the analysis -----------------------------------------------
echo ========================================
echo  Running federated analysis...
echo  Sites: !SITE_URLS!
echo ========================================
echo.
echo The analysis will first validate all sites, then run the statistics.
echo This may take a minute...
echo.

set "FED_MODE=remote"
set "FED_SITE_URLS=!SITE_URLS!"
if not "!TOKEN!"=="" set "FED_API_TOKEN=!TOKEN!"

!RSCRIPT! run.R

echo.
pause
