@echo off
setlocal enabledelayedexpansion
title Federated Statistics — Network Setup

echo.
echo  +--------------------------------------------+
echo  ^|   Federated Statistics — Network Setup     ^|
echo  +--------------------------------------------+
echo.

:: ── Step 1 of 3: Find Tailscale ──────────────────────────────
echo [ Step 1 of 3 ]  Checking Tailscale...

set "TAILSCALE="
where tailscale >nul 2>&1
if !errorlevel! equ 0 (
    set "TAILSCALE=tailscale"
    goto :ts_found
)
if exist "C:\Program Files\Tailscale\tailscale.exe" (
    set "TAILSCALE=C:\Program Files\Tailscale\tailscale.exe"
    goto :ts_found
)

echo.
echo   X  Tailscale is not installed.
echo.
echo      1. Download and install Tailscale:
echo         https://tailscale.com/download
echo.
echo      2. Accept the invitation link your coordinator sent you.
echo.
echo      3. Run this script again.
echo.
start https://tailscale.com/download
pause
exit /b 1

:ts_found
echo   OK  Tailscale is installed.
echo.

:: ── Step 2 of 3: Network check ───────────────────────────────
echo [ Step 2 of 3 ]  Checking network conditions...
echo.
!TAILSCALE! netcheck
echo.
echo   If you see errors above, take a screenshot and send it
echo   to your coordinator before continuing.
echo.

:: ── Step 3 of 3: Connect ─────────────────────────────────────
echo [ Step 3 of 3 ]  Connecting...
echo.

set "TS_IP="
for /f "tokens=* usebackq" %%I in (`!TAILSCALE! ip -4 2^>nul`) do (
    set "TS_IP=%%I" & goto :ip_check_done
)
:ip_check_done

if not "!TS_IP!"=="" (
    echo   OK  Already connected.
    echo.
    echo       Your Tailscale IP address:
    echo       !TS_IP!
    echo.
    echo       Share this address with your coordinator.
    echo.
    echo   --------------------------------------------------
    echo   If your coordinator cannot reach you, you may be
    echo   connected to a different Tailscale network.
    echo.
    set /p "SWITCH=  Switch to the coordinator's network now? (y/n): "
    if /i "!SWITCH!"=="y" (
        echo.
        !TAILSCALE! logout
        echo.
        echo   Connecting to coordinator's network...
        echo   A browser window will open. Log in with the account
        echo   your coordinator invited.
        echo.
        !TAILSCALE! up
    )
) else (
    echo   Not connected. Starting login...
    echo.
    echo   A browser window will open. Log in with the account
    echo   your coordinator invited.
    echo.
    !TAILSCALE! up
)

echo.

:: Final result
set "TS_IP="
for /f "tokens=* usebackq" %%I in (`!TAILSCALE! ip -4 2^>nul`) do (
    set "TS_IP=%%I" & goto :final_done
)
:final_done

if not "!TS_IP!"=="" (
    echo   OK  Connected.
    echo.
    echo       Your Tailscale IP address:
    echo       !TS_IP!
    echo.
    echo       Send this address to your coordinator.
    echo       They need it to include your site in the analysis.
) else (
    echo   X  Could not get a Tailscale IP address.
    echo.
    echo      Possible reasons:
    echo      Your coordinator has not yet approved your device.
    echo      Ask them to check the Tailscale admin console.
    echo      Or login was cancelled -- run this script again.
)

echo.
pause
