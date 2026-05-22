#!/bin/bash
# Federated Statistics — Coordinator GUI Launcher (macOS)
# Double-click this file in Finder to open the browser-based coordinator.

cd "$(dirname "$BASH_SOURCE")"

echo "========================================"
echo " Federated Statistics — Coordinator GUI"
echo "========================================"
echo ""

# ---- Check R --------------------------------------------------------
if ! command -v Rscript &>/dev/null; then
  echo "[ERROR] R is not installed (or Rscript is not on your PATH)."
  echo ""
  echo "  Option 1 — If you have Homebrew:  brew install r"
  echo "  Option 2 — Download the installer: https://cran.r-project.org/bin/macosx/"
  echo ""
  read -rp "Press Enter to open the download page in your browser..."
  open "https://cran.r-project.org/bin/macosx/"
  echo "After installing R, close this window and double-click this file again."
  read -rp "Press Enter to exit..."
  exit 1
fi

echo "[OK] $(Rscript --version 2>&1 | head -1)"
echo ""

# ---- Install required R packages ------------------------------------
echo "Checking required R packages (shiny, httr, jsonlite)..."
Rscript -e "
  pkgs <- c('shiny', 'httr', 'jsonlite')
  need <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(need) == 0) {
    cat('  All packages already installed.\n')
  } else {
    cat('  Installing:', paste(need, collapse = ', '), '\n')
    install.packages(need, repos = 'https://cloud.r-project.org')
    cat('  Done.\n')
  }
"
echo ""

# ---- Ensure Tailscale daemon is running and connected ---------------
echo "Checking Tailscale..."
if ! command -v tailscale &>/dev/null; then
  echo "[WARNING] Tailscale not installed — make sure all sites are reachable."
  echo "  Download Tailscale: https://tailscale.com/download"
else
  MY_IP=$(tailscale ip -4 2>/dev/null | head -1)

  if [ -z "$MY_IP" ]; then
    if ! tailscale status &>/dev/null 2>&1; then
      echo "  Tailscale daemon is not running."
      echo "  Starting it now — you may be prompted for your password."
      sudo tailscaled &>/dev/null &
      sleep 3
    fi

    MY_IP=$(tailscale ip -4 2>/dev/null | head -1)

    if [ -z "$MY_IP" ]; then
      echo "  Tailscale is running but not connected. Running 'tailscale up'..."
      sudo tailscale up
      MY_IP=$(tailscale ip -4 2>/dev/null | head -1)
    fi
  fi

  if [ -n "$MY_IP" ]; then
    echo "[OK] Your Tailscale IP: $MY_IP"
  else
    echo "[WARNING] Could not connect to Tailscale automatically."
    echo "  If site connections fail, run: sudo tailscaled"
    echo "  in a separate terminal, then re-run this script."
  fi
fi
echo ""

# ---- Start the Shiny coordinator app --------------------------------
echo "========================================"
echo " Starting coordinator interface..."
echo ""
echo "  A browser window will open automatically."
echo "  Keep this terminal window open while running the analysis."
echo "  Press Ctrl+C here (or close this window) to stop."
echo "========================================"
echo ""

Rscript -e "shiny::runApp('coordinator_app.R', launch.browser = TRUE)"

echo ""
read -rp "Server stopped. Press Enter to close this window..."
