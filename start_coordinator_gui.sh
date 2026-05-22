#!/bin/bash
# Federated Statistics — Coordinator GUI Launcher (Linux)
# Run:  bash start_coordinator_gui.sh

cd "$(dirname "$0")"

echo "========================================"
echo " Federated Statistics — Coordinator GUI"
echo "========================================"
echo ""

# ---- Check R --------------------------------------------------------
if ! command -v Rscript &>/dev/null; then
  echo "[ERROR] R is not installed."
  echo ""
  echo "Installing R on Ubuntu/Debian:"
  echo "  sudo apt update && sudo apt install -y r-base r-base-dev"
  echo ""
  read -rp "Would you like to install R now? (requires sudo) [y/N]: " INSTALL_R
  if [[ "$INSTALL_R" =~ ^[Yy]$ ]]; then
    sudo apt update && sudo apt install -y r-base r-base-dev
    if ! command -v Rscript &>/dev/null; then
      echo "[ERROR] Installation failed. Please install R manually and re-run."
      exit 1
    fi
  else
    echo "Please install R and re-run this script."
    exit 1
  fi
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

# ---- Check Tailscale ------------------------------------------------
echo "Checking Tailscale..."
TAILSCALE_IP=""
if command -v tailscale &>/dev/null; then
  TAILSCALE_IP=$(tailscale ip -4 2>/dev/null | head -1)
fi

if [ -n "$TAILSCALE_IP" ]; then
  echo "[OK] Your Tailscale IP: $TAILSCALE_IP"
else
  echo "[WARNING] Tailscale not connected — make sure all sites are reachable."
  echo "  Run:  sudo tailscale up"
fi
echo ""

# ---- Start the Shiny coordinator app --------------------------------
echo "========================================"
echo " Starting coordinator interface..."
echo ""
echo "  A browser window will open (or copy the URL printed below)."
echo "  Keep this terminal open while running the analysis."
echo "  Press Ctrl+C to stop."
echo "========================================"
echo ""

Rscript -e "shiny::runApp('coordinator_app.R', launch.browser = TRUE)"
