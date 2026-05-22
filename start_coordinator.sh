#!/bin/bash
# Federated Statistics — Coordinator Launcher (Linux)
# Run:  bash start_coordinator.sh

cd "$(dirname "$0")"

echo "========================================"
echo " Federated Statistics — Coordinator"
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
echo "Checking required R packages (httr, jsonlite, readxl)..."
Rscript -e "
  pkgs <- c('httr', 'jsonlite', 'readxl')
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

# ---- Collect site URLs ----------------------------------------------
echo "You need the Tailscale URLs of ALL participating sites."
echo "Each site operator should have started their server already."
echo ""
echo "Format: http://100.x.x.x:8000"
echo "Multiple sites — separate with commas, no spaces:"
echo "  Example: http://100.74.226.3:8000,http://100.86.16.91:8000"
echo ""
read -rp "Site URLs: " SITE_URLS

if [ -z "$SITE_URLS" ]; then
  echo "[ERROR] No site URLs entered."
  exit 1
fi

# ---- Security token -------------------------------------------------
echo ""
echo "Security token: must match the token entered on each site server."
echo "(Leave blank if sites were started without a token.)"
read -rp "Security token [press Enter for none]: " TOKEN
echo ""

# ---- Run the analysis -----------------------------------------------
echo "========================================"
echo " Running federated analysis..."
echo "  Sites: $SITE_URLS"
echo "========================================"
echo ""
echo "The analysis will first validate all sites, then run the statistics."
echo ""

if [ -n "$TOKEN" ]; then
  FED_MODE=remote FED_SITE_URLS="$SITE_URLS" FED_API_TOKEN="$TOKEN" Rscript run.R
else
  FED_MODE=remote FED_SITE_URLS="$SITE_URLS" Rscript run.R
fi
