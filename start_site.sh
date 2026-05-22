#!/bin/bash
# Federated Statistics — Site Server Launcher (Linux)
# Run:  bash start_site.sh

cd "$(dirname "$0")"

echo "========================================"
echo " Federated Statistics — Site Server"
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

# ---- Install system libraries (needed by R packages on Linux) -------
echo "Checking system libraries for R packages..."
MISSING_LIBS=()
for lib in libcurl4-openssl-dev libssl-dev libxml2-dev; do
  if ! dpkg -l "$lib" &>/dev/null 2>&1; then
    MISSING_LIBS+=("$lib")
  fi
done

if [ ${#MISSING_LIBS[@]} -gt 0 ]; then
  echo "  Some system libraries are missing: ${MISSING_LIBS[*]}"
  read -rp "  Install them now? (requires sudo) [y/N]: " INSTALL_LIBS
  if [[ "$INSTALL_LIBS" =~ ^[Yy]$ ]]; then
    sudo apt update && sudo apt install -y "${MISSING_LIBS[@]}"
  else
    echo "  Skipping — R package installation may fail without these."
  fi
else
  echo "  System libraries OK."
fi
echo ""

# ---- Install required R packages ------------------------------------
echo "Checking required R packages (plumber, jsonlite)..."
Rscript -e "
  pkgs <- c('plumber', 'jsonlite')
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
  echo "     Tell the coordinator your address: http://${TAILSCALE_IP}:8000"

  echo ""
  echo "Running network connectivity check (tailscale netcheck)..."
  netcheck_out=$(tailscale netcheck 2>&1)
  echo "$netcheck_out"
  if echo "$netcheck_out" | grep -qi "UDP.*false\|no UDP"; then
    echo ""
    echo "  [WARNING] UDP traffic appears to be blocked."
    echo "  Tailscale will use relay servers (DERP) instead — connections will"
    echo "  work but may be slower. Allow UDP port 41641 outbound to fix."
  fi
else
  echo "[WARNING] Tailscale is not running or not connected."
  echo "  Run:  sudo tailscale up"
  echo "  Or download Tailscale: https://tailscale.com/download/linux"
fi
echo ""

# ---- Find the data CSV ----------------------------------------------
# Look in data/ first; fall back to the current directory.
CSV_FILES=()
if [ -d "data" ]; then
  for f in data/*.csv; do [ -f "$f" ] && CSV_FILES+=("$f"); done
fi
if [ ${#CSV_FILES[@]} -eq 0 ]; then
  for f in *.csv; do [ -f "$f" ] && CSV_FILES+=("$f"); done
fi

if [ ${#CSV_FILES[@]} -eq 0 ]; then
  echo "[ERROR] No .csv file found in data/ or the current folder."
  echo "  Copy your registry data CSV into the data/ subfolder and re-run."
  exit 1
fi

if [ ${#CSV_FILES[@]} -eq 1 ]; then
  DATA_FILE="${CSV_FILES[0]}"
  echo "Data file: $DATA_FILE  (auto-selected — only one CSV found)"
else
  echo "Multiple CSV files found. Which one is your registry data?"
  for i in "${!CSV_FILES[@]}"; do
    echo "  $((i+1))) ${CSV_FILES[$i]}"
  done
  echo ""
  while true; do
    read -rp "Enter the number of your data file [1-${#CSV_FILES[@]}]: " CHOICE
    if [[ "$CHOICE" =~ ^[0-9]+$ ]] && [ "$CHOICE" -ge 1 ] && [ "$CHOICE" -le "${#CSV_FILES[@]}" ]; then
      DATA_FILE="${CSV_FILES[$((CHOICE-1))]}"
      break
    fi
    echo "  Invalid choice, please try again."
  done
fi
echo ""

# ---- Port -----------------------------------------------------------
read -rp "Port to listen on (press Enter for default 8000): " PORT
PORT="${PORT:-8000}"

# ---- Security token -------------------------------------------------
echo ""
echo "Security token: a shared password the coordinator must also enter."
echo "(Leave blank to run without a password.)"
read -rp "Security token [press Enter for none]: " TOKEN
echo ""

# ---- Start the server -----------------------------------------------
echo "========================================"
echo " Starting server..."
echo "  Data file : $DATA_FILE"
echo "  Port      : $PORT"
if [ -n "$TAILSCALE_IP" ]; then
echo "  Your URL  : http://${TAILSCALE_IP}:${PORT}"
fi
echo ""
echo "  Keep this terminal open while the coordinator is running."
echo "  Press Ctrl+C to stop the server."
echo "========================================"
echo ""

if [ -n "$TOKEN" ]; then
  FED_DATA_FILE="$DATA_FILE" FED_PORT="$PORT" FED_TOKEN="$TOKEN" Rscript api_server.R
else
  FED_DATA_FILE="$DATA_FILE" FED_PORT="$PORT" Rscript api_server.R
fi
