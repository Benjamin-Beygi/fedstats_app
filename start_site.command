#!/bin/bash
# Federated Statistics — Site Server Launcher (macOS)
# Double-click this file in Finder to start the site server.

cd "$(dirname "$BASH_SOURCE")"

echo "========================================"
echo " Federated Statistics — Site Server"
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

  # Run netcheck: probes UDP/DERP connectivity and refreshes Tailscale's
  # routing table — this often pre-empts the connection issues that only
  # appear when the coordinator first tries to reach this site.
  echo ""
  echo "Running network connectivity check (tailscale netcheck)..."
  netcheck_out=$(tailscale netcheck 2>&1)
  echo "$netcheck_out"
  if echo "$netcheck_out" | grep -qi "UDP.*false\|no UDP"; then
    echo ""
    echo "  [WARNING] UDP traffic appears to be blocked (often a firewall or VPN)."
    echo "  Tailscale will fall back to relay servers (DERP) — connections will"
    echo "  still work but may be slower. To fix: allow UDP port 41641 outbound."
  fi
else
  echo "[WARNING] Tailscale is not running or not connected."
  echo "  Please open the Tailscale app, log in, and connect before the analysis."
  echo "  Download Tailscale: https://tailscale.com/download"
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
  echo "[ERROR] No .csv file found in this folder or in data/."
  echo "  Copy your registry data CSV into the data/ subfolder,"
  echo "  then double-click this file again."
  read -rp "Press Enter to exit..."
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
echo "Security token: a shared password that the coordinator must also enter."
echo "(Leave blank to run without a password — only do this on a trusted network.)"
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
echo "  IMPORTANT: Keep this window open the entire time the"
echo "  coordinator is running the analysis."
echo "  To stop the server, close this window or press Ctrl+C."
echo "========================================"
echo ""

if [ -n "$TOKEN" ]; then
  FED_DATA_FILE="$DATA_FILE" FED_PORT="$PORT" FED_TOKEN="$TOKEN" Rscript api_server.R
else
  FED_DATA_FILE="$DATA_FILE" FED_PORT="$PORT" Rscript api_server.R
fi

echo ""
read -rp "Server stopped. Press Enter to close this window..."
