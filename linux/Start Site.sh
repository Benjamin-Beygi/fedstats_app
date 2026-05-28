#!/bin/bash
# ============================================================
#   Federated Statistics — Site Server  (Linux)
#   Run:  bash "linux/Start Site.sh"
#   Or make it executable and double-click in your file manager.
# ============================================================
#
#   What this does:
#     • Starts a secure local server that shares aggregate
#       statistics from your registry data with the coordinator.
#     • No individual patient records ever leave this computer —
#       only counts, means, and model summaries are transmitted.
#     • The server runs only while this terminal is open.
# ============================================================

# Go to project root (parent of this linux/ folder)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR/.."

echo ""
echo "┌──────────────────────────────────────────────┐"
echo "│     Federated Statistics — Site Server       │"
echo "└──────────────────────────────────────────────┘"
echo ""
echo "  No individual patient records will leave this computer."
echo "  Keep this terminal open for the duration of the analysis."
echo ""

# ── Step 1 of 4: Check R ────────────────────────────────────
echo "[ Step 1 of 4 ]  Checking R..."

if ! command -v Rscript &>/dev/null; then
  echo ""
  echo "  ✗  R is not installed."
  echo ""
  echo "     Install R on Ubuntu/Debian:"
  echo "       sudo apt update && sudo apt install -y r-base r-base-dev"
  echo ""
  read -rp "  Would you like to install R now? (requires sudo) [y/N]: " INSTALL_R
  if [[ "$INSTALL_R" =~ ^[Yy]$ ]]; then
    sudo apt update && sudo apt install -y r-base r-base-dev
    if ! command -v Rscript &>/dev/null; then
      echo "  ✗  Installation failed. Please install R manually and re-run."
      exit 1
    fi
  else
    echo "  Please install R and re-run this script."
    exit 1
  fi
fi

echo "  ✓  $(Rscript --version 2>&1 | head -1)"
echo ""

# ── System libraries (Linux only) ───────────────────────────
echo "  Checking system libraries needed by R packages..."
MISSING_LIBS=()
for lib in libcurl4-openssl-dev libssl-dev libxml2-dev; do
  dpkg -l "$lib" &>/dev/null 2>&1 || MISSING_LIBS+=("$lib")
done

if [ ${#MISSING_LIBS[@]} -gt 0 ]; then
  echo "  Some libraries are missing: ${MISSING_LIBS[*]}"
  read -rp "  Install them now? (requires sudo) [y/N]: " INSTALL_LIBS
  if [[ "$INSTALL_LIBS" =~ ^[Yy]$ ]]; then
    sudo apt update && sudo apt install -y "${MISSING_LIBS[@]}"
  else
    echo "  Skipping — R package installation may fail without these."
  fi
else
  echo "  ✓  System libraries OK."
fi
echo ""

# ── Step 2 of 4: R packages ─────────────────────────────────
echo "[ Step 2 of 4 ]  Checking required R packages (plumber, jsonlite)..."

Rscript -e "
  pkgs <- c('plumber', 'jsonlite')
  need <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(need) == 0) {
    cat('  \U2713  All packages already installed.\n')
  } else {
    cat('  Installing:', paste(need, collapse = ', '), '-- this may take a minute...\n')
    install.packages(need, repos = 'https://cloud.r-project.org', quiet = TRUE)
    cat('  \U2713  Done.\n')
  }
"
echo ""

# ── Step 3 of 4: Tailscale ──────────────────────────────────
echo "[ Step 3 of 4 ]  Checking Tailscale (secure network connection)..."

TAILSCALE_IP=""
if command -v tailscale &>/dev/null; then
  TAILSCALE_IP=$(tailscale ip -4 2>/dev/null | head -1)
fi

if [ -n "$TAILSCALE_IP" ]; then
  echo "  ✓  Tailscale connected.  Your IP: $TAILSCALE_IP"
  echo "     The coordinator will reach you at:  http://${TAILSCALE_IP}:8000"

  echo ""
  echo "  Running a quick network check..."
  netcheck_out=$(tailscale netcheck 2>&1)
  if echo "$netcheck_out" | grep -qi "UDP.*false\|no UDP"; then
    echo "  ⚠  UDP traffic appears blocked — Tailscale will use relay servers."
    echo "     Connections will still work but may be slightly slower."
    echo "     Allow UDP port 41641 outbound to fix."
  else
    echo "  ✓  Network looks good."
  fi
else
  echo ""
  echo "  ⚠  Tailscale is not running or not connected."
  echo "     Run:  sudo tailscale up"
  echo "     Or download Tailscale:  https://tailscale.com/download/linux"
  echo ""
  read -rp "  Press Enter to continue anyway (not recommended)..."
fi
echo ""

# ── Step 4 of 4: Configure and start ────────────────────────
echo "[ Step 4 of 4 ]  Configure your server"
echo ""

# Find the data CSV
CSV_FILES=()
if [ -d "data" ]; then
  for f in data/*.csv; do [ -f "$f" ] && CSV_FILES+=("$f"); done
fi
if [ ${#CSV_FILES[@]} -eq 0 ]; then
  for f in *.csv; do [ -f "$f" ] && CSV_FILES+=("$f"); done
fi

if [ ${#CSV_FILES[@]} -eq 0 ]; then
  echo "  ✗  No data file (.csv) found in the data/ folder."
  echo ""
  echo "     Copy your registry data file into the data/ subfolder"
  echo "     and re-run this script."
  exit 1
elif [ ${#CSV_FILES[@]} -eq 1 ]; then
  DATA_FILE="${CSV_FILES[0]}"
  echo "  Data file:  $DATA_FILE  (only one found — selected automatically)"
else
  echo "  Several data files were found. Which one should this server use?"
  echo ""
  for i in "${!CSV_FILES[@]}"; do
    echo "    $((i+1)))  ${CSV_FILES[$i]}"
  done
  echo ""
  while true; do
    read -rp "  Enter number [1–${#CSV_FILES[@]}]: " CHOICE
    if [[ "$CHOICE" =~ ^[0-9]+$ ]] && [ "$CHOICE" -ge 1 ] && [ "$CHOICE" -le "${#CSV_FILES[@]}" ]; then
      DATA_FILE="${CSV_FILES[$((CHOICE-1))]}"
      break
    fi
    echo "  Please enter a number between 1 and ${#CSV_FILES[@]}."
  done
fi
echo ""

read -rp "  Port number (press Enter for default 8000): " PORT
PORT="${PORT:-8000}"

echo ""
echo "  Security token — a shared password between you and the coordinator."
echo "  Both sides must use the same token. Leave blank if not needed."
read -rp "  Security token [Enter for none]: " TOKEN
echo ""

# ── Launch ──────────────────────────────────────────────────
echo "══════════════════════════════════════════════════════"
echo "  Server starting..."
echo ""
echo "  Data file :  $DATA_FILE"
echo "  Port      :  $PORT"
if [ -n "$TAILSCALE_IP" ]; then
echo ""
echo "  ★  Tell the coordinator your address:"
echo "     http://${TAILSCALE_IP}:${PORT}"
fi
echo ""
echo "  Keep this terminal open while the analysis runs."
echo "  To stop the server:  press Ctrl+C"
echo "══════════════════════════════════════════════════════"
echo ""

if [ -n "$TOKEN" ]; then
  FED_DATA_FILE="$DATA_FILE" FED_PORT="$PORT" FED_TOKEN="$TOKEN" Rscript api_server.R
else
  FED_DATA_FILE="$DATA_FILE" FED_PORT="$PORT" Rscript api_server.R
fi

echo ""
echo "  Server stopped."
