#!/bin/bash
# ============================================================
#   Federated Statistics — Coordinator  (Linux)
#   Run:  bash "linux/Start Coordinator.sh"
#   Or make it executable and double-click in your file manager.
# ============================================================
#
#   What this does:
#     • Opens the analysis coordinator in your web browser.
#     • You will enter the addresses of all participating sites,
#       then run the federated analysis from the browser window.
#     • Make sure every site has already started their server
#       before you click "Run Analysis".
# ============================================================

# Go to project root (parent of this linux/ folder)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR/.."

echo ""
echo "┌──────────────────────────────────────────────┐"
echo "│   Federated Statistics — Coordinator         │"
echo "└──────────────────────────────────────────────┘"
echo ""
echo "  Before continuing, make sure:"
echo "    • Tailscale is connected  (run: tailscale status)"
echo "    • All site operators have started their servers and"
echo "      sent you their addresses (http://100.x.x.x:8000)"
echo ""

# ── Step 1 of 3: Check R ────────────────────────────────────
echo "[ Step 1 of 3 ]  Checking R..."

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

# ── Step 2 of 3: R packages ─────────────────────────────────
echo "[ Step 2 of 3 ]  Checking required R packages (shiny, httr, jsonlite)..."

Rscript -e "
  pkgs <- c('shiny', 'httr', 'jsonlite')
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

# ── Step 3 of 3: Tailscale ──────────────────────────────────
echo "[ Step 3 of 3 ]  Checking Tailscale..."

MY_IP=$(tailscale ip -4 2>/dev/null | head -1)

if [ -n "$MY_IP" ]; then
  echo "  ✓  Tailscale connected.  Your IP: $MY_IP"
else
  if ! tailscale status &>/dev/null 2>&1; then
    echo "  Tailscale daemon is not running — attempting to start it..."
    sudo tailscaled &>/dev/null &
    sleep 3
  fi
  MY_IP=$(tailscale ip -4 2>/dev/null | head -1)
  if [ -z "$MY_IP" ]; then
    echo "  Tailscale is not connected.  Running 'tailscale up'..."
    sudo tailscale up
    MY_IP=$(tailscale ip -4 2>/dev/null | head -1)
  fi
  if [ -n "$MY_IP" ]; then
    echo "  ✓  Tailscale connected.  Your IP: $MY_IP"
  else
    echo ""
    echo "  ⚠  Could not connect to Tailscale automatically."
    echo "     Run:  sudo tailscale up"
    echo "     Then close this terminal and re-run this script."
    echo ""
    read -rp "  Press Enter to continue anyway..."
  fi
fi
echo ""

# ── Launch ──────────────────────────────────────────────────
echo "══════════════════════════════════════════════════════"
echo "  Starting coordinator interface..."
echo ""
echo "  A browser window will open automatically."
echo "  If it doesn't, look for a URL in the output below"
echo "  (starts with  http://127.0.0.1:...)  and paste it"
echo "  into your browser manually."
echo ""
echo "  Keep this terminal open while running the analysis."
echo "  To stop:  press Ctrl+C"
echo "══════════════════════════════════════════════════════"
echo ""

Rscript -e "shiny::runApp('coordinator_app.R', launch.browser = TRUE)"

echo ""
echo "  Coordinator stopped."
