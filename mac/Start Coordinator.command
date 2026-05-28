#!/bin/bash
# ============================================================
#   Federated Statistics — Coordinator  (macOS)
#   Double-click this file in Finder to open the interface.
# ============================================================
#
#   What this does:
#     • Opens the analysis coordinator in your web browser.
#     • You will enter the addresses of all participating sites,
#       then run the federated analysis from the browser window.
#     • Make sure every site has already started their server
#       before you click "Run Analysis".
#
#   If you see "permission denied" when double-clicking:
#     Open Terminal, paste this, and press Enter:
#       chmod +x ~/path/to/mac/Start\ Coordinator.command
# ============================================================

# Go to project root (parent of this mac/ folder)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR/.."

echo ""
echo "╔══════════════════════════════════════════════╗"
echo "║   Federated Statistics — Coordinator         ║"
echo "╚══════════════════════════════════════════════╝"
echo ""
echo "  Before continuing, make sure:"
echo "    • Tailscale is connected (check the menu bar icon)"
echo "    • All site operators have started their servers and"
echo "      sent you their addresses (http://100.x.x.x:8000)"
echo ""

# ── Step 1 of 3: Check R ────────────────────────────────────
echo "[ Step 1 of 3 ]  Checking R..."

if ! command -v Rscript &>/dev/null; then
  echo ""
  echo "  ✗  R is not installed (or not found on your PATH)."
  echo ""
  echo "     Install R in one of two ways:"
  echo "       Option 1 — Homebrew (if installed):  brew install r"
  echo "       Option 2 — Download the installer from CRAN:"
  echo "                  https://cran.r-project.org/bin/macosx/"
  echo ""
  read -rp "  Press Enter to open the download page in your browser..."
  open "https://cran.r-project.org/bin/macosx/"
  echo "  After installing R, close this window and double-click this file again."
  read -rp "  Press Enter to close..."
  exit 1
fi

echo "  ✓  $(Rscript --version 2>&1 | head -1)"
echo ""

# ── Step 2 of 3: R packages ─────────────────────────────────
echo "[ Step 2 of 3 ]  Checking required R packages (shiny, httr, jsonlite)..."

Rscript -e "
  pkgs <- c('shiny', 'httr', 'jsonlite')
  need <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(need) == 0) {
    cat('  ✓  All packages already installed.\n')
  } else {
    cat('  Installing:', paste(need, collapse = ', '), '— this may take a minute...\n')
    install.packages(need, repos = 'https://cloud.r-project.org', quiet = TRUE)
    cat('  ✓  Done.\n')
  }
"
echo ""

# ── Step 3 of 3: Tailscale ──────────────────────────────────
echo "[ Step 3 of 3 ]  Checking Tailscale..."

MY_IP=$(tailscale ip -4 2>/dev/null | head -1)

if [ -n "$MY_IP" ]; then
  echo "  ✓  Tailscale connected.  Your IP: $MY_IP"
else
  # Try to start the daemon if it's not running
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
    echo "     Open the Tailscale app and connect manually, then"
    echo "     close this window and double-click this file again."
    echo ""
    read -rp "  Press Enter to continue anyway..."
  fi
fi
echo ""

# ── Launch ──────────────────────────────────────────────────
echo "══════════════════════════════════════════════════════"
echo "  Starting coordinator interface..."
echo ""
echo "  Your browser will open automatically in a few seconds."
echo "  If it doesn't open, look for a URL in the output below"
echo "  (starts with  http://127.0.0.1:...)  and paste it into"
echo "  your browser manually."
echo ""
echo "  Keep this window open while running the analysis."
echo "  To stop: close this window or press Ctrl+C."
echo "══════════════════════════════════════════════════════"
echo ""

Rscript -e "shiny::runApp('coordinator_app.R', launch.browser = TRUE)"

echo ""
echo "  Coordinator stopped."
read -rp "  Press Enter to close this window..."
