#!/bin/bash
# Federated Statistics — Coordinator Launcher (macOS)
# Double-click this file in Finder to run the federated analysis.

cd "$(dirname "$BASH_SOURCE")"

echo "========================================"
echo " Federated Statistics — Coordinator"
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

# ---- Ensure Tailscale daemon is running and connected ---------------
echo "Checking Tailscale..."
if ! command -v tailscale &>/dev/null; then
  echo "[WARNING] Tailscale not installed — make sure all sites are reachable."
  echo "  Download Tailscale: https://tailscale.com/download"
else
  # Fast path: already connected
  MY_IP=$(tailscale ip -4 2>/dev/null | head -1)

  if [ -z "$MY_IP" ]; then
    # `tailscale status` exits non-zero when the daemon socket is missing
    # (daemon not started at all). It exits 0 even when logged out.
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
    echo "  in a separate terminal window, then re-run this script."
  fi
fi
echo ""

# ---- Collect site URLs ----------------------------------------------
echo "You need the Tailscale URLs of ALL participating sites."
echo "Each site operator should have started their server and told you their URL."
echo ""
echo "Format:  http://100.x.x.x:8000"
echo "Multiple sites — separate with commas, no spaces:"
echo "  Example: http://100.74.226.3:8000,http://100.86.16.91:8000"
echo ""
read -rp "Site URLs: " SITE_URLS

if [ -z "$SITE_URLS" ]; then
  echo "[ERROR] No site URLs entered. Please re-run and enter at least one URL."
  read -rp "Press Enter to exit..."
  exit 1
fi

# ---- Security token -------------------------------------------------
echo ""
echo "Security token: must match the token the sites were started with."
echo "(Leave blank if the sites were started without a token.)"
read -rp "Security token [press Enter for none]: " TOKEN
echo ""

# ---- Run the analysis -----------------------------------------------
echo "========================================"
echo " Running federated analysis..."
echo "  Sites: $SITE_URLS"
echo "========================================"
echo ""
echo "The analysis will first validate all sites, then run the statistics."
echo "This may take a minute..."
echo ""

if [ -n "$TOKEN" ]; then
  FED_MODE=remote FED_SITE_URLS="$SITE_URLS" FED_API_TOKEN="$TOKEN" Rscript run.R
else
  FED_MODE=remote FED_SITE_URLS="$SITE_URLS" Rscript run.R
fi

echo ""
read -rp "Analysis complete. Press Enter to close this window..."
