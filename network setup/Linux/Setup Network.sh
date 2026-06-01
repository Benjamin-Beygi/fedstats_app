#!/bin/bash
# ============================================================
#   Federated Statistics — Network Setup  (Linux)
#   Run in Terminal:  bash "Setup Network.sh"
# ============================================================
#   Or make it executable first:
#     chmod +x "Setup Network.sh"
#   Then double-click or run:
#     ./"Setup Network.sh"
# ============================================================

echo ""
echo "╔══════════════════════════════════════════════╗"
echo "║     Federated Statistics — Network Setup     ║"
echo "╚══════════════════════════════════════════════╝"
echo ""

# ── Step 1 of 3: Check Tailscale ────────────────────────────
echo "[ Step 1 of 3 ]  Checking Tailscale..."

if ! command -v tailscale &>/dev/null; then
  echo ""
  echo "  ✗  Tailscale is not installed."
  echo ""
  echo "     1. Download and install Tailscale:"
  echo "        https://tailscale.com/download"
  echo ""
  echo "     Linux quick install:"
  echo "        curl -fsSL https://tailscale.com/install.sh | sh"
  echo ""
  echo "     2. Accept the invitation link your coordinator sent you."
  echo ""
  echo "     3. Run this script again."
  echo ""
  xdg-open "https://tailscale.com/download" 2>/dev/null &
  read -rp "  Press Enter to close..."
  exit 1
fi

echo "  ✓  Tailscale is installed."
echo ""

# ── Step 2 of 3: Network check ──────────────────────────────
echo "[ Step 2 of 3 ]  Checking network conditions..."
echo ""
tailscale netcheck 2>&1
echo ""
echo "  If you see errors above, take a screenshot and send it"
echo "  to your coordinator before continuing."
echo ""

# ── Step 3 of 3: Connect ─────────────────────────────────────
echo "[ Step 3 of 3 ]  Connecting..."
echo ""

TS_IP=$(tailscale ip -4 2>/dev/null)

if [ -n "$TS_IP" ]; then
  echo "  ✓  Already connected."
  echo ""
  echo "     Your Tailscale IP address:"
  echo "     $TS_IP"
  echo ""
  echo "     Share this address with your coordinator."
  echo ""
  echo "  ─────────────────────────────────────────────────────"
  echo "  If your coordinator cannot reach you, you may be"
  echo "  connected to a different Tailscale network."
  echo ""
  read -rp "  Switch to the coordinator's network now? (y/n): " SWITCH
  if [[ "$SWITCH" =~ ^[Yy]$ ]]; then
    echo ""
    sudo tailscale logout
    echo ""
    echo "  Connecting to coordinator's network..."
    echo "  A browser window will open. Log in with the account"
    echo "  your coordinator invited."
    echo ""
    sudo tailscale up
  fi
else
  echo "  Not connected. Starting login..."
  echo ""
  echo "  A browser window will open. Log in with the account"
  echo "  your coordinator invited."
  echo ""
  sudo tailscale up
fi

echo ""

# Final result
TS_IP=$(tailscale ip -4 2>/dev/null)
if [ -n "$TS_IP" ]; then
  echo "  ✓  Connected."
  echo ""
  echo "     Your Tailscale IP address:"
  echo "     $TS_IP"
  echo ""
  echo "     Send this address to your coordinator."
  echo "     They need it to include your site in the analysis."
else
  echo "  ✗  Could not get a Tailscale IP address."
  echo ""
  echo "     Possible reasons:"
  echo "     Your coordinator has not yet approved your device."
  echo "     Ask them to check the Tailscale admin console."
  echo "     Or login was cancelled — run this script again."
fi

echo ""
read -rp "  Press Enter to close..."
