#!/bin/bash
# Renders the PNGs of the dampack logo from the SVGs written by logo/make_hex.R.
# Run from the package root after `Rscript logo/make_hex.R`:  bash logo/render.sh
#
# Uses headless Google Chrome, because it loads the JetBrains Mono wordmark font from Google
# Fonts. Set CHROME to the Chrome or Chromium binary if it is not at the macOS default.
set -euo pipefail
CHROME="${CHROME:-/Applications/Google Chrome.app/Contents/MacOS/Google Chrome}"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
mkdir -p man/figures pkgdown/favicon

# render <svg> <width> <height> <png> <page background>
render() {
  python3 -I - "$1" "$2" "$3" "$5" "$TMP/page.html" <<'EOF'
import sys
svg, w, h, bg, out = sys.argv[1:6]
s = open(svg).read().replace('width="173.2" height="200"',
                             f'width="{w}" height="{h}" preserveAspectRatio="xMidYMid meet"')
open(out, "w").write("<!doctype html><html><head><style>html,body{margin:0;padding:0;background:"
                     + bg + ";} svg{display:block}</style></head><body>" + s + "</body></html>")
EOF
  "$CHROME" --headless=new --disable-gpu --hide-scrollbars --force-device-scale-factor=1 \
    --default-background-color=00000000 --virtual-time-budget=8000 \
    --window-size="$2,$3" --screenshot="$4" "file://$TMP/page.html" > /dev/null 2>&1
}

# the sticker: README, pkgdown home page, and print (600 dpi at 2 in tall)
render logo/dampack-hex.svg 240 278 man/figures/logo.png transparent
render logo/dampack-hex.svg 1039 1200 logo/dampack-hex-600dpi.png transparent

# favicons for the pkgdown site, from the icon (no text, so no font needed)
cp logo/dampack-hex-icon.svg pkgdown/favicon/favicon.svg
render logo/dampack-hex-icon.svg 96 96 pkgdown/favicon/favicon-96x96.png transparent
render logo/dampack-hex-icon.svg 192 192 pkgdown/favicon/web-app-manifest-192x192.png transparent
render logo/dampack-hex-icon.svg 512 512 pkgdown/favicon/web-app-manifest-512x512.png transparent
render logo/dampack-hex-icon.svg 180 180 pkgdown/favicon/apple-touch-icon.png "#ffffff"
for s in 16 32 48; do render logo/dampack-hex-icon.svg $s $s "$TMP/icon-$s.png" transparent; done

# favicon.ico holding the 16, 32 and 48 px PNGs
python3 -I - "$TMP" pkgdown/favicon/favicon.ico <<'EOF'
import struct, sys
tmp, out = sys.argv[1], sys.argv[2]
v_png = [open(f"{tmp}/icon-{s}.png", "rb").read() for s in (16, 32, 48)]
header = struct.pack("<HHH", 0, 1, len(v_png))
offset = 6 + 16 * len(v_png)
entries, data = b"", b""
for s, png in zip((16, 32, 48), v_png):
    entries += struct.pack("<BBBBHHII", s, s, 0, 0, 1, 32, len(png), offset + len(data))
    data += png
open(out, "wb").write(header + entries + data)
EOF

cat > pkgdown/favicon/site.webmanifest <<'EOF'
{
  "name": "dampack",
  "short_name": "dampack",
  "icons": [
    {"src": "/web-app-manifest-192x192.png", "sizes": "192x192", "type": "image/png", "purpose": "maskable"},
    {"src": "/web-app-manifest-512x512.png", "sizes": "512x512", "type": "image/png", "purpose": "maskable"}
  ],
  "theme_color": "#ffffff",
  "background_color": "#ffffff",
  "display": "standalone"
}
EOF
echo "Rendered man/figures/logo.png, logo/dampack-hex-600dpi.png and pkgdown/favicon/"
