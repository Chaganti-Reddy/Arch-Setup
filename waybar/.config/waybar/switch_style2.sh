#!/bin/bash
killall waybar
SDIR="$HOME/.config/waybar"
waybar -c "$SDIR"/config2.jsonc -s "$SDIR"/style2.css &
