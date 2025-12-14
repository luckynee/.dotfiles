#!/bin/bash
# Script to mark the current i3 window with a custom tag via Rofi

# Get the mark from Rofi
TAG=$(rofi -dmenu -p "Enter tag for current window:" -lines 0)

# If a tag is provided, apply it using i3-msg
if [ -n "$TAG" ]; then
    i3-msg mark "$TAG"
fi
