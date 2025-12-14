#!/bin/bash
exec 2> /tmp/rofi_debug.log
set -x

TREE=$(i3-msg -t get_tree)

# Generate list in format: "[MARK]       Application Class" with Icon
# 1. We format the tag as "[TAG]"
# 2. We pad it to 15 characters for alignment
# 3. We add the icon using the special \0icon\x1f separator
# 4. We use the lowercase class name for the icon (usually works best)

LIST=$(echo "$TREE" | jq -r '
  recurse(.nodes[]?, .floating_nodes[]?)
  | select(.window != null and .name != "i3bar" and .marks and (.marks | length) > 0)
  | (.marks[0]) as $mark
  | (.window_properties.class // "application-default-icon") as $class
  | "[" + $mark + "]" as $tag_raw
  | ($tag_raw + "               ")[:15] as $padded_tag
  | "\($padded_tag) \($class)\u0000icon\u001f\($class | ascii_downcase)"
')

if [ -z "$LIST" ]; then
   rofi -e "No tagged windows found"
   exit 0
fi

# -show-icons: Enable icon display
SELECTED=$(echo "$LIST" | rofi -dmenu -p "Tagged Apps" -i -show-icons)

if [ -n "$SELECTED" ]; then
    # Extract the mark from between the brackets [MARK]
    # We take the first part of the string (the padded tag), trim spaces, remove brackets.
    # The string starts like "[MARK]       Firefox..."

    # Extract the first "word" which is [MARK]
    RAW_TAG=$(echo "$SELECTED" | awk '{print $1}')

    # Remove brackets
    MARK=$(echo "$RAW_TAG" | tr -d '[]')

    if [ -n "$MARK" ]; then
        i3-msg "[con_mark=\"$MARK\"] focus"
    fi
fi
