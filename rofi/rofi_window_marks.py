#!/usr/bin/env python3
import json
import subprocess
import os
import sys

# Cache file path
CACHE_FILE = os.path.expanduser("~/.cache/rofi_window_marks_icons.json")

# Directories to search for .desktop files
DESKTOP_DIRS = [
    os.path.expanduser("~/.local/share/applications"),
    "/usr/share/applications",
    "/usr/local/share/applications"
]

# Unity Icon Paths
UNITY_6_ICON = "/home/luckymp/Unity/Hub/Editor/6000.0.63f1/Editor/Data/Resources/LargeUnityIcon.png"
UNITY_2022_ICON = "/home/luckymp/Unity/Hub/Editor/2022.3.62f3/Editor/Data/Resources/LargeUnityIcon.png"

def get_i3_tree():
    result = subprocess.run(['i3-msg', '-t', 'get_tree'], capture_output=True, text=True)
    if result.returncode != 0:
        return None
    return json.loads(result.stdout)

def find_tagged_windows(node, windows_list):
    if node.get('window') is not None and node.get('name') != 'i3bar':
        marks = node.get('marks')
        if marks and len(marks) > 0:
            windows_list.append({
                'id': node.get('id'),
                'marks': marks,
                'class': node.get('window_properties', {}).get('class', 'Unknown'),
                'instance': node.get('window_properties', {}).get('instance', 'Unknown'),
                'title': node.get('name', '') # Get window title
            })
    
    for child in node.get('nodes', []) + node.get('floating_nodes', []):
        find_tagged_windows(child, windows_list)

def load_icon_cache():
    if os.path.exists(CACHE_FILE):
        try:
            with open(CACHE_FILE, 'r') as f:
                return json.load(f)
        except:
            pass
    return None

def save_icon_cache(cache):
    try:
        os.makedirs(os.path.dirname(CACHE_FILE), exist_ok=True)
        with open(CACHE_FILE, 'w') as f:
            json.dump(cache, f)
    except:
        pass

def build_icon_map():
    cached = load_icon_cache()
    if cached:
        return cached

    icon_map = {}
    for directory in DESKTOP_DIRS:
        if not os.path.exists(directory): continue
        try:
            for filename in os.listdir(directory):
                if not filename.endswith(".desktop"): continue
                filepath = os.path.join(directory, filename)
                try:
                    with open(filepath, 'r', errors='ignore') as f:
                        icon = None
                        wm_class = None
                        for line in f:
                            stripped = line.strip()
                            if stripped.startswith('Icon='):
                                icon = stripped.split('=')[1]
                            if stripped.startswith('StartupWMClass='):
                                wm_class = stripped.split('=')[1]
                        
                        if icon:
                            name_no_ext = filename[:-8].lower()
                            icon_map[name_no_ext] = icon
                            if wm_class:
                                icon_map[wm_class.lower()] = icon
                except:
                    pass
        except:
            pass
    
    save_icon_cache(icon_map)
    return icon_map

def main():
    tree = get_i3_tree()
    if tree is None:
        sys.exit(1)

    tagged_windows = []
    find_tagged_windows(tree, tagged_windows)
    
    if not tagged_windows:
        subprocess.run(['rofi', '-e', 'No tagged windows found'])
        return

    icon_map = build_icon_map()

    rofi_input_lines = []
    for w in tagged_windows:
        mark = w['marks'][0]
        app_class = w['class']
        app_instance = w['instance']
        title = w['title']
        
        # Default lookup
        icon = icon_map.get(app_class.lower()) or icon_map.get(app_instance.lower()) or app_class.lower()
        
        # Custom Logic for Unity
        if app_class == "Unity":
            # Check title for version
            if "6000." in title:
                icon = UNITY_6_ICON
            elif "2022." in title:
                icon = UNITY_2022_ICON
            # Else fallback to generic "unity" (which might match nothing or unityhub)

        # Format
        display_str = f"[{mark}]"
        padded_str = f"{display_str:<25}"
        rofi_input_lines.append(f"{padded_str} {app_class}\0icon\x1f{icon}")

    rofi_input = "\n".join(rofi_input_lines)

    rofi_cmd = ['rofi', '-dmenu', '-p', 'Tagged Apps', '-i', '-show-icons']
    
    process = subprocess.Popen(rofi_cmd, stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)
    stdout, _ = process.communicate(input=rofi_input)
    
    selected = stdout.strip()
    
    if selected:
        try:
            parts = selected.split(']')
            if len(parts) > 1:
                mark = parts[0].replace('[', '').strip()
                if mark:
                    subprocess.run(['i3-msg', f'[con_mark="{mark}"] focus'], capture_output=True)
        except:
            pass

if __name__ == "__main__":
    main()
