# Emacs Configuration

Personal Emacs configuration with all packages included.

## Setup on a New Machine

### 1. Install dependencies

**macOS (Homebrew):**
```bash
brew install cmake libtool
```

**Ubuntu/Debian:**
```bash
sudo apt install cmake libtool-bin
```

**Fedora:**
```bash
sudo dnf install cmake libtool
```

### 2. Clone and symlink

```bash
git clone https://github.com/tedmellors/emacs.git ~/emacs
ln -s ~/emacs/.emacs ~/.emacs
ln -s ~/emacs/.emacs.d ~/.emacs.d
```

### 3. Clone dependencies

```bash
git clone https://github.com/tedmellors/specflow.git ~/emacs/.emacs.d/specflow
git clone https://github.com/manzaltu/claude-code-ide.el ~/emacs/.emacs.d/site-lisp/claude-code-ide
git clone https://github.com/lizqwerscott/mcp.el ~/emacs/.emacs.d/site-lisp/mcp
```

### 4. Launch Emacs

**GUI:**
```bash
emacs
```

**Terminal (in tmux):**
```bash
tmux new -s emacs
emacs -nw
```

Use `-nw` (no window) to run Emacs in terminal mode inside tmux.

On first launch:
- Packages (helm, ranger, gptel, vterm) will auto-install from MELPA
- vterm will compile its native module (requires cmake/libtool)

## iTerm2 Settings (for terminal Emacs)

If using iTerm2 and you see "OI" printed when switching focus, disable focus reporting:

```bash
defaults write com.googlecode.iterm2 FocusReportingEnabled -bool false
```

Then restart iTerm2.

Or manually: iTerm2 → Settings → Advanced → search "Focus Reporting" → set to No.

## Keybindings

### Tabs

| Key   | Action                              |
|-------|-------------------------------------|
| M-n   | New tab (prompts for name)          |
| M-N   | Close tab (with confirmation)       |
| M-r   | Rename tab                          |
| M-]   | Next tab                            |
| M-[   | Previous tab                        |
| M-}   | Move tab right                      |
| M-{   | Move tab left                       |

### Windows

| Key   | Action                              |
|-------|-------------------------------------|
| M-o   | Next window                         |
| M-O   | Previous window                     |
| M-0   | Delete window                       |
| M-1   | Delete other windows                |
| M-2   | Split below                         |
| M-3   | Split right                         |
| M-i   | Enlarge window (vertical)           |
| M-m   | Shrink window (vertical)            |
| M-h   | Enlarge window (horizontal)         |
| M-H   | Shrink window (horizontal)          |
| C-c f | Freeze/unfreeze window layout       |

### Buffers & Files

| Key   | Action                              |
|-------|-------------------------------------|
| M-k   | Kill current buffer                 |
| C-c r | Revert buffer (no prompts)          |
| C-c e | Reload ~/.emacs config              |
| C-c E | Eval current buffer                 |
| C-h   | Show kill ring (helm)               |
| M-x   | Helm M-x                            |
| C-c p | Projectile command prefix           |
| C-c j | Avy jump to char                    |

### Org-mode

| Key     | Action                              |
|---------|-------------------------------------|
| M-RET   | Insert heading/item after content   |
| M-p     | Move subtree/item up                |
| M-P     | Move subtree/item down              |
| M-l     | Promote subtree                     |
| M-L     | Demote subtree                      |
| C-c C-o | Open link (file: new tab, web: browser) |
| C-c w   | Toggle word wrap / truncate         |
| C-c a   | Org agenda                          |

### Org Agenda Views

| Key     | Action                              |
|---------|-------------------------------------|
| C-c a n | All NEXT tasks                      |
| C-c a p | In Progress                         |
| C-c a m | Scheduled Meetings                  |
| C-c a d | Dashboard (all active)              |

### gptel (LLM)

| Key     | Action                              |
|---------|-------------------------------------|
| C-c g   | Open gptel at bottom                |
| C-c m   | Switch gptel model                  |
| C-c RET | Send to gptel                       |
| C-c b   | Reattach buffer to bottom           |

### Claude Code IDE

| Key       | Action                            |
|-----------|-----------------------------------|
| C-c '     | Claude Code IDE menu (terminal)   |
| C-c C-'   | Claude Code IDE menu (GUI)        |

### Ranger

| Key | Action                              |
|-----|-------------------------------------|
| +   | Create directory                    |
| yp  | Copy absolute file path             |
| yn  | Copy filename                       |
| yd  | Copy directory path                 |

### Other

| Key           | Action                          |
|---------------|---------------------------------|
| C-wheel-up    | Increase text size              |
| C-wheel-down  | Decrease text size              |

## iTerm2 Settings

- **C-RET**: Set to "Ignore" in iTerm2 (Settings > Profiles > Keys > Key Mappings) to prevent garbage escape sequences in terminal Emacs.
- **Focus Reporting**: If you see "OI" printed when switching focus, disable it:
  ```bash
  defaults write com.googlecode.iterm2 FocusReportingEnabled -bool false
  ```

## Notes

- Config detects GUI vs terminal mode automatically via `(display-graphic-p)`
- Packages auto-install on first boot if missing
