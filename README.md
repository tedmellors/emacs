# Emacs Configuration

Personal Emacs configuration with all packages included.

## Setup on a New Machine (Local)

### 1. Install Emacs and dependencies

```bash
brew install emacs cmake libtool
```

### 2. Back up any existing config

```bash
mv ~/.emacs ~/.emacs.bak 2>/dev/null
mv ~/.emacs.d ~/.emacs.d.bak 2>/dev/null
```

### 3. Clone and symlink

```bash
git clone https://github.com/tedmellors/emacs.git ~/emacs
ln -sf ~/emacs/.emacs ~/.emacs
ln -sf ~/emacs/.emacs.d ~/.emacs.d
ln -sf ~/emacs/.tmux.conf ~/.tmux.conf
```

### 4. Clone dependencies

```bash
mkdir -p ~/.emacs.d/site-lisp
cd ~/.emacs.d/site-lisp
git clone https://github.com/manzaltu/claude-code-ide.el claude-code-ide
git clone https://github.com/lizqwerscott/mcp.el mcp
git clone https://github.com/tedmellors/org-weekly.git org-weekly
cd ~/.emacs.d
git clone https://github.com/tedmellors/specflow.git specflow
```

### 5. Launch Emacs

**GUI:**
```bash
emacs
```

**Terminal (in tmux):**
```bash
tmux new -s emacs
emacs -nw
```

On first launch, packages (helm, ranger, gptel, vterm, etc.) will auto-install from MELPA. vterm will compile its native module (requires cmake/libtool).

## Setup on a Remote Machine (SSH)

For running terminal Emacs on a remote server (e.g., Mac Mini via SSH).

### 1. SSH into the remote machine

```bash
ssh user@hostname
```

### 2. Install Emacs and dependencies

```bash
brew install emacs cmake libtool
```

### 3. Back up any existing config

```bash
mv ~/.emacs ~/.emacs.bak 2>/dev/null
mv ~/.emacs.d ~/.emacs.d.bak 2>/dev/null
```

### 4. Clone and symlink

```bash
git clone https://github.com/tedmellors/emacs.git ~/emacs
ln -sf ~/emacs/.emacs ~/.emacs
ln -sf ~/emacs/.emacs.d ~/.emacs.d
ln -sf ~/emacs/.tmux.conf ~/.tmux.conf
```

### 5. Clone dependencies

```bash
mkdir -p ~/.emacs.d/site-lisp
cd ~/.emacs.d/site-lisp
git clone https://github.com/manzaltu/claude-code-ide.el claude-code-ide
git clone https://github.com/lizqwerscott/mcp.el mcp
git clone https://github.com/tedmellors/org-weekly.git org-weekly
cd ~/.emacs.d
git clone https://github.com/tedmellors/specflow.git specflow
```

### 6. Fix terminal colors for SSH

Add these to `~/.zshrc` on the **remote** machine:

```bash
echo 'export TERM=xterm-256color' >> ~/.zshrc
echo 'export COLORTERM=truecolor' >> ~/.zshrc
```

Then **disconnect and reconnect** via SSH for the changes to take effect. `source ~/.zshrc` alone is not sufficient — a full SSH reconnect is required.

### 7. Launch Emacs

```bash
emacs
```

Packages will auto-install on first launch. If you get errors about missing packages, run `M-x package-refresh-contents` inside Emacs and restart.

## tmux

Prefix is remapped from `C-b` to `C-z` so `C-b` (`backward-char`) reaches Emacs. `C-z` is unbound in Emacs, so passing it through is harmless.

Common commands after the prefix change:

| Key       | Action                  |
|-----------|-------------------------|
| C-z d     | Detach session          |
| C-z c     | Create window           |
| C-z 0/1/2 | Switch to window N      |
| C-z :     | Command prompt          |

To reload the config in a running tmux session without restart: `C-z :` then type `source-file ~/.tmux.conf`.

## iTerm2 Settings (for terminal Emacs)

See [ITERM2_CONFIG.org](ITERM2_CONFIG.org) for full details.

**Required settings (iTerm2 → Settings → Profiles → Terminal):**
- Scrollback Lines: **0**
- **UNCHECK** "Save lines to scrollback in alternate screen mode"
- iTerm2 may need a **full restart** after changing these settings

**C-RET and M-S-RET**: Set to "Ignore" in iTerm2 (Settings → Profiles → Keys → Key Mappings) to prevent garbage escape sequences in terminal Emacs.

**Focus Reporting**: If you see "OI" printed when switching focus, disable it:

```bash
defaults write com.googlecode.iterm2 FocusReportingEnabled -bool false
```

Then restart iTerm2.

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
