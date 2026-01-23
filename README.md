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

```bash
emacs
```

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

## Notes

- Config detects GUI vs terminal mode automatically via `(display-graphic-p)`
- Packages auto-install on first boot if missing
