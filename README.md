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

### 3. Clone specflow

```bash
git clone https://github.com/tedmellors/specflow.git ~/emacs/.emacs.d/specflow
```

### 4. Launch Emacs

```bash
emacs
```

On first launch, `vterm` will recompile its native module for your architecture. This happens automatically.

## Notes

- Config detects GUI vs terminal mode automatically via `(display-graphic-p)`
- Packages are included in the repo for offline/reproducible setup
- The `vterm-module.so` is architecture-specific and will rebuild as needed
