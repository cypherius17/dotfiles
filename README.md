# dotfiles

Arch Linux config for pepper (Ryzen 9 7900X, Niri/Wayland), with a parallel
cross-platform config for macOS.

## Stack
- **WM (Linux only):** Niri (Wayland scrollable tiling)
- **Bar (Linux only):** Waybar (Gruvbox)
- **Notifications (Linux only):** mako
- **Input (Linux only):** fcitx5 + unikey (Vietnamese)
- **Terminal:** Ghostty
- **Shell:** zsh + Starship (Gruvbox Rainbow)
- **Multiplexer:** tmux (Ctrl+A prefix)
- **Editor:** Emacs (daemon mode, evil-mode)
- **Launcher (Linux only):** fuzzel

## Layout
Each top-level directory is an independent Stow package:

| Package    | Cross-platform? | Notes                                  |
|------------|------------------|-----------------------------------------|
| `zsh`      | yes              | OS-detects for `ls` color flag          |
| `tmux`     | yes              | OS-detects for clipboard (`pbcopy`/`wl-copy`) |
| `starship` | yes              |                                          |
| `emacs`    | yes              | has its own `(eq system-type 'darwin)` guards |
| `ghostty`  | yes              |                                          |
| `niri`     | Linux only       | Wayland compositor, no macOS equivalent |
| `waybar`   | Linux only       | Wayland-only status bar                 |

## Install

Clone first on either OS:
```bash
git clone git@github.com:cypherius17/dotfiles.git ~/dotfiles
cd ~/dotfiles
```

**Linux (pepper):**
```bash
stow --target=$HOME zsh tmux starship emacs ghostty niri waybar
```

**macOS:**
```bash
stow --target=$HOME zsh tmux starship emacs ghostty
```

Adding or removing a package only touches that package's symlinks — `stow -D <package>` to unlink one without affecting the rest.
