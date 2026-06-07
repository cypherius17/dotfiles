# dotfiles

Arch Linux config for pepper (Ryzen 9 7900X, Niri/Wayland).

## Stack
- **WM:** Niri (Wayland scrollable tiling)
- **Terminal:** Ghostty
- **Shell:** zsh + Starship (Gruvbox Rainbow)
- **Multiplexer:** tmux (Ctrl+A prefix)
- **Launcher:** fuzzel
- **Bar:** Waybar (Gruvbox)
- **Notifications:** mako
- **Input:** fcitx5 + unikey (Vietnamese)

## Install
```bash
git clone git@github.com:cypherius17/dotfiles.git ~/dotfiles
cd ~/dotfiles
stow --target=$HOME .
```
