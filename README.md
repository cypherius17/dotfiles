# Dotfiles

Automated daily backup with sensitive data detection. Syncs at 9 PM daily.

## Synced Files

`.bashrc`, `.zshrc`, `.tmux.conf`, `.emacs.d/`, `.config/i3/`, `.config/kitty/`, `.config/polybar/`, `.oh-my-zsh/custom/`

## Usage

```bash
# Restore dotfiles from remote (pulls and overwrites local configs)
~/lab/tools/dotfiles/scripts/restore-dotfiles.sh

# Restore with dry-run (preview changes without applying)
~/lab/tools/dotfiles/scripts/restore-dotfiles.sh --dry-run

# Manual sync (backup to repo)
~/lab/tools/dotfiles/scripts/sync-dotfiles.sh

# Check for sensitive data
~/lab/tools/dotfiles/scripts/check-sensitive.sh

# Full sync + commit + push
~/lab/tools/dotfiles/scripts/auto-update.sh

# View timer status
systemctl --user status dotfiles-sync.timer

# View logs
cat ~/lab/tools/dotfiles/sync.log
```

## Add New Files

Edit `scripts/sync-dotfiles.sh`:

```bash
copy_file "$HOME/.newconfig" "$REPO_DIR/.newconfig"
sync_directory "$HOME/.config/newapp" "$REPO_DIR/config/newapp"
```
