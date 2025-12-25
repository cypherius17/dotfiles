# Dotfiles Backup & Sync

Automated daily backup of dotfiles to GitHub with built-in sensitive data detection.

## What Gets Synced

- `~/.bashrc`
- `~/.zshrc`
- `~/.tmux.conf`
- `~/.emacs.d/init.el`
- `~/.emacs.d/custom.el`
- `~/.emacs.d/lisp/` (directory)
- `~/.config/i3/` (directory)
- `~/.config/kitty/` (directory)

## Security Features

The sync process includes multiple layers of protection against accidentally publishing sensitive data:

1. **Pattern Detection**: Scans for common sensitive patterns (API keys, tokens, passwords, credentials)
2. **Automatic Exclusion**: `.gitignore` prevents auto-generated files and known sensitive patterns
3. **Pre-commit Check**: Runs sensitive data scan before every commit
4. **Manual Review**: Always review changes before first push

## Setup

### 1. Configure Git Remote

```bash
cd ~/lab/tools/dotfiles
git remote add origin https://github.com/YOUR_USERNAME/dotfiles.git
```

### 2. Initial Sync (with review)

Run a dry-run first to see what will be synced:

```bash
~/lab/tools/dotfiles/scripts/sync-dotfiles.sh --dry-run
```

Then run the actual sync:

```bash
~/lab/tools/dotfiles/scripts/sync-dotfiles.sh
```

Review the files that were copied:

```bash
cd ~/lab/tools/dotfiles
git status
git diff
```

### 3. Manual First Push

After reviewing the initial sync, manually commit and push:

```bash
cd ~/lab/tools/dotfiles
git add -A
git commit -m "Initial dotfiles commit"
git push -u origin main
```

## Automated Daily Sync

The systemd timer is already set up and running. It will:

- Run daily at **9:00 PM**
- Sync dotfiles from your home directory
- Check for sensitive data
- Commit and push changes automatically
- Log all activity to `~/lab/tools/dotfiles/sync.log`

### Timer Management

Check timer status:
```bash
systemctl --user status dotfiles-sync.timer
```

View next scheduled run:
```bash
systemctl --user list-timers dotfiles-sync.timer
```

Manually trigger sync:
```bash
systemctl --user start dotfiles-sync.service
```

View logs:
```bash
journalctl --user -u dotfiles-sync.service
# or
cat ~/lab/tools/dotfiles/sync.log
```

Stop automatic syncs:
```bash
systemctl --user stop dotfiles-sync.timer
systemctl --user disable dotfiles-sync.timer
```

Re-enable automatic syncs:
```bash
systemctl --user enable dotfiles-sync.timer
systemctl --user start dotfiles-sync.timer
```

## Manual Usage

### Sync dotfiles manually

```bash
~/lab/tools/dotfiles/scripts/sync-dotfiles.sh
```

### Check for sensitive data

```bash
~/lab/tools/dotfiles/scripts/check-sensitive.sh
```

### Full auto-update (sync + commit + push)

```bash
~/lab/tools/dotfiles/scripts/auto-update.sh
```

## Sensitive Data Patterns Detected

The check-sensitive script looks for:

- password, passwd, api_key, secret, token, credential
- Environment variables: AWS_, GITHUB_TOKEN, API_KEY
- Private keys: PEM files, RSA private keys
- API tokens: OpenAI (sk-*), Slack (xox*), GitHub (ghp_*, gho_*)

If any patterns are found, the sync will **fail** and require manual review.

## Adding New Dotfiles

Edit `~/lab/tools/dotfiles/scripts/sync-dotfiles.sh` and add new files/directories to sync:

```bash
# For individual files
copy_file "$HOME/.newconfig" "$REPO_DIR/.newconfig"

# For directories
sync_directory "$HOME/.config/newapp" "$REPO_DIR/config/newapp"
```

## Troubleshooting

### Sync fails with sensitive data warning

1. Review the flagged files
2. Remove or comment out sensitive lines
3. Add patterns to `.gitignore` if needed
4. Re-run the sync

### Timer not running

Check if systemd user services are enabled:
```bash
systemctl --user status
```

Enable linger if needed (allows user services to run when not logged in):
```bash
sudo loginctl enable-linger $USER
```

### Push fails

Ensure you've set up the remote and authenticated:
```bash
cd ~/lab/tools/dotfiles
git remote -v
git push origin main
```

## Files Overview

```
~/lab/tools/dotfiles/
├── .gitignore                      # Excludes sensitive and auto-generated files
├── README.md                       # This file
├── scripts/
│   ├── check-sensitive.sh         # Scans for sensitive data
│   ├── sync-dotfiles.sh           # Syncs files from home to repo
│   └── auto-update.sh             # Full automation (sync + commit + push)
├── .bashrc
├── .zshrc
├── .tmux.conf
├── emacs.d/
│   ├── init.el
│   ├── custom.el
│   └── lisp/
└── config/
    ├── i3/
    └── kitty/

~/.config/systemd/user/
├── dotfiles-sync.service          # Systemd service definition
└── dotfiles-sync.timer            # Daily timer (9:00 PM)
```
