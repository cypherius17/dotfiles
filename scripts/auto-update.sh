#!/bin/bash
# Main automation script to sync, check, commit, and push dotfiles

set -euo pipefail

# Color codes
RED='\033[0;31m'
YELLOW='\033[1;33m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m'

REPO_DIR="$HOME/lab/tools/dotfiles"
LOG_FILE="$REPO_DIR/sync.log"
MAX_LOG_SIZE=1048576  # 1MB

# Function to log messages
log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1" | tee -a "$LOG_FILE"
}

# Rotate log if it's too large
if [ -f "$LOG_FILE" ] && [ $(stat -f%z "$LOG_FILE" 2>/dev/null || stat -c%s "$LOG_FILE" 2>/dev/null || echo 0) -gt $MAX_LOG_SIZE ]; then
    mv "$LOG_FILE" "$LOG_FILE.old"
    log "Log rotated"
fi

log "Starting dotfiles auto-update..."

# Change to repo directory
cd "$REPO_DIR"

# Sync dotfiles from home directory
log "Running sync script..."
if ! "$REPO_DIR/scripts/sync-dotfiles.sh"; then
    log "ERROR: Sync failed or sensitive data detected!"
    exit 1
fi

# Check if there are any changes
if git diff --quiet && git diff --cached --quiet; then
    log "No changes detected. Nothing to commit."
    exit 0
fi

# Show what changed
log "Changes detected:"
git status --short | tee -a "$LOG_FILE"

# Stage all changes
git add -A

# Create commit message
COMMIT_MSG="Auto-update dotfiles $(date '+%Y-%m-%d %H:%M:%S')"

# Run pre-commit sensitive data check one more time
log "Running final sensitive data check..."
if ! "$REPO_DIR/scripts/check-sensitive.sh" >> "$LOG_FILE" 2>&1; then
    log "ERROR: Sensitive data check failed before commit!"
    git reset
    exit 1
fi

# Commit changes
log "Committing changes..."
git commit -m "$COMMIT_MSG"

# Push to remote (if remote is configured)
if git remote | grep -q 'origin'; then
    log "Pushing to remote repository..."
    if git push origin main; then
        log "Successfully pushed to remote!"
    else
        log "WARNING: Failed to push to remote. Will retry on next run."
        exit 1
    fi
else
    log "WARNING: No remote repository configured. Skipping push."
    log "Run: cd $REPO_DIR && git remote add origin <your-repo-url>"
fi

log "Dotfiles auto-update completed successfully!"
