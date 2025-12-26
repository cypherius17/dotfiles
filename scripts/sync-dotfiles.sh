#!/bin/bash
# Script to sync dotfiles from home directory to dotfiles repo

set -euo pipefail

# Color codes
RED='\033[0;31m'
YELLOW='\033[1;33m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m'

REPO_DIR="$HOME/lab/tools/dotfiles"
DRY_RUN=0

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --dry-run)
            DRY_RUN=1
            shift
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

if [ $DRY_RUN -eq 1 ]; then
    echo -e "${BLUE}Running in DRY-RUN mode - no files will be copied or committed${NC}\n"
fi

# Function to copy file with verification
copy_file() {
    local src="$1"
    local dest="$2"

    if [ ! -f "$src" ]; then
        echo -e "${YELLOW}Warning: $src does not exist, skipping${NC}"
        return
    fi

    if [ $DRY_RUN -eq 1 ]; then
        echo -e "${BLUE}Would copy: $src -> $dest${NC}"
    else
        cp "$src" "$dest"
        echo -e "${GREEN}Copied: $src -> $dest${NC}"
    fi
}

# Function to sync directory
sync_directory() {
    local src="$1"
    local dest="$2"

    if [ ! -d "$src" ]; then
        echo -e "${YELLOW}Warning: $src does not exist, skipping${NC}"
        return
    fi

    if [ $DRY_RUN -eq 1 ]; then
        echo -e "${BLUE}Would sync directory: $src -> $dest${NC}"
    else
        # Create destination if it doesn't exist
        mkdir -p "$dest"
        # Sync directory, excluding .gitignore patterns
        rsync -av --exclude='*.log' --exclude='*~' --exclude='.DS_Store' \
              --exclude='auto-save-list/' --exclude='eln-cache/' --exclude='elpa/' \
              "$src/" "$dest/"
        echo -e "${GREEN}Synced directory: $src -> $dest${NC}"
    fi
}

echo -e "${GREEN}Starting dotfiles sync...${NC}\n"

# Sync individual dotfiles
copy_file "$HOME/.bashrc" "$REPO_DIR/.bashrc"
copy_file "$HOME/.zshrc" "$REPO_DIR/.zshrc"
copy_file "$HOME/.tmux.conf" "$REPO_DIR/.tmux.conf"

# Sync emacs.d (only init.el, custom.el, and lisp/ directory)
if [ -d "$HOME/.emacs.d" ]; then
    mkdir -p "$REPO_DIR/emacs.d/lisp"
    copy_file "$HOME/.emacs.d/init.el" "$REPO_DIR/emacs.d/init.el"
    copy_file "$HOME/.emacs.d/custom.el" "$REPO_DIR/emacs.d/custom.el"

    if [ -d "$HOME/.emacs.d/lisp" ]; then
        sync_directory "$HOME/.emacs.d/lisp" "$REPO_DIR/emacs.d/lisp"
    fi
fi

# Sync config directories
sync_directory "$HOME/.config/i3" "$REPO_DIR/config/i3"
sync_directory "$HOME/.config/kitty" "$REPO_DIR/config/kitty"
sync_directory "$HOME/.config/polybar" "$REPO_DIR/config/polybar"

# Sync oh-my-zsh custom configurations
if [ -d "$HOME/.oh-my-zsh/custom" ]; then
    sync_directory "$HOME/.oh-my-zsh/custom" "$REPO_DIR/oh-my-zsh/custom"
fi

echo -e "\n${GREEN}Sync completed!${NC}\n"

# Run sensitive data check
if [ $DRY_RUN -eq 0 ]; then
    echo -e "${BLUE}Running sensitive data check...${NC}\n"
    if "$REPO_DIR/scripts/check-sensitive.sh"; then
        echo -e "\n${GREEN}Sensitive data check passed!${NC}"
    else
        echo -e "\n${RED}ERROR: Sensitive data check failed!${NC}"
        echo -e "${RED}Please review and remove sensitive data before committing.${NC}"
        exit 1
    fi
fi
