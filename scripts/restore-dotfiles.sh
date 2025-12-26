#!/bin/bash
# Script to restore dotfiles from repo to home directory

set -euo pipefail

# Color codes
RED='\033[0;31m'
YELLOW='\033[1;33m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m'

REPO_DIR="$HOME/lab/tools/dotfiles"
DRY_RUN=0
SKIP_PULL=0

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --dry-run)
            DRY_RUN=1
            shift
            ;;
        --skip-pull)
            SKIP_PULL=1
            shift
            ;;
        -h|--help)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Options:"
            echo "  --dry-run     Show what would be restored without making changes"
            echo "  --skip-pull   Skip git pull, restore from current repo state"
            echo "  -h, --help    Show this help message"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            echo "Use -h or --help for usage information"
            exit 1
            ;;
    esac
done

if [ $DRY_RUN -eq 1 ]; then
    echo -e "${BLUE}Running in DRY-RUN mode - no files will be modified${NC}\n"
fi

# Function to restore file with verification
restore_file() {
    local src="$1"
    local dest="$2"

    if [ ! -f "$src" ]; then
        echo -e "${YELLOW}Warning: $src does not exist in repo, skipping${NC}"
        return
    fi

    if [ $DRY_RUN -eq 1 ]; then
        echo -e "${BLUE}Would restore: $src -> $dest${NC}"
    else
        # Backup existing file if it exists and is different
        if [ -f "$dest" ] && ! cmp -s "$src" "$dest"; then
            backup="${dest}.backup.$(date +%Y%m%d_%H%M%S)"
            cp "$dest" "$backup"
            echo -e "${YELLOW}Backed up existing file to: $backup${NC}"
        fi
        cp "$src" "$dest"
        echo -e "${GREEN}Restored: $src -> $dest${NC}"
    fi
}

# Function to restore directory
restore_directory() {
    local src="$1"
    local dest="$2"

    if [ ! -d "$src" ]; then
        echo -e "${YELLOW}Warning: $src does not exist in repo, skipping${NC}"
        return
    fi

    if [ $DRY_RUN -eq 1 ]; then
        echo -e "${BLUE}Would restore directory: $src -> $dest${NC}"
    else
        # Create destination if it doesn't exist
        mkdir -p "$dest"
        # Sync directory
        rsync -av --exclude='*.log' --exclude='*~' --exclude='.DS_Store' \
              "$src/" "$dest/"
        echo -e "${GREEN}Restored directory: $src -> $dest${NC}"
    fi
}

# Pull latest changes from remote
if [ $SKIP_PULL -eq 0 ]; then
    echo -e "${GREEN}Pulling latest changes from remote...${NC}\n"
    if [ $DRY_RUN -eq 1 ]; then
        echo -e "${BLUE}Would run: git pull${NC}\n"
    else
        cd "$REPO_DIR"
        if ! git pull; then
            echo -e "${RED}ERROR: Failed to pull from remote${NC}"
            echo -e "${YELLOW}You can use --skip-pull to restore from current repo state${NC}"
            exit 1
        fi
        echo ""
    fi
else
    echo -e "${YELLOW}Skipping git pull, restoring from current repo state${NC}\n"
fi

echo -e "${GREEN}Starting dotfiles restoration...${NC}\n"

# Restore individual dotfiles
restore_file "$REPO_DIR/.bashrc" "$HOME/.bashrc"
restore_file "$REPO_DIR/.zshrc" "$HOME/.zshrc"
restore_file "$REPO_DIR/.tmux.conf" "$HOME/.tmux.conf"

# Restore emacs.d
if [ -d "$REPO_DIR/emacs.d" ]; then
    mkdir -p "$HOME/.emacs.d"
    restore_file "$REPO_DIR/emacs.d/init.el" "$HOME/.emacs.d/init.el"
    restore_file "$REPO_DIR/emacs.d/custom.el" "$HOME/.emacs.d/custom.el"

    if [ -d "$REPO_DIR/emacs.d/lisp" ]; then
        restore_directory "$REPO_DIR/emacs.d/lisp" "$HOME/.emacs.d/lisp"
    fi
fi

# Restore config directories
restore_directory "$REPO_DIR/config/i3" "$HOME/.config/i3"
restore_directory "$REPO_DIR/config/kitty" "$HOME/.config/kitty"
restore_directory "$REPO_DIR/config/polybar" "$HOME/.config/polybar"

# Restore oh-my-zsh custom configurations
if [ -d "$REPO_DIR/oh-my-zsh/custom" ]; then
    mkdir -p "$HOME/.oh-my-zsh/custom"
    restore_directory "$REPO_DIR/oh-my-zsh/custom" "$HOME/.oh-my-zsh/custom"
fi

echo -e "\n${GREEN}Dotfiles restoration completed!${NC}\n"

if [ $DRY_RUN -eq 0 ]; then
    echo -e "${YELLOW}Note: You may need to reload your shell or configuration files:${NC}"
    echo -e "  - For shell: ${BLUE}source ~/.zshrc${NC} or ${BLUE}source ~/.bashrc${NC}"
    echo -e "  - For tmux: ${BLUE}tmux source-file ~/.tmux.conf${NC}"
    echo -e "  - For i3: ${BLUE}i3-msg reload${NC} or ${BLUE}i3-msg restart${NC}"
fi
