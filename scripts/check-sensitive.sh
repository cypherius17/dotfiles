#!/bin/bash
# Script to check for sensitive data in dotfiles before committing

set -euo pipefail

# Color codes for output
RED='\033[0;31m'
YELLOW='\033[1;33m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

REPO_DIR="$HOME/lab/tools/dotfiles"
FOUND_ISSUES=0

# Patterns that indicate sensitive data
SENSITIVE_PATTERNS=(
    "password"
    "passwd"
    "api_key"
    "api-key"
    "apikey"
    "secret"
    "token"
    "credential"
    "auth_token"
    "private_key"
    "access_key"
    "SECRET"
    "API_KEY"
    "TOKEN"
    "GITHUB_TOKEN"
    "AWS_"
    "-----BEGIN.*PRIVATE KEY-----"
    "-----BEGIN RSA PRIVATE KEY-----"
    "sk-[a-zA-Z0-9]{32,}"  # OpenAI API keys
    "xox[baprs]-[0-9]{10,13}-[0-9]{10,13}-[a-zA-Z0-9]{24,}"  # Slack tokens
    "ghp_[a-zA-Z0-9]{36}"  # GitHub personal access tokens
    "gho_[a-zA-Z0-9]{36}"  # GitHub OAuth tokens
)

echo -e "${GREEN}Checking for sensitive data in dotfiles...${NC}\n"

# Function to check a file for sensitive patterns
check_file() {
    local file="$1"
    local relative_path="${file#$REPO_DIR/}"

    # Skip documentation and this script itself
    if [[ "$relative_path" == "README.md" ]] || \
       [[ "$relative_path" == ".gitignore" ]] || \
       [[ "$relative_path" == "scripts/check-sensitive.sh" ]]; then
        return
    fi

    for pattern in "${SENSITIVE_PATTERNS[@]}"; do
        # Use grep with proper pattern separation
        if grep -iE -- "$pattern" "$file" 2>/dev/null | grep -v "^[[:space:]]*#" > /dev/null 2>&1; then
            echo -e "${RED}WARNING: Potential sensitive data found in $relative_path${NC}"
            echo -e "${YELLOW}Pattern: $pattern${NC}"
            grep -iE -- "$pattern" "$file" 2>/dev/null | grep -v "^[[:space:]]*#" | head -3
            echo ""
            FOUND_ISSUES=1
        fi
    done
}

# Check all tracked files
cd "$REPO_DIR"
while IFS= read -r file; do
    if [ -f "$file" ]; then
        check_file "$file"
    fi
done < <(git ls-files 2>/dev/null || find . -type f -not -path '*/\.*' -not -path './scripts/*')

if [ $FOUND_ISSUES -eq 0 ]; then
    echo -e "${GREEN}No sensitive data patterns detected!${NC}"
    exit 0
else
    echo -e "${RED}Sensitive data patterns detected! Review the files above.${NC}"
    exit 1
fi
