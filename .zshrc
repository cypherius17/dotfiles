# Path to your Oh My Zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Theme configuration
ZSH_THEME="gruvbox"

# Plugins
# (Note: zsh-syntax-highlighting must be last)
plugins=(
  git
  archlinux
  rust
  python
  docker
  docker-compose
  sudo
  z
  scala
  sbt
  gradle
  terraform
  kubectl
  helm
  zsh-autosuggestions
  zsh-syntax-highlighting
)

# Load Oh My Zsh
source $ZSH/oh-my-zsh.sh

# --- User Configuration ---

# Editor variables
export EDITOR="emacsclient -nw"
export VISUAL="emacsclient -c -a 'emacs'"

# Add local bin to PATH
export PATH="$HOME/.local/bin:$PATH"

# Aliases
alias vi="vim"

# Emacs Aliases
# 'ec': Emacs Client (GUI) - Creates new window, connects to daemon
alias ec="emacsclient -c -a 'emacs'"
# 'et': Emacs Terminal (TUI) - Opens in current terminal pane
alias et="emacsclient -nw"

# --- Tmux Autostart ---
# (Ensures tmux starts only if not already inside one)
if [ -z "$TMUX" ]; then
    tmux attach -t default || tmux new -s default
fi
