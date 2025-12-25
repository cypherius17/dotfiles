# Autostart tmux if not already inside a tmux session
if [ -z "$TMUX" ]; then
    tmux attach-t default || tmux new -s default
fi

alias vi="vim"

# --- Emacs Daemon Setup ---

# 'ec': Emacs Client (GUI)
# Creates a new window. Connects to daemon. If daemon fails, starts normal emacs.
alias ec="emacsclient -c -a 'emacs'"

# 'et': Emacs Terminal (TUI)
# Opens inside the current terminal pane. Perfect for Tmux.
alias et="emacsclient -nw"

# Editor variables
export EDITOR="emacsclient -nw"
export VISUAL="emacsclient -c -a 'emacs'"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
