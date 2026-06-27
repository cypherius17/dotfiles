# ── History ─────────────────────────────────────────────
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE
setopt SHARE_HISTORY
setopt AUTO_CD

# ── Completion ───────────────────────────────────────────
autoload -Uz compinit && compinit
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# ── Vi mode ──────────────────────────────────────────────
bindkey -v
KEYTIMEOUT=1

# Restore useful keys that vi mode breaks
bindkey '^R' history-incremental-search-backward
bindkey '^A' beginning-of-line
bindkey '^E' end-of-line
bindkey '^?' backward-delete-char

# Show vi mode indicator in prompt
function zle-keymap-select {
  if [[ ${KEYMAP} == vicmd ]]; then
    echo -ne '\e[2 q'  # block cursor = normal mode
  else
    echo -ne '\e[6 q'  # bar cursor = insert mode
  fi
}
zle -N zle-keymap-select

# ── Aliases ──────────────────────────────────────────────
alias ls='ls --color=auto'
alias ll='ls -lah --color=auto'
alias la='ls -A --color=auto'
alias grep='grep --color=auto'
alias c='clear'
alias ..='cd ..'
alias ...='cd ../..'
alias gs='git status'
alias ga='git add'
alias gc='git commit'
alias gp='git push'
alias gl='git log --oneline --graph --decorate'
# ── Emacs client ─────────────────────────────────────────
alias e='emacsclient -c --no-wait --alternate-editor="emacs"'
alias et='emacsclient -nw --alternate-editor="emacs"'

# ── Secrets (not in git) ─────────────────────────────────
[[ -f ~/.secrets ]] && source ~/.secrets

# ── Plugins ──────────────────────────────────────────────
source ~/.local/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
source ~/.local/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Autosuggestion settings
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=#928374'
ZSH_AUTOSUGGEST_STRATEGY=(history completion)

# Accept suggestion with right arrow (default) or Ctrl+F
bindkey '^F' autosuggest-accept

# Auto-attach to tmux
if command -v tmux &>/dev/null && [ -z "$TMUX" ] && [ -z "$EMACS" ]; then
    tmux attach -t main 2>/dev/null || tmux new -s main
fi

# ── Starship prompt ──────────────────────────────────────
eval "$(starship init zsh)"
export PATH="$HOME/.npm-global/bin:$PATH"
