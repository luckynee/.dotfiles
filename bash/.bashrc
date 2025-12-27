# .bashrc
export EDITOR=emacs

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\[\e[32m\]\h\[\e[0m\] \W]\$ '

eval $(keychain --eval github_key id_ed25519)

# GPG Signing Configuration
export GPG_TTY=$(tty)

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
