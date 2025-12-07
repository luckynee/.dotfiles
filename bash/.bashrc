# .bashrc
export EDITOR=emacs



# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

# GPG Signing Configuration
export GPG_TTY=$(tty)
