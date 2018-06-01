# .bashrc
# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

# Source local definitions
if [ -f "$HOME"/.bash_local ]; then
    . "$HOME"/.bash_local
fi

umask 022
ulimit -c 0

# If not runnig interactively, don't do anything
[[ $- != *i* ]] && return

if [ "x$COLORFGBG" != "x" ]; then
    unset COLORFGBG
fi
if [ "x$COLORTERM" != "x" ]; then
    unset COLORTERM
fi
if [ "TERM" != "screen" ]; then
    TERM=xterm-256color
fi

[[ $DISPLAY ]] && shopt -s checkwinsize

set -o notify
set -o noclobber

PS1='[\u@\h \W]\$ '

HISTCONTROL=ignoreboth:erasedups
HISTSIZE=-1
HISTFILESIZE=-1
MAILCHECK=60
shopt -s histappend

alias mv='mv -i'
alias cp='cp -i'
alias rm='rm -i'
if [ `uname` = "Darwin" ]; then
    alias ls='ls -G'
elif [ `uname` = "Linux" ]; then
    alias ls='ls --color=auto'
fi

# EDITOR='emacsclient --alternate-editor= --tty'
EDITOR='emacsclient -a "" -nw'
alias emacs="$EDITOR"


if [[ $LANG = '' ]]; then
    LANG=en_US.utf8
fi

CC=/usr/bin/clang
CXX=/usr/bin/clang++

BROWSER=/usr/bin/firefox

