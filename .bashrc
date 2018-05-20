# .bashrc
# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi
# Source local definitions
if [ -f /~/.bash_local ]; then
    . .bash_local
fi

umask 022
ulimit -c 0

if [ "x$COLORFGBG" != "x" ]; then
    unset COLORFGBG
fi
if [ "x$COLORTERM" != "x" ]; then
    unset COLORTERM
fi
if [ "TERM" != "screen" ]; then
    TERM=xterm-256color
fi

# If not runnig interactively, don't do anything
[[ $- != *i* ]] && return

[[ $DISPLAY ]] && shopt -s checkwinsize

set -o notify
set -o noclobber
set -o posix
shopt -s autocd
shopt -s cdspell
shopt -s checkhash
shopt -s cmdhist
shopt -s direxpand
shopt -s dirspell
shopt -s globstar
shopt -s histappend

FIGNORE='.c~:.h~:.tex~:.ps~:.sty~:.aux:.lot:.lof:.toc'

PS1='[\u@\h \W]\$ '

HISTCONTROL=ignoreboth:erasedups
HISTSIZE=-1
HISTFILESIZE=-1
MAILCHECK=60

alias mv='mv -i'
alias cp='cp -i'
alias rm='rm -i'
if [ `uname` = "Darwin" ]; then
    alias ls='ls -G'
elif [ `uname` = "Linux" ]; then
    alias ls='ls --color=auto'
fi

EDITOR=emacs
alias emacs='emacsclient --alternate-editor= --tty'

if [[ $LANG = '' ]]; then
    LANG=en_US.utf8
fi

### Use clang
CC=/usr/bin/clang
CXX=/usr/bin/clang++
