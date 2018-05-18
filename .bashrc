# .bashrc
# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
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
    TERM=xterm
fi

# If running interactively, then:
if [ "$PS1" ]; then
    set -o ignoreeof
    set -o notify
    set -o noclobber
    set -o posix

    FIGNORE='.c~:.h~:.tex~:.ps~:.sty~:.aux:.lot:.lof:.toc'

    if [ ! "$LOGIN_SHELL" ]; then
	export PS1='\[$(if [ $? -eq 0 ]; then echo -en \e[1\;32m ; else echo -en \e[1\;31m; fi;)\]\u@\H\e[m > \e[1;34m\w\e[m \n\$ '
    fi

    HISTCONTROL=ignoreboth:erasedups
    HISTSIZE=-1
    HISTFILESIZE=-1
    MAILCHECK=60
fi

# User specific aliases and functions

[ -f ~/.fzf.bash ] && source ~/.fzf.bash

## Aliases
### Prompt interactive mv, cp, rm
alias mv='mv -i'
alias cp='cp -i'
alias rm='rm -i'

### Change ls accordin to kernel
if [ `uname` = "Darwin" ]; then
    alias ls='ls -G'
elif [ `uname` = "Linux" ]; then
    alias ls='ls --color=auto'
fi

## Un-coque emacs oh
export EDITOR=emacs
alias emacs='emacsclient --alternate-editor= --tty'

## Advanced shell options
shopt -s autocd
shopt -s cdspell
shopt -s globstar
shopt -s histappend
export LANG=en_US.utf8
export TERM=xterm-256color

## For local based configure files
if [ -f .bash_local ]; then
    source .bash_local
fi

### Use clang
export CC=/usr/bin/clang
export CXX=/usr/bin/clang++
