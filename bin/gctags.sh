#!/bin/bash
 
export GTAGSLABEL=ctags
 
if [ -r $PWD/.globalrc ]; then
    GTAGSCONF=$PWD/.globalrc
elif [ -r $HOME/.globalrc ]; then
    GTAGSCONF=$HOME/.globalrc
elif [ -r /usr/local/share/gtags/gtags.conf ]; then
    GTAGSCONF=/usr/local/share/gtags/gtags.conf
fi
 
export GTAGSCONF
 
if which mkid > /dev/null; then
    gtags -I
else
    gtags
fi
