#!/bin/bash
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)
#

## Start host-specific programs
case ${HOSTNAME} in 
    watson)   skype &
              mediatomb -l ~/.mediatomb/mediatomb.log &
              #minidlna &
              #pms &
              tomahawk &
              firefox &
              dropboxd & 
              ;;

    sherlock) xinput set-prop 12 "Synaptics Coasting Speed" 10 30
              xmodmap ~/.Xmodmap
              nm-applet & 
              dropboxd & 
              batti &
              skype &
              ;;
esac &> - # throw away output

# Allow boinc to use GPU
#xhost local:boinc

# load X settings
xrdb -load ~/.Xresources 

# set X cursor
xsetroot -cursor_name left_ptr

# Speed up key repeating
xset r rate 250

# Generate a new background
WP=/tmp/wall.png
(~/bin/fortunewall $WP && habak -hi $WP) &

wm_bin="$1"

if [[ xmonad == ${wm_bin} ]]
then
    #cairo-compmgr &
    xscreensaver -no-splash &
    trayer --transparent true --alpha 0 --tint 0x002b36 --edge top \
        --align right --height 16  --heighttype pixel \
        --widthtype request &
    exec ck-launch-session dbus-launch --sh-syntax --exit-with-session xmonad
elif [[ xfce4 == ${wm_bin} ]]
then
    exec startxfce4
fi

