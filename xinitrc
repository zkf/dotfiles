#!/bin/bash
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)
#

## Do host-specific things
case ${HOSTNAME} in
    watson)   # start some programs
              skype &
              #mediatomb -l ~/.mediatomb/mediatomb.log &
              #minidlna &
              tomahawk &
              firefox &
              dropboxd &

              # Generate a new background
              WP=/tmp/wall.png
              (~/bin/vladswallpaper $WP && habak -hi $WP) &
              ;;

    sherlock) # start some programs
              xinput set-prop 12 "Synaptics Coasting Speed" 10 30
              xmodmap ~/.Xmodmap
              nm-applet &
              dropboxd &
              batti &
              skype &
              xscreensaver -no-splash &

              # Generate a new background
              WP=/tmp/wall.png
              (~/bin/fortunewall $WP && habak -hi $WP) &
              ;;

    lestrade) skype &
              firefox &
              dropboxd &
              nm-applet &
              ;;
esac >& - # throw away output

# Allow boinc to use GPU
#xhost local:boinc

# load X settings
xrdb -load ~/.Xresources

# set X cursor
xsetroot -cursor_name left_ptr

# Speed up key repeating
xset r rate 250


wm_bin="$1"
wm_bin="xmonad"

if [[ xmonad == ${wm_bin} ]]
then
    #cairo-compmgr &
    trayer --transparent true --alpha 0 --tint 0x002b36 --edge top \
        --align right --heighttype pixel \
        --widthtype request &
    exec dbus-launch --sh-syntax --exit-with-session xmonad
elif [[ xfce4 == ${wm_bin} ]]
then
    exec startxfce4
fi

