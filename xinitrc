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
              xcompmgr -D 6 -C -f &
              dunst &

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
              udiskie &
              xcompmgr -D 6 -C -f &
              dunst &
              (sleep 2 && alock -auth pam -bg shade:90) &
              ;;
esac >& - # throw away output

# Allow boinc to use GPU
#xhost local:boinc

# load X settings
xrdb -load ~/.Xresources
Xres_local="$HOME/config/Xresources-$HOSTNAME"
[[ -e $Xres_local ]] && xrdb -merge $Xres_local

# set X cursor
xsetroot -cursor_name left_ptr

# Speed up key repeating
xset r rate 250

xset +fp /usr/share/fonts/local/
xset fp rehash


wm_bin="$1"
wm_bin="xmonad"

if [[ xmonad == ${wm_bin} ]]
then
    trayer --transparent true --alpha 0 --tint 0xfdf6e3 --edge top \
        --align right --heighttype pixel --height 22 \
        --widthtype percent --width 5 &
    exec dbus-launch --sh-syntax --exit-with-session xmonad
elif [[ xfce4 == ${wm_bin} ]]
then
    exec startxfce4
fi

