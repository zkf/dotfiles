#!/bin/bash
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)
#


# Redirect stdout/stderr to log file
exec > /var/log/xinitrc.log 2>&1

## Do host-specific things
case $(hostname) in
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

    lestrade)
              systemctl --user start xorg.target
              firefox &
              udiskie &
              dunst &
              # setxkbmap -rules colemak -model pc105awide-sl -layout \
                  # 'nocolemak(cmk_ed_us),brcolemak(cmk_ed_us)' \
                  # -option '' -option misc:extend -option lv5:caps_switch_lock \
                  # -option grp:menu_toggle -v 10
              xbindkeys
              xsetroot -solid black
              redshift -l 3.688855:11.674803 -t 6500:3000 &
              compton --backend glx --paint-on-overlay --glx-no-stencil \
                  --vsync opengl-swc --config ~/config/compton.conf -b &
              syndaemon -i 1.0 -t -k -d
              ;;
esac >&/dev/null # we don't want the output from those programs to clutter up the log file.

# load X settings
xrdb -load ~/.Xresources
Xres_local="$HOME/config/Xresources-$HOSTNAME"
[[ -e $Xres_local ]] && xrdb -merge $Xres_local

# set X cursor
xsetroot -cursor_name left_ptr

# Speed up key repeating
xset r rate 250

# Set screen to dim after 5min, timeout/lock 1m later (with xss-lock)
xset s 300 60

xset +fp /usr/share/fonts/local/
xset fp rehash

synclient PalmDetect=1 PalmMinZ=40 PalmMinWidth=4

rm ~/.volpipe && mkfifo ~/.volpipe
echo "--" >> ~/.volpipe &

tmux new-session -d -s'com' irssi
tmux new-session -d -s'work' -c ~/projects/work/tingtun/

exec $HOME/.cabal/bin/xmonad

