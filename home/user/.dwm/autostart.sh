#!/bin/bash
#
#
xinput set-prop "DLL09D9:00 04F3:3147 Touchpad" "libinput Tapping Enabled" 1
xinput set-prop "DLL09D9:00 04F3:3147 Touchpad" "libinput Natural Scrolling Enabled" 1
/usr/lib/polkit-kde-authentication-agent-1 &
/usr/bin/emacs --daemon &
numlockx &
# nm-applet &
dunst &
clipmenud &
# walogram &
# rsibreak &
# flameshot &
# blueman-applet &
xcompmgr &
mpd &
slstatus &
/home/ram/.dwm/newlook &
# /home/ram/.dwm/scripts/whatsapp_start &
