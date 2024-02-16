#!/bin/bash
#
#
/home/ram/.dwm/newlook &
/usr/lib/polkit-kde-authentication-agent-1 &
numlockx &
nm-applet &
dunst &
clipmenud &
xinput set-prop "DLL09D9:00 04F3:3147 Touchpad" "libinput Tapping Enabled" 1
xinput set-prop "DLL09D9:00 04F3:3147 Touchpad" "libinput Natural Scrolling Enabled" 1
# blueman-applet &
xcompmgr &
slstatus &
