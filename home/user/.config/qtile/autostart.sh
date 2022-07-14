#!/usr/bin/env bash 

~/.xmonad/startsound.sh &
~/.xmonad/wall_change.sh &
udiskie &
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
xinput set-prop 'DLL09D9:00 04F3:3147 Touchpad' 'libinput Tapping Enabled' 1 &
xinput set-prop 'DLL09D9:00 04F3:3147 Touchpad' 'libinput Natural Scrolling Enabled' 1 &
numlockx & 
pamixer --set-volume 45 &
picom &
/usr/bin/emacs --daemon &
mpd &
mpc &
mpv &
nm-applet &
conky -c $HOME/.config/conky/qtile/doom-one-01.conkyrc
### UNCOMMENT ONLY ONE OF THE FOLLOWING THREE OPTIONS! ###
# 1. Uncomment to restore last saved wallpaper
# xargs xwallpaper --stretch < ~/.cache/wall &
# 2. Uncomment to set a random wallpaper on login
# find /usr/share/backgrounds/dtos-backgrounds/ -type f | shuf -n 1 | xargs xwallpaper --stretch &
# 3. Uncomment to set wallpaper with nitrogen
# nitrogen --restore &
