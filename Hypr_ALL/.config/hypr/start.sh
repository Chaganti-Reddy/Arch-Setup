#!/bin/bash

export SSH_AUTH_SOCK
export GPG_AGENT_INFO
export GNOME_KEYRING_CONTROL
export GNOME_KEYRING_PID

~/.config/waybar/launch_waybar.sh &
~/dotfiles/hypr_wall/wallpaper_random.sh &
