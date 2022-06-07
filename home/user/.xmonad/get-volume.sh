#!/bin/bash
# ~/bin/get-volume.sh
# Get the maximum volume of any pulseaudio sink channel
# amixer get Master | egrep -o "[0-9]+%"
# vol=$(amixer get Master | awk -F'[]%[]' '/%/ {if ($7 == "off") { print "MM" } else { print $2 }}' | head -n 1)

vol=$(amixer get Master | tr -d '[]%' | awk 'END{print}' | awk '{ print $6 }')

vol1=$(amixer get Master | tr -d '[]%' | awk 'END{print}' | awk '{ print $5 }')

if [ "$vol" = "off" ]
then 
    echo -e Vol: "MM"
else 
    echo -e Vol: "$vol1"%
fi

exit 0
\xf028
