#!/usr/bin/env bash

# Terminate already running bar instances
killall -q polybar
#
# # Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done
#
# # Launch the bar named 'gruvbox-bar' (we will create this next)
# polybar gruvbox-bar 2>&1 | tee -a /tmp/polybar.log & disown
polybar example 2>&1 | tee -a /tmp/polybar.log >/dev/null & disown

echo "Polybar launched..."
