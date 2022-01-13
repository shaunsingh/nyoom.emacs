#!/usr/bin/env bash

exec >~/.logs/xsession 2>&1
export LANG="en_GB.UTF-8"
export LANGUAGE="en_GB.UTF-8"
export DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/$(id -u)/bus
xset -dpms
xset s off
xhost +SI:localuser:$USER
emacs -mm --with-exwm --debug-init
