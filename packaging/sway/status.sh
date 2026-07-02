#!/bin/sh
# Sway status bar for VR development hosts.
# Shows date/time and HMD connector status.
while :; do
    resolver="/usr/libexec/exwm-vr/hmd-connector"
    [ -x "$resolver" ] || resolver="exwm-vr-hmd-connector"
    connector="$($resolver 2>/dev/null || true)"
    if [ -n "$connector" ]; then
        path=""
        for candidate in /sys/class/drm/card*-"$connector"; do
            [ -e "$candidate" ] || continue
            path="$candidate"
            break
        done
        hmd=$(cat "$path/status" 2>/dev/null || echo "unknown")
        echo "$(date +'%Y-%m-%d %H:%M') | HMD:${connector}:${hmd}"
    else
        echo "$(date +'%Y-%m-%d %H:%M') | HMD:N/A"
    fi
    sleep 5
done
