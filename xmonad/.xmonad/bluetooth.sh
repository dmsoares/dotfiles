#!/usr/bin/env bash

device=$(bluetoothctl info | rg '^\W+Name: (.*)$' -r '$1')

if [ "$device" != "" ]; then
    echo -n "$device"
else
    echo -n " - "
fi
