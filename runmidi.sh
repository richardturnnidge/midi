#!/bin/bash


cd /Users/richardturnnidge/Programming/agon/midi

echo "Starting midi bridge"


python3 serialmidi.py --serial_name=/dev/tty.usbserial-0001 --midi_in_name="Keystation Port 1" --midi_out_name="Keystation Port 1" --debug 
