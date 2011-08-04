#!/bin/sh

# Get frames
mplayer -vo jpeg:outdir=frames -ao dummy -vf framestep=30,scale=16:9 "$1"

# Create montage and remove frames if successful
echo Creating montage ...
montage -geometry +0+0 -background black -tile 60x -mode Concatenate \
    "frames/*jpg" montage.jpg \
    && rm -fr frames/
