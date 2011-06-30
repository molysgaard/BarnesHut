#!/bin/bash

shopt -s nullglob

echo "Compiling"
ghc --make Main.hs

echo "Running"
./Main

#convert to PNG
SVGS=*.svg

for f in *.svg
do
  echo "Converting $f"
  convert $f ${f%%.svg}.bmp
done

echo "Encoding to orbit.mp4"
ffmpeg -y -qscale 5 -r 20 -b 9600 -i frame%04d.bmp orbit.mp4

#remove old files
echo "Removing old frames"
rm /home/lysgaard/programming/haskell/BarnesHut/frame*

totem orbit.mp4
