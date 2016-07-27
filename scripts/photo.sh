#!/bin/bash

## Bash script with ImageMagick commands to modify and combine photos for illustration
## Ghislain Vieilledent <ghislain.vieilledent@cirad.fr>

##=============
## Biodiversity
dir="photos/biodiversity/selected"
mkdir -p "$dir/dir_mogr"
mogrify -path "$dir/dir_mogr" -resize 450x300^ -gravity center -extent 450x300 "$dir/*.jpg"
montage "$dir/dir_mogr/*.jpg" -tile 4x4 -geometry +0+0 "figs/biodiversity.jpg"

##========================
## Causes of deforestation

## Text size
init_pointsize=48
init_figsize=800
ts=$(($init_pointsize*600/$init_figsize))

## Slash-and-burn agriculture: arachide
dir="photos/arachide/selected"
fsmall="537_Menabe.jpg"
fbig="610_Menabe.jpg"
convert "$dir/$fsmall" -resize 300x200^ -gravity center -extent 300x200 "$dir/fsmall.jpg"
convert "$dir/$fbig" -resize 600x400^ -gravity center -extent 600x400 "$dir/fbig.jpg"

## Slash-and-burn agriculture: mais
dir="photos/mais/selected"
fsmall="177_Menabe.jpg"
fbig="311_Menabe.jpg"
convert "$dir/$fsmall" -resize 300x200^ -gravity center -extent 300x200 "$dir/fsmall.jpg"
convert "$dir/$fbig" -resize 600x400^ -gravity center -extent 600x400 "$dir/fbig.jpg"

## Cyclone, grasslands and uncontrolled fires
dir="photos/bosake_cyclone/selected"
fsmall="52_Menabe.jpg"
fbig="491_Menabe.jpg"
convert "$dir/$fsmall" -resize 300x200^ -gravity center -extent 300x200 "$dir/fsmall.jpg"
convert "$dir/$fbig" -resize 600x400^ -gravity center -extent 600x400 "$dir/fbig.jpg"

## Illegal logging
dir="photos/illegal_logging/selected"
fsmall="37_Menabe.jpg"
fbig="337_Menabe.jpg"
convert "$dir/$fsmall" -resize 300x200^ -gravity center -extent 300x200 "$dir/fsmall.jpg"
convert "$dir/$fbig" -resize 600x400^ -gravity center -extent 600x400 "$dir/fbig.jpg"

## Montage
# small
fsmall_1="photos/arachide/selected/fsmall.jpg"
fsmall_2="photos/mais/selected/fsmall.jpg"
fsmall_3="photos/bosake_cyclone/selected/fsmall.jpg"
fsmall_4="photos/illegal_logging/selected/fsmall.jpg"
# big
fbig_1="photos/arachide/selected/fbig.jpg"
fbig_2="photos/mais/selected/fbig.jpg"
fbig_3="photos/bosake_cyclone/selected/fbig.jpg"
fbig_4="photos/illegal_logging/selected/fbig.jpg"
# montage small
montage -tile 4x1 -geometry +0+0 "$fsmall_1" "$fsmall_2" "$fsmall_3" "$fsmall_4" "figs/small.jpg"
rm "$fsmall_1" "$fsmall_2" "$fsmall_3" "$fsmall_4"
# montage big
montage -tile 2x1 -geometry +0+0 "$fbig_1" "$fbig_3" "figs/big_1.jpg"
montage -tile 2x1 -geometry +0+0 "$fbig_2" "$fbig_4" "figs/big_2.jpg"
rm "$fbig_1" "$fbig_3" "$fbig_2" "$fbig_4"
# montage big + small
montage -tile 1x3 -geometry +0+0 "figs/big_1.jpg" "figs/small.jpg" "figs/big_2.jpg" "figs/m1.jpg"

## Annotate with label
# big
convert -fill white -gravity NorthWest -annotate +5+5 "a" -pointsize $ts "figs/m1.jpg" "figs/m2.jpg"
convert -fill white -gravity NorthWest -annotate +605+5 "c" -pointsize $ts "figs/m2.jpg" "figs/m3.jpg"
convert -fill white -gravity NorthWest -annotate +5+605 "b" -pointsize $ts "figs/m3.jpg" "figs/m4.jpg"
convert -fill white -gravity NorthWest -annotate +605+605 "d" -pointsize $ts "figs/m4.jpg" "figs/m5.jpg"
# small
convert -fill white -gravity NorthWest -annotate +5+405 "a'" -pointsize $(($ts-12)) "figs/m5.jpg" "figs/m6.jpg"
convert -fill white -gravity NorthWest -annotate +305+405 "b'" -pointsize $(($ts-12)) "figs/m6.jpg" "figs/m7.jpg"
convert -fill white -gravity NorthWest -annotate +605+405 "c'" -pointsize $(($ts-12)) "figs/m7.jpg" "figs/m8.jpg"
convert -fill white -gravity NorthWest -annotate +905+405 "d'" -pointsize $(($ts-12)) "figs/m8.jpg" "figs/causes.jpg"

## Cleaning
rm "figs/big_1.jpg" "figs/big_2.jpg" "figs/small.jpg" figs/m[1-8].jpg

##==================================
## Household and stakeholder surveys

## Text size
init_pointsize=48
init_figsize=800
ts=$(($init_pointsize*600/$init_figsize))

## Surveys
dir="photos/surveys/selected"
f1="410_Menabe.jpg"
f2="581_Menabe.jpg"
f3="406_Menabe.jpg"
f4="568_Menabe.jpg"
# Resize
convert "$dir/$f1" -resize 600x400^ -gravity center -extent 600x400 "$dir/f1.jpg"
convert "$dir/$f2" -resize 600x400^ -gravity center -extent 600x400 "$dir/f2.jpg"
convert "$dir/$f3" -resize 600x400^ -gravity center -extent 600x400 "$dir/f3.jpg"
convert "$dir/$f4" -resize 600x400^ -gravity center -extent 600x400 "$dir/f4.jpg"
# Montage
montage -tile 2x2 -geometry +0+0 "$dir/f1.jpg" "$dir/f3.jpg" "$dir/f2.jpg" "$dir/f4.jpg" "figs/m1.jpg"
# Annotate
convert -gravity NorthWest -annotate +5+5 "a" -pointsize $ts "figs/m1.jpg" "figs/m2.jpg"
convert -gravity NorthWest -annotate +605+5 "c" -pointsize $ts "figs/m2.jpg" "figs/m3.jpg"
convert -gravity NorthWest -annotate +5+405 "b" -pointsize $ts "figs/m3.jpg" "figs/m4.jpg"
convert -gravity NorthWest -annotate +605+405 "d" -pointsize $ts "figs/m4.jpg" "figs/surveys.jpg"
# Clean
rm "$dir/f1.jpg" "$dir/f3.jpg" "$dir/f2.jpg" "$dir/f4.jpg"
rm figs/m[1-4].jpg

