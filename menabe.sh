#!/bin/bash

# # sizes
# $h_small=400
# $w_small=600
# $h_big=800
# $w_big=1200

# arachide
dir="figs/photos/arachide/selected"
f1="537_Menabe.jpg"
f2="334_Menabe.jpg"
f3="610_Menabe.jpg"
convert "$dir/$f1" -resize 600x400^ -gravity center -extent 600x400 "$dir/f1.jpg"
convert "$dir/$f2" -resize 600x400^ -gravity center -extent 600x400 "$dir/f2.jpg"
convert "$dir/$f3" -resize 1200x800^ -gravity center -extent 1200x800 "$dir/f3.jpg"
montage -tile 2x1 -geometry +0+0 "$dir/f1.jpg" "$dir/f2.jpg" "$dir/m1.jpg"
montage -tile 1x2 -geometry +0+0 "$dir/m1.jpg" "$dir/f3.jpg" "$dir/m2.jpg"
convert -gravity NorthWest -annotate +10+10 "a)" -pointsize 48 "$dir/m2.jpg" "$dir/a1.jpg"
convert -gravity NorthWest -annotate +610+10 "b)" -pointsize 48 "$dir/a1.jpg" "$dir/a2.jpg"
convert -gravity NorthWest -annotate +10+410 "c)" -pointsize 48 "$dir/a2.jpg" "$dir/a3.jpg"
mv "$dir/a3.jpg" "figs/arachide.jpg"
rm "$dir/f1.jpg" "$dir/f2.jpg" "$dir/f3.jpg" "$dir/m1.jpg" "$dir/m2.jpg" "$dir/a1.jpg" "$dir/a2.jpg"

# mais
dir="figs/photos/mais/selected"
f1="177_Menabe.jpg"
f2="148_Menabe.jpg"
f3="311_Menabe.jpg"
convert "$dir/$f1" -resize 600x400^ -gravity center -extent 600x400 "$dir/f1.jpg"
convert "$dir/$f2" -resize 600x400^ -gravity center -extent 600x400 "$dir/f2.jpg"
convert "$dir/$f3" -resize 1200x800^ -gravity center -extent 1200x800 "$dir/f3.jpg"
montage -tile 2x1 -geometry +0+0 "$dir/f1.jpg" "$dir/f2.jpg" "$dir/m1.jpg"
montage -tile 1x2 -geometry +0+0 "$dir/m1.jpg" "$dir/f3.jpg" "$dir/m2.jpg"
convert -gravity NorthWest -annotate +10+10 "a)" -pointsize 48 "$dir/m2.jpg" "$dir/a1.jpg"
convert -gravity NorthWest -annotate +610+10 "b)" -pointsize 48 "$dir/a1.jpg" "$dir/a2.jpg"
convert -gravity NorthWest -annotate +10+410 "c)" -pointsize 48 "$dir/a2.jpg" "$dir/a3.jpg"
mv "$dir/a3.jpg" "figs/mais.jpg"
rm "$dir/f1.jpg" "$dir/f2.jpg" "$dir/f3.jpg" "$dir/m1.jpg" "$dir/m2.jpg" "$dir/a1.jpg" "$dir/a2.jpg"

# biodiversity
dir="figs/photos/biodiversity/selected"
mkdir "$dir/dir_mogr"
mogrify -path "$dir/dir_mogr" -resize 450x300^ -gravity center -extent 450x300 "$dir/*.jpg"
montage "$dir/dir_mogr/*.jpg" -tile 4x4 -geometry +0+0 "figs/biodiversity.jpg"

