#!/bin/bash
number=0
letters="a)b)c)"
for f in *.tif; do
   label=${letters:number:1}
   convert "$f" -gravity northwest -background gray90 label:"$label" -composite miff:-
   ((number=number+2))
done | montage -tile 2x - result.png