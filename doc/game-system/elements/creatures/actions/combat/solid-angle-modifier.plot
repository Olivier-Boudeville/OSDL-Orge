set title "Impact of the solid angle on\nto-hit modifiers for ranged attacks"
set terminal png
set grid
set output "solid-angle-modifier.png"
set xlabel "Solid Angle"
set ylabel "Modifier (in %):\nto-hit modifier for ranged attacks"
set key rmargin vertical  box
plot "solid-angle-modifier.dat" using 1:2 with lines title 'To-Hit Modifier'
