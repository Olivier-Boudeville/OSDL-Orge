set title "Determining the modifier based on the new evaluated probability"
set terminal png transparent
set style line 1 lt 1 lw 3
set style line 2 lt 2 lw 1
set style line 3 lt 3 lw 3
set grid
set output "modifier-abacus.png"
set xlabel "New probability evaluated by the Game Master\nby comparison to a nominal balanced situation (in %)"
set ylabel "Corresponding deduced modifier for later use in combinations"
set key box left
plot "modifier-abacus.dat" using 1:2 title 'Determined modifier' with lines lt rgb "blue" 
