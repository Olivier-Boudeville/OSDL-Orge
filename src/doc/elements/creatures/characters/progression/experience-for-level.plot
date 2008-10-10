set title "Experience needed to level up"
set terminal png
set grid
set autoscale
set style fill solid 1.0
set xtics 0,2,50
set mxtics 1
set ytics 10000
set output "experience-for-level.png"
set xlabel "Target level"
set ylabel "Experience points needed to reach a given level"
set key box left
set style fill solid border -1 
plot "experience-for-level.dat" using 1:2 title 'Experience points needed' with boxes lt rgb "blue" 
