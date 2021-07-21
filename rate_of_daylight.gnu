set terminal png size 350,300 font "arial,9"
set output 'rod_plots.png'

set multiplot
set style line 1
set nokey
set lmargin at screen 0.15

xPos = 171-1
set arrow 1 at xPos, graph 0 to xPos, graph 1 nohead lc "red" dt 2 lw 2
set label 1 at xPos, graph 1 "Summer\nSolstice" offset -7.5,-3.0 tc "red"

xPos = 355-1
set arrow 2 at xPos, graph 0 to xPos, graph 1 nohead lc "red" dt 2 lw 2
set label 2 at xPos, graph 1 "Winter\nSolstice" offset -7.5,-3.0 tc "red"

xPos = 79-1
set arrow 3 at xPos, graph 0 to xPos, graph 1 nohead lc "blue" dt 2 lw 2
set label 3 at xPos, graph 1 "Vernal\nEquinox" offset -7.5,-3.0 tc "blue"

xPos = 265-1
set arrow 4 at xPos, graph 0 to xPos, graph 1 nohead lc "blue" dt 2 lw 2
set label 4 at xPos, graph 1 "Autumnal\nEquinox" offset -7.5,-3.0 tc "blue"

set size 1, 0.5
set origin 0, 0
set xlabel "Day"
set ylabel "Daylength [hr]"
set xrange [0:365]
set yrange [8:16]

plot "delta_times.txt" with lines linetype 1 lw 3 lc rgb "#808080"

set size 1, 0.5
set origin 0, 0.5
set xlabel "Day"
set ylabel "Rate of Daylength Change [min]"
set xrange [0:365]
set yrange [-3:3]
plot "derivatives.txt" with lines linetype 2 lw 3 lc rgb "#808080"

unset multiplot
