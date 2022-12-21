# arguments: name outfile csvfile
# usage: gnuplot -e "name='the title'" -e "outfile='out.pdf'" -e "csvfile='data.csv'" bars.gp
set terminal pdf
set output outfile
set title name
unset key
set style data histogram
set datafile separator ","
set boxwidth 2
set xtic rotate by -20 scale 0
set grid y
set ylabel "time (s)"
set style fill solid
plot csvfile using "Mean":"Color":xtic(2) title col lc variable
