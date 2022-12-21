# arguments: name outfile csvfile
# usage: gnuplot -e "name='the title'" -e "outfile='out'" -e "csvfile='data.csv'" bars.gp
set title name
unset key
set style data histogram
set datafile separator ","
set boxwidth 2
set xtic rotate by -20 scale 0
set grid y
set ylabel "time (s)"
set style fill solid

set terminal pdf
set output outfile.'.pdf'
plot csvfile using "Mean":"Color":xtic(2) title col lc variable

set terminal svg enhanced mouse
set output outfile.'.svg'
replot
