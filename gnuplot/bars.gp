# arguments: name outfile csvfile
# usage: gnuplot -e "name='the title'" -e "outfile='out'" -e "csvfile='data.csv'" bars.gp

meanCol = 'Mean'
colorCol = 'Color'
stddevCol = 'Stddev'
targets = meanCol . ' ' . stddevCol
set terminal svg enhanced mouse size 800, 1100
set output outfile.'.svg'
set multiplot layout 2, 1 title ('suite '.name) font ",24"
set key outside
set tmargin 3
set style data histogram
set datafile separator ","
set boxwidth 2
set xtic rotate by -20 scale 0 font ",16"
set ytic scale 0 font ",16"
set grid y
set ylabel "execution time (s)" font ", 20"
set style fill solid
unset key
set yrange [0 : *]
set offsets graph 0,0.5

set title meanCol font ",20"
plot csvfile \
        using meanCol:colorCol:xtic(2) notitle linecolor rgbcolor variable, \
     '' using 0:meanCol:(sprintf("%1.4f",column(meanCol))) with labels font ",13" center offset 0, 0.4 title meanCol, \


set title "Standard deviation" font ",20"
unset ylabel

f(x) = column(stddevCol)*100/column(meanCol)
plot csvfile \
        using (f('')):colorCol:xtic(2) notitle linecolor rgbcolor variable, \
     '' using ($0 - 1):(f('')):(sprintf("%1.2f%",(f('')))) with labels font ",13" center offset 0, 0.4 notitle

unset multiplot
