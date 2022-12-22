# arguments: name outfile csvfile
# usage: gnuplot -e "name='the title'" -e "outfile='out'" -e "csvfile='data.csv'" bars.gp

meanCol = 'Mean'
colorCol = 'Color'
stddevCol = 'Stddev'
targets = meanCol . ' ' . stddevCol

set terminal pdf size 18cm, 26cm
set output outfile.'.pdf'
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

set multiplot layout 2, 1 title ('suite '.name) font ",24" scale 1, 1
do for [target in targets] {
    set title target font ",20"
    plot csvfile \
        using target:colorCol:xtic(2) notitle column(2) lc rgbcolor variable, \
     '' using 0:target:(sprintf("%1.4f",column(target))) with labels font ",13" center offset 0, 0.4 title target, \

}

unset multiplot

set terminal svg enhanced mouse size 600, 1100
set output outfile.'.svg'
set multiplot layout 2, 1 title ('suite '.name) font ",24" scale 1, 1
do for [target in targets] {
set key outside
replot
}
unset multiplot
