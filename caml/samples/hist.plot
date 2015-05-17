clear
reset
unset key
# Make the x axis labels easier to read.
set xtics rotate out
# Select histogram data
set style data histogram
# Give the bars a plain fill pattern, and draw a solid line around them.
set style fill solid border
set style histogram clustered
set term pngcairo
set output "small.png"
plot for [COL=2:5] 'small.rez' using COL:xticlabels(1) title columnheader
