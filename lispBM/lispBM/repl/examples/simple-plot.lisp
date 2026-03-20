
(define gp (gnuplot-open))

(gnuplot-data gp "data" '(10.5 12.3 8.7 15.2))

(gnuplot-cmd gp "plot '$data' with linespoints title \"numbers\"")
