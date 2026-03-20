#!/bin/bash

set -e
cd "$(dirname "$0")"

benches=("q2.lisp" "fibonacci_tail.lisp" "dec_cnt3.lisp" "dec_cnt1.lisp"
         "fibonacci.lisp" "tak.lisp" "dec_cnt2.lisp" "insertionsort.lisp"
         "tail_call_200k.lisp" "loop_200k.lisp" "sort500.lisp" "env_lookup.lisp")


echo "=== LispBM Performance Animation ==="

bench_data=$(mktemp)
trap "rm -f '$bench_data'" EXIT
echo "Created temporary $bench_data file"

echo "Extracting benchmark data..."

frame=0
for file in $(ls -1 stored_results/benchresult* | sort); do
    frame=$((frame + 1))
    date=$(basename "$file" | sed 's/benchresult//' | cut -d'_' -f1-3)

    for bench in "${benches[@]}"; do
        eval_time=$(awk -F',' -v bench="$bench" '
                NR==1 {
                    for(i=1; i<=NF; i++) {
                        gsub(/^[ \t]+|[ \t]+$/, "", $i);
                        if ($i ~ /Eval/) eval_col=i;
                        if ($i ~ /File/) file_col=i;
                    }
                }
                NR>1 {
                    gsub(/^[ \t]+|[ \t]+$/, "", $file_col);
                    if ($file_col == bench) {
                        gsub(/^[ \t]+|[ \t]+$/, "", $eval_col);
                        print $eval_col;
                        exit;
                    }
                }
            ' "$file")

        [ -n "$eval_time" ] && echo "$frame $date $bench $eval_time" >> "$bench_data"
    done
done
echo "Extracted $(wc -l <  "$bench_data") data points"

total=$(cut -d' ' -f1 "$bench_data" | sort -n | tail -1)
echo "Generating $total frames..."

mkdir -p frames
rm -f frames/*.png

# Prepare data files for each benchmark

for bench in "${benches[@]}"; do
    grep " $bench " "$bench_data" | awk '{print $1, $4}' > "frames/${bench}.dat"
done

# Generate frames
for frame_num in $(seq 1 $total); do
    [ $((frame_num % 10)) -eq 0 ] && echo "  Frame $frame_num/$total"

    # Extract date for this frame (format: YY_MM_DD)
    date_raw=$(awk -v f=$frame_num '$1 == f {print $2; exit}' "$bench_data")
    # Convert YY_MM_DD to readable format (e.g., "2022-01-22")
    date_display="20${date_raw//_/-}"

    # Create gnuplot script
    cat > /tmp/anim.gp <<EOF
set terminal pngcairo size 1600,900 font 'Arial,12'
set output 'frames/frame_$(printf "%03d" $frame_num).png'

set title "LispBM Performance History ($date_display)" font 'Arial,16'
set xlabel "Benchmark Snapshot" font 'Arial,12'
set ylabel "Eval Time (seconds)" font 'Arial,12'
set grid
set key outside right top

set style data linespoints
set pointsize 0.7
set xrange [0:$total]
set yrange [0:6]

plot \\
EOF

    idx=0
    for bench in "${benches[@]}"; do
        idx=$((idx + 1))
        comma=","
        [ $idx -eq ${#benches[@]} ] && comma=""

        # Escape underscores for gnuplot enhanced mode (replace _ with \_)
        bench_label="${bench//_/\\_}"

        # Filter data to only show points where frame number <= current frame
        echo "  'frames/${bench}.dat' using (\$1<=$frame_num?\$1:1/0):(\$1<=$frame_num?\$2:1/0) with linespoints lw 2 title '$bench_label'$comma \\" >> /tmp/anim.gp
    done

    gnuplot /tmp/anim.gp 2>/dev/null
done

echo "Creating video..."
ffmpeg -y -framerate 10 -i 'frames/frame_%03d.png' \
    -c:v libx264 -pix_fmt yuv420p -crf 18 \
    lbm_performance_history.mp4 >/dev/null 2>&1

echo ""
echo "=== Done! ==="
echo "Video: lbm_performance_history.mp4"
ls -lh lbm_performance_history.mp4
