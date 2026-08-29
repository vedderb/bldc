#!/usr/bin/env python3
#
# Usage:
#   python3 plot_bench.py                       # every format x size slice -> plots/
#   python3 plot_bench.py --format indexed4 --size large   # just one slice
#   python3 plot_bench.py --list                # show available formats/sizes/shapes

import argparse
import re
from glob import glob
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

HERE = Path(__file__).parent
RESULTS_DIR = HERE / "results"
PLOTS_DIR = HERE / "plots"

FILENAME_RE = re.compile(
    r"bench_(\d{4}-\d{2}-\d{2})_(\d{2}-\d{2}-\d{2})_([\d.]+)\.csv$"
)


def parse_run(path):
    m = FILENAME_RE.search(Path(path).name)
    if not m:
        return None
    date_str, time_str, version = m.groups()
    ts = pd.Timestamp(f"{date_str} {time_str.replace('-', ':')}")
    return ts, version


def load_runs():
    runs = []
    for path in sorted(glob(str(RESULTS_DIR / "*.csv"))):
        parsed = parse_run(path)
        if parsed is None:
            print(f"skipping (unrecognized filename): {path}")
            continue
        ts, version = parsed
        df = pd.read_csv(path)
        runs.append((ts, version, df))
    return runs


def plot_slice(runs, fmt, size, out_dir):
    slice_frames = []
    for ts, version, df in runs:
        sub = df[(df["format"] == fmt) & (df["param"] == size)]
        if sub.empty:
            continue
        s = sub.set_index("shape")["us_per_call"]
        s.name = ts
        slice_frames.append(s)

    if not slice_frames:
        print(f"no data for format={fmt} size={size}")
        return None

    table = pd.concat(slice_frames, axis=1).sort_index(axis=1).T
    shapes = list(table.columns)

    plt.figure(figsize=(10.0, 5.0))
    cmap = plt.get_cmap("jet")
    colors = cmap(np.linspace(0, 1.0, len(shapes)))
    # Cycle marker + linestyle independently of color -- with ~29 shapes,
    # nearby jet-colormap hues can look near-identical, especially at
    # small thumbnail size or in grayscale printouts. len(MARKERS) *
    # len(LINESTYLES) = 36 combinations, more than enough to keep every
    # shape's line distinguishable by shape/dash alone, not just color.
    MARKERS = ["o", "s", "^", "D", "v", "P", "X", "*", "h"]
    LINESTYLES = ["-", "--", "-.", ":"]
    for i, (shape, color) in enumerate(zip(shapes, colors)):
        marker = MARKERS[i % len(MARKERS)]
        linestyle = LINESTYLES[(i // len(MARKERS)) % len(LINESTYLES)]
        plt.plot(table.index, table[shape], label=shape, color=color,
                  marker=marker, markersize=4, linestyle=linestyle, linewidth=1.2)

    lgd = plt.legend(loc="center left", bbox_to_anchor=(1, 0.5), fontsize=6)
    ax = plt.gca()
    for tick in ax.get_xticklabels():
        tick.set_rotation(90)
    ax.tick_params(axis="both", which="major", labelsize=6)
    ax.tick_params(axis="both", which="minor", labelsize=4)
    ax.set_facecolor("lightgray")
    plt.ylabel("Time per call (us)")
    plt.title(f"display_extensions benchmarks format={fmt} size={size}")
    plt.grid()

    out_dir.mkdir(parents=True, exist_ok=True)
    base = f"bench_{fmt}_{size}"
    plt.savefig(out_dir / f"{base}.png", dpi=600, bbox_extra_artists=(lgd,), bbox_inches="tight")
    plt.yscale("log")
    plt.savefig(out_dir / f"{base}_log.png", dpi=600, bbox_extra_artists=(lgd,), bbox_inches="tight")
    plt.close()
    return base


def write_index_html(out_dir, formats, sizes, generated):
    # generated: set of (format, size) base names that actually got a
    # plot (some combos may be missing if a run predates a shape/format).
    def js_list(xs):
        return "[" + ",".join(f'"{x}"' for x in xs) + "]"

    html = f"""<!doctype html>
<html>
<head>
<meta charset="utf-8">
<title>display_extensions benchmark plots</title>
<style>
  body {{ font-family: sans-serif; margin: 24px; background: #1a1a1a; color: #eee; }}
  .controls {{ display: flex; gap: 16px; align-items: center; margin-bottom: 16px; flex-wrap: wrap; }}
  label {{ font-size: 14px; color: #aaa; }}
  select, button {{ font-size: 14px; padding: 4px 8px; }}
  button.active {{ background: #3388cc; color: white; }}
  img {{ max-width: 100%; border: 1px solid #444; background: #ddd; }}
  #missing {{ color: #e2596b; margin-top: 8px; }}
</style>
</head>
<body>
<h1>display_extensions benchmark plots</h1>
<div class="controls">
  <label>Format:
    <select id="fmt"></select>
  </label>
  <label>Size:
    <select id="size"></select>
  </label>
  <button id="scale-btn">Scale: log</button>
</div>
<div id="missing"></div>
<img id="plot" src="">

<script>
const formats = {js_list(formats)};
const sizes = {js_list(sizes)};
const generated = new Set({js_list([f"bench_{f}_{s}" for f, s in generated])});
let logScale = true;

const fmtSel = document.getElementById('fmt');
const sizeSel = document.getElementById('size');
const scaleBtn = document.getElementById('scale-btn');
const img = document.getElementById('plot');
const missing = document.getElementById('missing');

for (const f of formats) {{
  const o = document.createElement('option');
  o.value = f; o.textContent = f;
  fmtSel.appendChild(o);
}}
for (const s of sizes) {{
  const o = document.createElement('option');
  o.value = s; o.textContent = s;
  sizeSel.appendChild(o);
}}

function update() {{
  const f = fmtSel.value, s = sizeSel.value;
  const base = `bench_${{f}}_${{s}}`;
  scaleBtn.textContent = 'Scale: ' + (logScale ? 'log' : 'linear');
  if (generated.has(base)) {{
    missing.textContent = '';
    img.src = base + (logScale ? '_log' : '') + '.png';
    img.style.display = '';
  }} else {{
    missing.textContent = `No data for format=${{f}} size=${{s}}`;
    img.style.display = 'none';
  }}
}}

fmtSel.onchange = update;
sizeSel.onchange = update;
scaleBtn.onclick = () => {{ logScale = !logScale; update(); }};
update();
</script>
</body>
</html>
"""
    (out_dir / "index.html").write_text(html)


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--format", default=None, help="restrict to one color format (default: all)")
    ap.add_argument("--size", default=None, help="restrict to one size, small/medium/large (default: all)")
    ap.add_argument("--list", action="store_true", help="list available formats/sizes/shapes and exit")
    ap.add_argument("--out-dir", default=str(PLOTS_DIR), help="output directory (default: plots/)")
    args = ap.parse_args()

    runs = load_runs()
    if not runs:
        print(f"no result files found in {RESULTS_DIR}/  run ./run.sh first")
        return

    all_formats = sorted(runs[-1][2]["format"].unique())
    all_sizes = sorted(runs[-1][2]["param"].unique(), key=lambda s: ["small", "medium", "large"].index(s))

    if args.list:
        print("formats:", all_formats)
        print("sizes:  ", all_sizes)
        print("shapes: ", sorted(runs[-1][2]["shape"].unique()))
        return

    formats = [args.format] if args.format else all_formats
    sizes = [args.size] if args.size else all_sizes
    out_dir = Path(args.out_dir)

    generated = []
    for fmt in formats:
        for size in sizes:
            base = plot_slice(runs, fmt, size, out_dir)
            if base:
                generated.append((fmt, size))
                print(f"wrote {base}.png / {base}_log.png")

    if not generated:
        print("nothing generated, try --list to check format/size names")
        return

    write_index_html(out_dir, all_formats, all_sizes, generated)
    print(f"\n{len(generated)} slice(s) written to {out_dir}/")
    print(f"open {out_dir}/index.html in a browser to view and switch between them")


if __name__ == "__main__":
    main()
