#!/usr/bin/env python3
"""Parse and plot a VESC blackbox dump captured over RTT.

Accepts the raw capture from bb_capture.py or a J-Link RTT Viewer log;
everything outside the #BB_DUMP_BEGIN / #BB_DUMP_END markers is ignored.
If the file contains several dumps the last complete one is used.

Requires: pip install matplotlib

Usage:
  python bb_plot.py dump.log                 # show plots
  python bb_plot.py dump.log --png out.png   # also save a PNG
  python bb_plot.py dump.log --csv out.csv   # also save the clean CSV
"""

import argparse
import math
import re
import sys

COLUMNS = ["tick", "ia", "ib", "ic", "id", "iq", "i_abs", "i_abs_filter",
           "duty", "v_bus", "phase", "speed_rad_s", "fault", "state", "mode", "flags"]

FAULT_NAMES = {
    0: "NONE", 1: "OVER_VOLTAGE", 2: "UNDER_VOLTAGE", 3: "DRV",
    4: "ABS_OVER_CURRENT", 5: "OVER_TEMP_FET", 6: "OVER_TEMP_MOTOR",
}


def parse_dump(path):
    with open(path, "rb") as f:
        text = f.read().decode(errors="replace")

    # Take the last complete BEGIN...END block.
    blocks = re.findall(r"#BB_DUMP_BEGIN,([^\r\n]*)\r?\n(.*?)#BB_DUMP_END",
                        text, re.DOTALL)
    if not blocks:
        sys.exit("No complete #BB_DUMP_BEGIN/#BB_DUMP_END block found in " + path)
    meta_str, body = blocks[-1]

    meta = {}
    for kv in meta_str.split(","):
        if "=" in kv:
            k, v = kv.split("=", 1)
            meta[k.strip()] = v.strip()

    rows = []
    for line in body.splitlines():
        line = line.strip()
        if not line or line.startswith("#") or line.startswith("tick,"):
            continue
        parts = line.split(",")
        if len(parts) != len(COLUMNS):
            continue  # torn/interleaved line, skip
        try:
            rows.append([float(x) for x in parts])
        except ValueError:
            continue

    if not rows:
        sys.exit("Dump block found but contained no valid data rows.")

    data = {c: [r[i] for r in rows] for i, c in enumerate(COLUMNS)}
    return meta, data


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("logfile", help="Captured RTT log file")
    parser.add_argument("--png", help="Save figure to this PNG file")
    parser.add_argument("--csv", help="Save the clean CSV to this file")
    parser.add_argument("--no-show", action="store_true", help="Do not open a plot window")
    args = parser.parse_args()

    meta, d = parse_dump(args.logfile)

    f_isr = float(meta.get("f_isr_hz", 0)) or None
    decimation = int(meta.get("decimation", 1))
    fault = int(meta.get("fault", 0))

    tick0 = d["tick"][0]
    if f_isr:
        t = [(tk - tick0) / f_isr * 1000.0 for tk in d["tick"]]  # ms
        x_label = "t (ms)"
    else:
        t = [tk - tick0 for tk in d["tick"]]
        x_label = "ISR ticks"

    n = len(t)
    print(f"Records: {n}, f_isr: {f_isr} Hz, decimation: {decimation}, "
          f"fault: {fault} ({FAULT_NAMES.get(fault, '?')}), frozen: {meta.get('frozen')}")

    if args.csv:
        with open(args.csv, "w") as f:
            f.write("t_ms," + ",".join(COLUMNS) + "\n")
            for i in range(n):
                f.write(f"{t[i]:.4f}," + ",".join(str(d[c][i]) for c in COLUMNS) + "\n")
        print("CSV written to", args.csv)

    try:
        import matplotlib.pyplot as plt
    except ImportError:
        sys.exit("matplotlib not installed. Run: pip install matplotlib")

    fig, axes = plt.subplots(6, 1, sharex=True, figsize=(12, 14))
    fig.suptitle(f"VESC blackbox dump - fault {fault} ({FAULT_NAMES.get(fault, '?')})")

    # Mark where the fault flag first became active.
    t_fault = None
    for i in range(n):
        if int(d["flags"][i]) & 0x01:
            t_fault = t[i]
            break

    ax = axes[0]
    ax.plot(t, d["i_abs"], label="i_abs (raw)")
    ax.plot(t, d["i_abs_filter"], label="i_abs_filter")
    ax.set_ylabel("A")
    ax.legend(loc="upper left")
    ax.set_title("ABS current (fault comparison variable)")

    ax = axes[1]
    ax.plot(t, d["ia"], label="ia")
    ax.plot(t, d["ib"], label="ib")
    ax.plot(t, d["ic"], label="ic")
    ax.set_ylabel("A")
    ax.legend(loc="upper left")
    ax.set_title("Phase currents")

    ax = axes[2]
    ax.plot(t, d["id"], label="id")
    ax.plot(t, d["iq"], label="iq")
    ax.set_ylabel("A")
    ax.legend(loc="upper left")
    ax.set_title("dq currents")

    ax = axes[3]
    ax.plot(t, d["phase"], label="phase (rad)")
    erpm = [w * 60.0 / (2.0 * math.pi) for w in d["speed_rad_s"]]
    ax2 = ax.twinx()
    ax2.plot(t, erpm, color="tab:orange", alpha=0.6, label="ERPM")
    ax.set_ylabel("rad")
    ax2.set_ylabel("ERPM")
    ax.legend(loc="upper left")
    ax2.legend(loc="upper right")
    ax.set_title("Observer phase / speed")

    ax = axes[4]
    ax.plot(t, d["v_bus"], label="v_bus")
    ax2 = ax.twinx()
    ax2.plot(t, d["duty"], color="tab:green", alpha=0.6, label="duty")
    ax.set_ylabel("V")
    ax2.set_ylabel("duty")
    ax.legend(loc="upper left")
    ax2.legend(loc="upper right")
    ax.set_title("Bus voltage / duty")

    ax = axes[5]
    ax.step(t, d["fault"], where="post", label="fault_code")
    ax.step(t, d["flags"], where="post", alpha=0.6, label="flags")
    ax.set_ylabel("code")
    ax.set_xlabel(x_label)
    ax.legend(loc="upper left")
    ax.set_title("Fault code / flags")

    if t_fault is not None:
        for ax in axes:
            ax.axvline(t_fault, color="red", linestyle="--", alpha=0.7)
        axes[0].annotate("fault", xy=(t_fault, axes[0].get_ylim()[1]),
                         color="red", ha="left", va="top")

    fig.tight_layout(rect=(0, 0, 1, 0.97))

    if args.png:
        fig.savefig(args.png, dpi=150)
        print("Figure written to", args.png)

    if not args.no_show:
        plt.show()


if __name__ == "__main__":
    main()
