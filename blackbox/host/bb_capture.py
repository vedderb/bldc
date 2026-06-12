#!/usr/bin/env python3
"""Capture a VESC blackbox dump over J-Link RTT.

Connects to the target via J-Link, optionally triggers a dump by sending
'd' on RTT down channel 0, then records everything from RTT up channel 0
until the #BB_DUMP_END marker (or timeout) and writes it to a log file.

Requires: pip install pylink-square

Usage:
  python bb_capture.py --out dump.log                 # trigger + capture
  python bb_capture.py --out dump.log --no-trigger    # just listen (use the
                                                      # bb_dump terminal cmd)
  python bb_capture.py --clear                        # send 'c' (bb_clear)

The captured log can be plotted with bb_plot.py.
"""

import argparse
import sys
import time

try:
    import pylink
except ImportError:
    sys.exit("pylink not installed. Run: pip install pylink-square")

END_MARKER = b"#BB_DUMP_END"


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--device", default="STM32F407VG", help="J-Link device name")
    parser.add_argument("--speed", type=int, default=4000, help="SWD speed in kHz")
    parser.add_argument("--out", default="bb_dump.log", help="Output log file")
    parser.add_argument("--timeout", type=float, default=15.0, help="Capture timeout in seconds")
    parser.add_argument("--no-trigger", action="store_true",
                        help="Do not send 'd'; wait for a dump triggered elsewhere")
    parser.add_argument("--clear", action="store_true",
                        help="Send 'c' (blackbox clear) instead of capturing a dump")
    args = parser.parse_args()

    jlink = pylink.JLink()
    jlink.open()
    jlink.set_tif(pylink.enums.JLinkInterfaces.SWD)
    jlink.connect(args.device, speed=args.speed)
    jlink.rtt_start(None)

    # Wait for the RTT control block to be located.
    for _ in range(50):
        try:
            if jlink.rtt_get_num_up_buffers() > 0:
                break
        except pylink.errors.JLinkRTTException:
            pass
        time.sleep(0.1)
    else:
        sys.exit("RTT control block not found. Is the blackbox firmware running?")

    if args.clear:
        jlink.rtt_write(0, list(b"c"))
        time.sleep(0.3)
        data = jlink.rtt_read(0, 4096)
        if data:
            sys.stdout.write(bytes(data).decode(errors="replace"))
        print("Clear command sent.")
        jlink.close()
        return

    if not args.no_trigger:
        jlink.rtt_write(0, list(b"d"))
        print("Dump trigger sent.")

    print(f"Capturing to {args.out} (timeout {args.timeout:.0f} s)...")
    captured = bytearray()
    t_start = time.time()
    t_last_data = t_start

    while True:
        data = jlink.rtt_read(0, 4096)
        now = time.time()
        if data:
            captured.extend(bytes(data))
            t_last_data = now
            if END_MARKER in captured:
                # Drain the remaining bytes of the END line.
                time.sleep(0.2)
                tail = jlink.rtt_read(0, 4096)
                if tail:
                    captured.extend(bytes(tail))
                break
        else:
            time.sleep(0.02)

        if now - t_start > args.timeout:
            print("Warning: timeout reached before #BB_DUMP_END.", file=sys.stderr)
            break
        # Give up early if a dump started but stalled.
        if captured and now - t_last_data > 3.0:
            print("Warning: data stream stalled.", file=sys.stderr)
            break

    jlink.close()

    with open(args.out, "wb") as f:
        f.write(captured)

    n_lines = captured.count(b"\n")
    print(f"Captured {len(captured)} bytes, {n_lines} lines -> {args.out}")
    if END_MARKER in captured:
        print("Dump complete. Plot it with: python bb_plot.py " + args.out)


if __name__ == "__main__":
    main()
