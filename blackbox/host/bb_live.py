#!/usr/bin/env python3
"""Live VESC blackbox scope over J-Link RTT.

Requires:
  pip install pylink-square matplotlib

Usage:
  python bb_live.py --out live.csv
  python bb_live.py --speed 8000 --out live.csv
"""

import argparse
import bisect
import csv
import datetime as dt
import math
import os
import struct
import sys
import tempfile
import threading
import time
import tkinter as tk
from tkinter import filedialog, messagebox, ttk

import matplotlib as mpl

mpl.rcParams["path.simplify"] = True
mpl.rcParams["path.simplify_threshold"] = 1.0
mpl.rcParams["agg.path.chunksize"] = 10000
mpl.rcParams["axes.unicode_minus"] = False

from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg, NavigationToolbar2Tk
from matplotlib.figure import Figure
from matplotlib.widgets import RectangleSelector

try:
    import pylink
except ImportError:
    sys.exit("pylink not installed. Run: pip install pylink-square")

MAGIC = b"BBIN"
# Header carries the ISR tick of the first record so the wire records can omit
# the per-record tick (records in a batch are consecutive, decimation == 1).
HEADER = struct.Struct("<4sBBHII")
# Firmware wire record (BB_STREAM_VERSION 11), 14 bytes, no tick:
# ia, ib, ic, id, iq (int16, 50 LSB/A),
# theta (int16, full turn over int16 range), duty (int16, 10000 LSB/duty).
WIRE = struct.Struct("<hhhhhhh")
# Self-contained record stored in the temp .bin (tick reconstructed + 7 int16).
RECORD = struct.Struct("<Ihhhhhhh")
STREAM_VERSION = 11
STREAM_I_LSB_PER_A = 50.0
STREAM_DUTY_LSB = 10000.0
STREAM_ANG_LSB = 32768.0 / (2.0 * math.pi)


def decode_record(raw):
    # raw = (tick, ia, ib, ic, id, iq, theta, duty) as fixed-point ints.
    tick = raw[0]
    ia, ib, ic, idc, iqc, theta, duty = raw[1:8]
    return (
        tick,
        ia / STREAM_I_LSB_PER_A,
        ib / STREAM_I_LSB_PER_A,
        ic / STREAM_I_LSB_PER_A,
        idc / STREAM_I_LSB_PER_A,
        iqc / STREAM_I_LSB_PER_A,
        theta / STREAM_ANG_LSB,
        duty / STREAM_DUTY_LSB,
    )

RAW_COLUMNS = [
    "tick", "ia", "ib", "ic", "id", "iq", "theta", "duty",
]

PLOT_COLUMNS = [
    "ia", "ib", "ic", "id", "iq", "theta", "duty",
]

DEFAULT_PANELS = [
    {"title": "Phase currents", "cols": ["ia", "ib", "ic"]},
    {"title": "dq currents", "cols": ["id", "iq"]},
    {"title": "Theta", "cols": ["theta"]},
    {"title": "Duty", "cols": ["duty"]},
]


def wait_for_rtt(jlink):
    for _ in range(50):
        try:
            if jlink.rtt_get_num_up_buffers() > 0:
                return
        except pylink.errors.JLinkRTTException:
            pass
        time.sleep(0.1)
    raise RuntimeError("RTT control block not found. Is the blackbox firmware running?")


def parse_frames(buffer):
    records = []
    payloads = []
    bad_frames = 0

    while True:
        start = buffer.find(MAGIC)
        if start < 0:
            del buffer[:-3]
            return records, payloads, bad_frames

        if start > 0:
            del buffer[:start]

        if len(buffer) < HEADER.size:
            return records, payloads, bad_frames

        magic, version, record_size, count, first_tick, checksum = HEADER.unpack(buffer[:HEADER.size])
        if magic != MAGIC or version != STREAM_VERSION or record_size != WIRE.size:
            del buffer[0]
            bad_frames += 1
            continue

        frame_len = HEADER.size + record_size * count
        if len(buffer) < frame_len:
            return records, payloads, bad_frames

        payload = buffer[HEADER.size:frame_len]
        if stream_checksum(payload) != checksum:
            next_magic = buffer.find(MAGIC, 1)
            if next_magic >= 0:
                del buffer[:next_magic]
            else:
                del buffer[:-3]
            bad_frames += 1
            continue

        disk_chunk = bytearray()
        pos = HEADER.size
        for i in range(count):
            wire = WIRE.unpack(buffer[pos:pos + record_size])
            tick = first_tick + i
            records.append(decode_record((tick,) + wire))
            disk_chunk += RECORD.pack(tick, *wire)
            pos += record_size
        payloads.append(bytes(disk_chunk))

        del buffer[:frame_len]


def stream_checksum(data):
    total = 0x9E3779B9
    for byte in data:
        total = ((total << 5) | (total >> 27)) & 0xFFFFFFFF
        total ^= byte
        total = (total + 0x7F4A7C15) & 0xFFFFFFFF
    return total


def get_unit(col):
    if col in ("ia", "ib", "ic", "id", "iq", "i_abs", "i_abs_filter"):
        return "A"
    if col == "v_bus":
        return "V"
    if col == "erpm":
        return "ERPM"
    if col == "speed_rad_s":
        return "rad/s"
    if col in ("phase", "theta", "theta_used"):
        return "rad"
    return ""


class LiveStore:
    def __init__(self, sample_rate):
        self.sample_rate = sample_rate
        self.tick0 = None
        self.wall0 = None
        self.last_tick = None
        self.missing_ticks = 0
        self.row_count = 0
        self.data = {"t_s": []}
        for col in PLOT_COLUMNS:
            self.data[col] = []

    def append_record(self, rec, wall_now=None):
        values = dict(zip(RAW_COLUMNS, rec))
        if self.tick0 is None:
            self.tick0 = values["tick"]
        if self.wall0 is None:
            self.wall0 = wall_now if wall_now is not None else time.time()

        if self.last_tick is not None and values["tick"] > self.last_tick + 1:
            self.missing_ticks += values["tick"] - self.last_tick - 1
        self.last_tick = values["tick"]

        t_s = (values["tick"] - self.tick0) / self.sample_rate
        wall_time_s = (wall_now if wall_now is not None else time.time()) - self.wall0
        row = {"wall_time_s": wall_time_s, "t_s": t_s, **values}
        self.row_count += 1

        self.data["t_s"].append(t_s)
        for col in PLOT_COLUMNS:
            self.data[col].append(row[col])

        return row

    def clear(self):
        self.tick0 = None
        self.wall0 = None
        self.last_tick = None
        self.missing_ticks = 0
        self.row_count = 0
        for values in self.data.values():
            values.clear()

    def x(self):
        return self.data["t_s"]

    def y(self, col):
        return self.data[col]

    def nearest_index(self, x_val):
        xs = self.x()
        if not xs:
            return 0
        return min(range(len(xs)), key=lambda i: abs(xs[i] - x_val))


class RttScopeApp:
    def __init__(self, root, args):
        self.root = root
        self.args = args
        self.store = LiveStore(args.sample_rate)
        self.buffer = bytearray()
        self.data_lock = threading.RLock()
        self.rtt_lock = threading.Lock()
        self.raw_lock = threading.Lock()
        self.reader_stop = threading.Event()
        self.reader_thread = None

        self.jlink = None
        self.streaming = False
        self.connected = False
        self.auto_follow = True
        self.last_plot_update = 0.0
        self.last_status_update = 0.0
        self.bad_frames = 0
        self.raw_file = None
        self.raw_path = None
        self.raw_record_count = 0
        self.raw_tick0 = None
        self.last_raw_tick = None
        self.missing_ticks = 0

        self.panels = [dict(title=p["title"], cols=list(p["cols"])) for p in DEFAULT_PANELS]
        self.current_panel = 0
        self.axes = []
        self.lines = {}
        self.selectors = []
        self.box_zoom_mode = None
        self.wheel_mode = tk.StringVar(value="XY")
        self.window_s = tk.DoubleVar(value=args.window)
        self.status_text = tk.StringVar(value="未连接")

        self.root.title("VESC RTT Live Scope")
        self.root.geometry("1450x900")
        self.root.protocol("WM_DELETE_WINDOW", self.on_close)

        self.build_ui()
        self.connect_rtt()
        self.rebuild_figure(keep_xlim=False)
        self.start_reader_thread()
        self.root.after(50, self.gui_tick)

        if args.auto_start:
            self.start_stream()

    def build_ui(self):
        main = ttk.Frame(self.root)
        main.pack(fill=tk.BOTH, expand=True)

        left = ttk.Frame(main, width=285)
        left.pack(side=tk.LEFT, fill=tk.Y, padx=8, pady=8)
        left.pack_propagate(False)

        right = ttk.Frame(main)
        right.pack(side=tk.RIGHT, fill=tk.BOTH, expand=True)

        ttk.Label(left, text="RTT Live Scope", font=("", 12, "bold")).pack(anchor="w")
        ttk.Label(left, text=f"CSV 保存基名: {self.args.out}", wraplength=260).pack(anchor="w", pady=(2, 8))

        row = ttk.Frame(left)
        row.pack(fill=tk.X, pady=2)
        ttk.Button(row, text="Start", command=self.start_stream).pack(side=tk.LEFT, expand=True, fill=tk.X, padx=(0, 2))
        ttk.Button(row, text="Stop", command=self.stop_stream).pack(side=tk.LEFT, expand=True, fill=tk.X, padx=2)
        ttk.Button(row, text="Save", command=self.save_now).pack(side=tk.LEFT, expand=True, fill=tk.X, padx=(2, 0))

        row = ttk.Frame(left)
        row.pack(fill=tk.X, pady=2)
        ttk.Button(row, text="清空数据", command=self.clear_data).pack(side=tk.LEFT, expand=True, fill=tk.X, padx=(0, 2))
        ttk.Button(row, text="重置视图", command=self.reset_view).pack(side=tk.LEFT, expand=True, fill=tk.X, padx=(2, 0))

        ttk.Separator(left).pack(fill=tk.X, pady=10)
        ttk.Label(left, text="子图列表").pack(anchor="w")
        self.panel_list = tk.Listbox(left, height=7, exportselection=False)
        self.panel_list.pack(fill=tk.X, pady=(2, 6))
        self.panel_list.bind("<<ListboxSelect>>", self.on_panel_select)

        row = ttk.Frame(left)
        row.pack(fill=tk.X, pady=2)
        ttk.Button(row, text="新增子图", command=self.add_panel_dialog).pack(side=tk.LEFT, expand=True, fill=tk.X, padx=(0, 2))
        ttk.Button(row, text="编辑子图", command=self.edit_panel_dialog).pack(side=tk.LEFT, expand=True, fill=tk.X, padx=(2, 0))
        ttk.Button(left, text="删除子图", command=self.delete_panel).pack(fill=tk.X, pady=(2, 0))

        ttk.Separator(left).pack(fill=tk.X, pady=10)
        ttk.Button(left, text="当前 X 范围内自动缩放 Y", command=self.autoscale_all_y_to_visible_x).pack(fill=tk.X)

        ttk.Label(left, text="显示窗口秒数(运行时自动跟随)").pack(anchor="w", pady=(8, 2))
        ttk.Entry(left, textvariable=self.window_s).pack(fill=tk.X)

        ttk.Separator(left).pack(fill=tk.X, pady=10)
        ttk.Label(left, text="滚轮缩放模式").pack(anchor="w")
        ttk.Combobox(left, textvariable=self.wheel_mode, values=["XY", "X", "Y"], state="readonly").pack(fill=tk.X)
        ttk.Label(
            left,
            text="鼠标靠近 X 轴滚轮只缩放 X；靠近 Y 轴只缩放 Y；图内按上方模式缩放。",
            wraplength=260,
        ).pack(anchor="w", pady=(4, 0))

        ttk.Separator(left).pack(fill=tk.X, pady=10)
        ttk.Label(left, text="框选缩放").pack(anchor="w")
        row = ttk.Frame(left)
        row.pack(fill=tk.X, pady=2)
        ttk.Button(row, text="XY", command=lambda: self.set_box_zoom("XY")).pack(side=tk.LEFT, expand=True, fill=tk.X, padx=(0, 2))
        ttk.Button(row, text="X", command=lambda: self.set_box_zoom("X")).pack(side=tk.LEFT, expand=True, fill=tk.X, padx=2)
        ttk.Button(row, text="Y", command=lambda: self.set_box_zoom("Y")).pack(side=tk.LEFT, expand=True, fill=tk.X, padx=(2, 0))
        ttk.Button(left, text="关闭框选", command=lambda: self.set_box_zoom(None)).pack(fill=tk.X, pady=(2, 0))

        ttk.Separator(left).pack(fill=tk.X, pady=10)
        ttk.Checkbutton(left, text="运行时自动跟随最新数据", variable=tk.BooleanVar(value=True), command=self.toggle_follow).pack(anchor="w")
        ttk.Label(left, textvariable=self.status_text, wraplength=260).pack(anchor="w", pady=(8, 0))

        self.fig = Figure(figsize=(12, 8), dpi=100)
        self.canvas = FigureCanvasTkAgg(self.fig, master=right)
        self.toolbar = NavigationToolbar2Tk(self.canvas, right, pack_toolbar=False)
        self.toolbar.update()
        self.toolbar.pack(side=tk.TOP, fill=tk.X)
        self.canvas.get_tk_widget().pack(side=tk.TOP, fill=tk.BOTH, expand=True)

        self.canvas.mpl_connect("scroll_event", self.on_scroll)

    def connect_rtt(self):
        try:
            self.jlink = pylink.JLink()
            self.jlink.open()
            self.jlink.set_tif(pylink.enums.JLinkInterfaces.SWD)
            self.jlink.connect(self.args.device, speed=self.args.speed)
            self.jlink.rtt_start(None)
            wait_for_rtt(self.jlink)
            self.jlink.rtt_read(0, 4096)
            self.connected = True
            self.status_text.set(f"RTT 已连接，SWD {self.args.speed} kHz")
        except Exception as e:
            messagebox.showerror("RTT 连接失败", str(e), parent=self.root)
            self.status_text.set("RTT 连接失败")

    def start_stream(self):
        if not self.connected or self.streaming:
            return
        self.reset_raw_capture()
        self.buffer.clear()
        with self.rtt_lock:
            self.jlink.rtt_read(0, 4096)
            self.jlink.rtt_write(0, list(b"s"))
        self.streaming = True
        self.auto_follow = True
        self.status_text.set("Streaming")

    def stop_stream(self):
        if not self.connected:
            return
        with self.rtt_lock:
            self.jlink.rtt_write(0, list(b"x"))
        self.streaming = False
        self.buffer.clear()
        self.status_text.set("Stopped，图像已冻结，可缩放查看")
        self.canvas.draw_idle()

    def toggle_follow(self):
        self.auto_follow = not self.auto_follow

    def reset_raw_capture(self):
        if self.raw_file:
            self.raw_file.close()
        if self.raw_path and os.path.exists(self.raw_path):
            try:
                os.remove(self.raw_path)
            except OSError:
                pass
        self.raw_file = tempfile.NamedTemporaryFile(prefix="bb_live_", suffix=".bin", delete=False)
        self.raw_path = self.raw_file.name
        self.raw_record_count = 0
        self.raw_tick0 = None
        self.last_raw_tick = None
        self.missing_ticks = 0

    def save_now(self):
        if not self.raw_path or self.raw_record_count == 0:
            self.status_text.set("没有可保存的数据")
            return

        with self.raw_lock:
            if self.raw_file:
                self.raw_file.flush()
            out_path = self.timestamped_csv_path(self.args.out)
            rows = self.export_raw_to_csv(out_path)

        self.status_text.set(f"已保存 CSV: {out_path} ({rows} rows)")

    def timestamped_csv_path(self, path):
        root, ext = os.path.splitext(path)
        if not ext:
            ext = ".csv"
        stamp = dt.datetime.now().strftime("%Y%m%d_%H%M%S")
        return f"{root}_{stamp}{ext}"

    def export_raw_to_csv(self, out_path):
        rows = 0
        tick0 = None
        with open(self.raw_path, "rb") as raw, open(out_path, "w", newline="") as out:
            writer = csv.DictWriter(out, fieldnames=["wall_time_s", "t_s"] + RAW_COLUMNS)
            writer.writeheader()
            while True:
                data = raw.read(RECORD.size)
                if not data:
                    break
                if len(data) != RECORD.size:
                    break
                values = dict(zip(RAW_COLUMNS, decode_record(RECORD.unpack(data))))
                if tick0 is None:
                    tick0 = values["tick"]
                t_s = (values["tick"] - tick0) / self.args.sample_rate
                writer.writerow({"wall_time_s": t_s, "t_s": t_s, **values})
                rows += 1
        return rows

    def clear_data(self):
        if self.streaming:
            self.stop_stream()
        with self.data_lock:
            self.store.clear()
        self.bad_frames = 0
        self.buffer.clear()
        self.reset_raw_capture()
        self.rebuild_figure(keep_xlim=False)
        self.status_text.set("已清空数据")

    def start_reader_thread(self):
        self.reader_thread = threading.Thread(target=self.reader_loop, name="rtt-reader", daemon=True)
        self.reader_thread.start()

    def reader_loop(self):
        last_flush = time.time()
        while not self.reader_stop.is_set():
            if not self.connected or not self.streaming:
                time.sleep(0.002)
                continue

            drained = False
            for _ in range(64):
                with self.rtt_lock:
                    data = self.jlink.rtt_read(0, 8192)
                if not data:
                    break
                drained = True
                self.buffer.extend(bytes(data))

            if drained:
                records, payloads, bad_frames = parse_frames(self.buffer)
                if bad_frames:
                    self.bad_frames += bad_frames
                if payloads and self.raw_file:
                    with self.raw_lock:
                        for payload in payloads:
                            self.raw_file.write(payload)

                if records:
                    wall_now = time.time()
                    plot_records = []
                    for rec in records:
                        tick = rec[0]
                        if self.raw_tick0 is None:
                            self.raw_tick0 = tick
                        if self.last_raw_tick is not None and tick > self.last_raw_tick + 1:
                            self.missing_ticks += tick - self.last_raw_tick - 1
                        self.last_raw_tick = tick
                        self.raw_record_count += 1
                        if (self.raw_record_count % self.args.plot_decimation) == 0:
                            plot_records.append(rec)

                    with self.data_lock:
                        for rec in plot_records:
                            self.store.append_record(rec, wall_now=wall_now)
            else:
                time.sleep(0.0005)

            now = time.time()
            if self.raw_file and now - last_flush > 0.25:
                with self.raw_lock:
                    self.raw_file.flush()
                last_flush = now

    def gui_tick(self):
        if self.streaming:
            self.update_plot_live()
            self.update_stream_status()
        self.root.after(50, self.gui_tick)

    def update_stream_status(self):
        raw_count = self.raw_record_count
        if self.raw_tick0 is not None and self.last_raw_tick is not None:
            t_s = (self.last_raw_tick - self.raw_tick0) / self.args.sample_rate
        else:
            t_s = 0.0
        saved_hz = raw_count / t_s if t_s > 0 else 0.0
        self.status_text.set(
            f"Streaming | raw={raw_count} | rate≈{saved_hz:.0f} Hz | "
            f"missing_ticks={self.missing_ticks} | bad_frames={self.bad_frames}"
        )

    def choose_columns_dialog(self, title, initial=None):
        initial = set(initial or [])
        win = tk.Toplevel(self.root)
        win.title(title)
        win.geometry("360x500")
        win.transient(self.root)
        win.grab_set()

        result = {"cols": None}
        lb = tk.Listbox(win, selectmode=tk.EXTENDED, exportselection=False)
        lb.pack(fill=tk.BOTH, expand=True, padx=10, pady=10)
        for col in PLOT_COLUMNS:
            lb.insert(tk.END, col)
            if col in initial:
                lb.selection_set(tk.END)

        btns = ttk.Frame(win)
        btns.pack(fill=tk.X, padx=10, pady=(0, 10))

        def ok():
            cols = [PLOT_COLUMNS[i] for i in lb.curselection()]
            if not cols:
                messagebox.showwarning("未选择变量", "至少选择一个变量。", parent=win)
                return
            result["cols"] = cols
            win.destroy()

        ttk.Button(btns, text="确定", command=ok).pack(side=tk.RIGHT, padx=(5, 0))
        ttk.Button(btns, text="取消", command=win.destroy).pack(side=tk.RIGHT)
        self.root.wait_window(win)
        return result["cols"]

    def add_panel_dialog(self):
        cols = self.choose_columns_dialog("新增子图")
        if cols:
            self.panels.append({"title": f"Panel {len(self.panels) + 1}", "cols": cols})
            self.current_panel = len(self.panels) - 1
            self.rebuild_figure(keep_xlim=True)

    def edit_panel_dialog(self):
        if not self.panels:
            return
        cols = self.choose_columns_dialog("编辑子图", self.panels[self.current_panel]["cols"])
        if cols:
            self.panels[self.current_panel]["cols"] = cols
            self.rebuild_figure(keep_xlim=True)

    def delete_panel(self):
        if len(self.panels) <= 1:
            return
        del self.panels[self.current_panel]
        self.current_panel = max(0, min(self.current_panel, len(self.panels) - 1))
        self.rebuild_figure(keep_xlim=True)

    def on_panel_select(self, _event=None):
        sel = self.panel_list.curselection()
        if sel:
            self.current_panel = int(sel[0])

    def refresh_panel_list(self):
        self.panel_list.delete(0, tk.END)
        for i, panel in enumerate(self.panels):
            self.panel_list.insert(tk.END, f"{i + 1}: {', '.join(panel['cols'])}")
        if self.panels:
            self.panel_list.selection_set(self.current_panel)

    def rebuild_figure(self, keep_xlim=True):
        old_xlim = self.axes[0].get_xlim() if keep_xlim and self.axes else None
        self.fig.clear()
        self.axes = []
        self.lines = {}

        n = len(self.panels)
        first_ax = None
        for i, panel in enumerate(self.panels):
            ax = self.fig.add_subplot(n, 1, i + 1, sharex=first_ax)
            if first_ax is None:
                first_ax = ax
            self.axes.append(ax)
            for col in panel["cols"]:
                line, = ax.plot([], [], lw=1.1, label=col)
                self.lines[(i, col)] = line
            ax.grid(True, linestyle="--", alpha=0.3)
            ax.set_ylabel(self.panel_ylabel(panel["cols"]))
            ax.set_title(panel["title"], loc="left", fontsize=10)
            ax.legend(loc="upper right", fontsize=8)

        if self.axes:
            self.axes[-1].set_xlabel("time / s")
            if old_xlim:
                self.axes[0].set_xlim(old_xlim)

        self.fig.tight_layout(rect=[0, 0, 1, 0.97])
        self.refresh_panel_list()
        self.create_selectors()
        self.update_all_line_data()
        self.canvas.draw_idle()

    def panel_ylabel(self, cols):
        units = {get_unit(c) for c in cols if get_unit(c)}
        return "/".join(sorted(units)) if units else "value"

    def update_all_line_data(self):
        with self.data_lock:
            xs = list(self.store.x())
            ys_by_col = {col: list(self.store.y(col)) for col in PLOT_COLUMNS}
        for (_panel_idx, col), line in self.lines.items():
            line.set_data(xs, ys_by_col[col])

    def update_visible_line_data(self, x0, x1):
        with self.data_lock:
            xs = self.store.x()
            if not xs:
                return

            i0 = max(0, bisect.bisect_left(xs, x0) - 1)
            i1 = min(len(xs), bisect.bisect_right(xs, x1) + 1)
            x_view = list(xs[i0:i1])
            y_view = {col: list(self.store.y(col)[i0:i1]) for col in PLOT_COLUMNS}

        for (_panel_idx, col), line in self.lines.items():
            line.set_data(x_view, y_view[col])

    def update_plot_live(self):
        with self.data_lock:
            xs = list(self.store.x())
        if not xs:
            self.canvas.draw_idle()
            return

        if self.auto_follow:
            window = max(0.05, float(self.window_s.get() or 2.0))
            xmax = xs[-1]
            xmin = max(0.0, xmax - window)
            self.axes[0].set_xlim(xmin, max(window, xmax))
            self.update_visible_line_data(xmin, max(window, xmax))
            self.autoscale_all_y_to_visible_x(redraw=False)
        else:
            x0, x1 = self.axes[0].get_xlim()
            self.update_visible_line_data(min(x0, x1), max(x0, x1))
            for ax in self.axes:
                ax.relim()
                ax.autoscale_view()

        self.canvas.draw_idle()

    def autoscale_all_y_to_visible_x(self, redraw=True):
        if not self.axes:
            return
        with self.data_lock:
            xs = list(self.store.x())
            ys_by_col = {col: list(self.store.y(col)) for col in PLOT_COLUMNS}
        if not xs:
            return
        x0, x1 = self.axes[0].get_xlim()
        if x0 > x1:
            x0, x1 = x1, x0
        i0 = max(0, bisect.bisect_left(xs, x0) - 1)
        i1 = min(len(xs), bisect.bisect_right(xs, x1) + 1)
        if i1 <= i0:
            i0, i1 = 0, len(xs)

        for panel_idx, ax in enumerate(self.axes):
            ys_all = []
            for col in self.panels[panel_idx]["cols"]:
                ys = ys_by_col[col]
                if ys:
                    ys_all.extend(y for y in ys[i0:i1] if math.isfinite(y))
            if not ys_all:
                continue
            y_min = min(ys_all)
            y_max = max(ys_all)
            if y_min == y_max:
                pad = abs(y_min) * 0.1 or 1.0
            else:
                pad = (y_max - y_min) * 0.08
            ax.set_ylim(y_min - pad, y_max + pad)
        if redraw:
            self.canvas.draw_idle()

    def reset_view(self):
        with self.data_lock:
            xs = list(self.store.x())
        if xs and self.axes:
            self.axes[0].set_xlim(xs[0], xs[-1] if xs[-1] > xs[0] else xs[0] + 1.0)
            self.autoscale_all_y_to_visible_x(redraw=False)
        self.canvas.draw_idle()

    def create_selectors(self):
        for selector in self.selectors:
            selector.set_active(False)
        self.selectors = []
        for ax in self.axes:
            selector = RectangleSelector(
                ax, self.on_box_select, useblit=True, button=[1],
                minspanx=5, minspany=5, spancoords="pixels", interactive=False,
            )
            selector.set_active(self.box_zoom_mode is not None)
            self.selectors.append(selector)

    def set_box_zoom(self, mode):
        self.box_zoom_mode = mode
        for selector in self.selectors:
            selector.set_active(mode is not None)
        self.status_text.set(f"框选缩放: {mode}" if mode else "框选缩放关闭")

    def on_box_select(self, eclick, erelease):
        if self.box_zoom_mode is None or eclick.inaxes != erelease.inaxes:
            return
        ax = eclick.inaxes
        mode = self.box_zoom_mode.upper()
        if mode in ("X", "XY") and eclick.xdata is not None and erelease.xdata is not None:
            x0, x1 = sorted([eclick.xdata, erelease.xdata])
            if abs(x1 - x0) > 1e-12:
                self.axes[0].set_xlim(x0, x1)
        if mode in ("Y", "XY") and eclick.ydata is not None and erelease.ydata is not None:
            y0, y1 = sorted([eclick.ydata, erelease.ydata])
            if abs(y1 - y0) > 1e-12:
                ax.set_ylim(y0, y1)
        if mode == "X":
            self.autoscale_all_y_to_visible_x(redraw=False)
        self.auto_follow = False
        self.canvas.draw_idle()

    def on_scroll(self, event):
        if not self.axes:
            return
        ax = self.axis_from_event(event)
        if ax is None:
            return

        mode = self.wheel_mode_from_event(event, ax)
        scale = 0.8 if event.button == "up" else 1.25

        if mode in ("X", "XY"):
            center = event.xdata
            if center is None:
                center = ax.transData.inverted().transform((event.x, event.y))[0]
            x0, x1 = self.axes[0].get_xlim()
            self.axes[0].set_xlim(center - (center - x0) * scale, center + (x1 - center) * scale)

        if mode in ("Y", "XY"):
            center = event.ydata
            if center is None:
                center = ax.transData.inverted().transform((event.x, event.y))[1]
            y0, y1 = ax.get_ylim()
            ax.set_ylim(center - (center - y0) * scale, center + (y1 - center) * scale)

        self.auto_follow = False
        self.canvas.draw_idle()

    def axis_from_event(self, event):
        if event.inaxes in self.axes:
            return event.inaxes
        for ax in self.axes:
            box = ax.bbox
            if box.x0 - 70 <= event.x <= box.x1 + 10 and box.y0 - 45 <= event.y <= box.y1 + 10:
                return ax
        return None

    def wheel_mode_from_event(self, event, ax):
        box = ax.bbox
        if box.x0 <= event.x <= box.x1 and box.y0 - 45 <= event.y <= box.y0 + 8:
            return "X"
        if box.x0 - 70 <= event.x <= box.x0 + 8 and box.y0 <= event.y <= box.y1:
            return "Y"
        return self.wheel_mode.get().upper()

    def on_close(self):
        try:
            self.stop_stream()
        except Exception:
            pass
        self.reader_stop.set()
        if self.reader_thread and self.reader_thread.is_alive():
            self.reader_thread.join(timeout=1.0)
        if self.raw_file:
            self.raw_file.flush()
            self.raw_file.close()
        if self.raw_path and os.path.exists(self.raw_path):
            try:
                os.remove(self.raw_path)
            except OSError:
                pass
        if self.jlink:
            try:
                self.jlink.close()
            except Exception:
                pass
        self.root.destroy()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--device", default="STM32F407VG", help="J-Link device name")
    parser.add_argument("--speed", type=int, default=8000, help="SWD speed in kHz")
    parser.add_argument("--out", default="bb_live.csv", help="CSV output file")
    parser.add_argument("--png", help="Save figure to this PNG")
    parser.add_argument("--sample-rate", type=float, default=15000.0,
                        help="FOC ADC ISR frequency in Hz; 75_300 low-side V0 default is 15000")
    parser.add_argument("--window", type=float, default=2.0, help="Auto-follow window in seconds")
    parser.add_argument("--plot-decimation", type=int, default=10,
                        help="Only every Nth raw sample is kept for GUI plotting; CSV export keeps all raw samples")
    parser.add_argument("--auto-start", action="store_true", help="Start streaming immediately")
    args = parser.parse_args()
    args.plot_decimation = max(1, args.plot_decimation)

    root = tk.Tk()
    try:
        RttScopeApp(root, args)
        root.mainloop()
    except Exception as e:
        messagebox.showerror("bb_live failed", str(e))
        raise


if __name__ == "__main__":
    main()
