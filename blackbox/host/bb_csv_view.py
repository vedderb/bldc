#!/usr/bin/env python3
"""View saved bb_live CSV files with multi-panel zoom controls.

Usage:
  python bb_csv_view.py blackbox/host/bb_live_20260612_120355.csv
  python bb_csv_view.py
"""

import argparse
import bisect
import csv
import os
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


DEFAULT_PANELS = [
    {"title": "Phase currents", "cols": ["ia", "ib", "ic"]},
    {"title": "Duty", "cols": ["duty"]},
    {"title": "Fault", "cols": ["fault"]},
]


def get_unit(col):
    if col in ("ia", "ib", "ic", "id", "iq", "i_abs", "i_abs_filter"):
        return "A"
    if col == "duty":
        return ""
    if col == "fault":
        return "code"
    if col == "v_bus":
        return "V"
    return ""


class CsvData:
    def __init__(self, path):
        self.path = path
        self.columns = []
        self.data = {}
        self.load(path)

    def load(self, path):
        with open(path, newline="") as f:
            reader = csv.DictReader(f)
            self.columns = [c for c in (reader.fieldnames or []) if c]
            for col in self.columns:
                self.data[col] = []
            for row in reader:
                for col in self.columns:
                    try:
                        self.data[col].append(float(row[col]))
                    except (ValueError, TypeError, KeyError):
                        self.data[col].append(float("nan"))

        if "t_s" not in self.data:
            raise ValueError("CSV does not contain t_s")

        self.plot_columns = [c for c in self.columns if c not in ("wall_time_s", "t_s", "tick")]

    def x(self):
        return self.data["t_s"]

    def y(self, col):
        return self.data[col]


class CsvScopeApp:
    def __init__(self, root, data):
        self.root = root
        self.data = data
        self.root.title(f"Blackbox CSV Viewer - {os.path.basename(data.path)}")
        self.root.geometry("1450x900")

        self.panels = []
        for panel in DEFAULT_PANELS:
            cols = [c for c in panel["cols"] if c in data.plot_columns]
            if cols:
                self.panels.append({"title": panel["title"], "cols": cols})
        if not self.panels:
            self.panels.append({"title": "Panel 1", "cols": data.plot_columns[:3]})

        self.current_panel = 0
        self.axes = []
        self.lines = {}
        self.selectors = []
        self.box_zoom_mode = None
        self.wheel_mode = tk.StringVar(value="XY")
        self.status_text = tk.StringVar(value=f"{len(data.x())} rows")

        self.build_ui()
        self.rebuild_figure(keep_xlim=False)

    def build_ui(self):
        main = ttk.Frame(self.root)
        main.pack(fill=tk.BOTH, expand=True)

        left = ttk.Frame(main, width=285)
        left.pack(side=tk.LEFT, fill=tk.Y, padx=8, pady=8)
        left.pack_propagate(False)

        right = ttk.Frame(main)
        right.pack(side=tk.RIGHT, fill=tk.BOTH, expand=True)

        ttk.Label(left, text="Blackbox CSV Viewer", font=("", 12, "bold")).pack(anchor="w")
        ttk.Label(left, text=os.path.basename(self.data.path), wraplength=260).pack(anchor="w", pady=(2, 8))

        ttk.Button(left, text="重置视图", command=self.reset_view).pack(fill=tk.X)
        ttk.Button(left, text="当前 X 范围内自动缩放 Y", command=self.autoscale_all_y_to_visible_x).pack(fill=tk.X, pady=(4, 0))

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
        ttk.Label(left, text="滚轮缩放模式").pack(anchor="w")
        ttk.Combobox(left, textvariable=self.wheel_mode, values=["XY", "X", "Y"], state="readonly").pack(fill=tk.X)
        ttk.Label(
            left,
            text="靠近 X 轴滚轮只缩放 X；靠近 Y 轴只缩放 Y；图内按上方模式缩放。",
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
        ttk.Label(left, textvariable=self.status_text, wraplength=260).pack(anchor="w")

        self.fig = Figure(figsize=(12, 8), dpi=100)
        self.canvas = FigureCanvasTkAgg(self.fig, master=right)
        self.toolbar = NavigationToolbar2Tk(self.canvas, right, pack_toolbar=False)
        self.toolbar.update()
        self.toolbar.pack(side=tk.TOP, fill=tk.X)
        self.canvas.get_tk_widget().pack(side=tk.TOP, fill=tk.BOTH, expand=True)
        self.canvas.mpl_connect("scroll_event", self.on_scroll)

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
        for col in self.data.plot_columns:
            lb.insert(tk.END, col)
            if col in initial:
                lb.selection_set(tk.END)

        btns = ttk.Frame(win)
        btns.pack(fill=tk.X, padx=10, pady=(0, 10))

        def ok():
            cols = [self.data.plot_columns[i] for i in lb.curselection()]
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
                line, = ax.plot(self.data.x(), self.data.y(col), lw=1.0, label=col)
                self.lines[(i, col)] = line
            ax.grid(True, linestyle="--", alpha=0.3)
            ax.set_ylabel(self.panel_ylabel(panel["cols"]))
            ax.set_title(panel["title"], loc="left", fontsize=10)
            ax.legend(loc="upper right", fontsize=8)

        if self.axes:
            self.axes[-1].set_xlabel("time / s")
            if old_xlim:
                self.axes[0].set_xlim(old_xlim)
            else:
                xs = self.data.x()
                self.axes[0].set_xlim(xs[0], xs[-1] if xs[-1] > xs[0] else xs[0] + 1.0)

        self.fig.tight_layout(rect=[0, 0, 1, 0.97])
        self.refresh_panel_list()
        self.create_selectors()
        self.autoscale_all_y_to_visible_x(redraw=False)
        self.canvas.draw_idle()

    def panel_ylabel(self, cols):
        units = {get_unit(c) for c in cols if get_unit(c)}
        return "/".join(sorted(units)) if units else "value"

    def autoscale_all_y_to_visible_x(self, redraw=True):
        if not self.axes:
            return
        xs = self.data.x()
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
                ys_all.extend(y for y in self.data.y(col)[i0:i1] if y == y)
            if not ys_all:
                continue
            y_min = min(ys_all)
            y_max = max(ys_all)
            pad = (y_max - y_min) * 0.08 if y_max != y_min else (abs(y_min) * 0.1 or 1.0)
            ax.set_ylim(y_min - pad, y_max + pad)
        if redraw:
            self.canvas.draw_idle()

    def reset_view(self):
        xs = self.data.x()
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


def choose_file():
    root = tk.Tk()
    root.withdraw()
    path = filedialog.askopenfilename(
        title="选择 bb_live CSV",
        filetypes=[("CSV files", "*.csv"), ("All files", "*.*")],
    )
    root.destroy()
    return path


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("csvfile", nargs="?", help="Saved bb_live CSV")
    args = parser.parse_args()

    path = args.csvfile or choose_file()
    if not path:
        return

    root = tk.Tk()
    try:
        data = CsvData(path)
        CsvScopeApp(root, data)
        root.mainloop()
    except Exception as e:
        messagebox.showerror("bb_csv_view failed", str(e))
        raise


if __name__ == "__main__":
    main()
