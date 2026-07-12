/*
  Copyright 2026 Joel Svensson  svenssonjoel@yahoo.se

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef QLBMPLOTITEM_H
#define QLBMPLOTITEM_H

#include <QQuickPaintedItem>
#include <QVector>
#include <QString>
#include <QColor>

class QCustomPlot;

class QLbmPlotItem : public QQuickPaintedItem {
    Q_OBJECT
public:
    explicit QLbmPlotItem(QQuickItem *parent = nullptr);
    ~QLbmPlotItem() override;

    // Expose the underlying QCustomPlot so callers can connect to its signals.
    QCustomPlot *plot() const { return m_plot; }

    // All methods below must be called on the Qt GUI thread
    // (use QMetaObject::invokeMethod from the LBM evaluator thread).
    int  addGraph(const QString &name, const QColor &color = QColor());
    void setData(int graphIndex, const QVector<double> &keys, const QVector<double> &values);
    void appendPoint(int graphIndex, double key, double value);
    void clearGraph(int graphIndex);
    void clearAll();
    void setTitle(const QString &title);
    void setXLabel(const QString &label);
    void setYLabel(const QString &label);
    void setXRange(double lower, double upper);
    void setYRange(double lower, double upper);
    void autoRescale();
    void triggerReplot();
    bool savePng(const QString &path, int w = 0, int h = 0);
    bool savePdf(const QString &path, int w = 0, int h = 0);

    void paint(QPainter *painter) override;

protected:
    void geometryChanged(const QRectF &newGeometry, const QRectF &oldGeometry) override;

    // Qt Quick input events — forwarded to QCustomPlot for interactive pan/zoom/select.
    void mousePressEvent(QMouseEvent *event) override;
    void mouseMoveEvent(QMouseEvent *event) override;
    void mouseReleaseEvent(QMouseEvent *event) override;
    void mouseDoubleClickEvent(QMouseEvent *event) override;
    void wheelEvent(QWheelEvent *event) override;
    void hoverMoveEvent(QHoverEvent *event) override;

private:
    QCustomPlot *m_plot;
};

#endif
