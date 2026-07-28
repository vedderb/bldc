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

#include "QLbmPlotItem.h"
#include "qcustomplot.h"

#include <QPainter>
#include <QCoreApplication>
#include <QHoverEvent>
#include <QMouseEvent>
#include <QWheelEvent>

QLbmPlotItem::QLbmPlotItem(QQuickItem *parent)
    : QQuickPaintedItem(parent)
    , m_plot(new QCustomPlot()) {

    m_plot->setBackground(Qt::white);

    // Enable the full set of built-in QCustomPlot interactions.
    m_plot->setInteractions(QCP::iRangeDrag | QCP::iRangeZoom |
                            QCP::iSelectPlottables | QCP::iSelectAxes |
                            QCP::iSelectLegend | QCP::iSelectItems |
                            QCP::iMultiSelect);

    // Trigger a Qt Quick repaint every time QCustomPlot finishes a replot
    // (which happens internally after drag-to-pan, scroll-to-zoom, etc.).
    QObject::connect(m_plot, &QCustomPlot::afterReplot, this, [this]() { update(); });

    // Accept all mouse buttons and hover events so Qt Quick delivers them to us.
    setAcceptedMouseButtons(Qt::AllButtons);
    setAcceptHoverEvents(true);

    setImplicitWidth(400);
    setImplicitHeight(300);
}

QLbmPlotItem::~QLbmPlotItem() {
    delete m_plot;
}

int QLbmPlotItem::addGraph(const QString &name, const QColor &color) {
    QCPGraph *graph = m_plot->addGraph();
    if (!graph) return -1;
    graph->setName(name);
    if (color.isValid())
        graph->setPen(QPen(color, 1.5));
    return m_plot->graphCount() - 1;
}

void QLbmPlotItem::setData(int idx, const QVector<double> &keys, const QVector<double> &values) {
    if (idx < 0 || idx >= m_plot->graphCount()) return;
    m_plot->graph(idx)->setData(keys, values);
}

void QLbmPlotItem::appendPoint(int idx, double key, double value) {
    if (idx < 0 || idx >= m_plot->graphCount()) return;
    m_plot->graph(idx)->addData(key, value);
}

void QLbmPlotItem::clearGraph(int idx) {
    if (idx < 0 || idx >= m_plot->graphCount()) return;
    m_plot->graph(idx)->data()->clear();
}

void QLbmPlotItem::clearAll() {
    for (int i = 0; i < m_plot->graphCount(); i++)
        m_plot->graph(i)->data()->clear();
}

void QLbmPlotItem::setTitle(const QString &title) {
    // Insert or update a QCPTextElement title row at the top of the plot layout.
    if (m_plot->plotLayout()->rowCount() > 1) {
        auto *el = qobject_cast<QCPTextElement*>(m_plot->plotLayout()->element(0, 0));
        if (el) { el->setText(title); return; }
    }
    m_plot->plotLayout()->insertRow(0);
    m_plot->plotLayout()->addElement(0, 0, new QCPTextElement(m_plot, title,
                                                               QFont("sans", 10, QFont::Bold)));
}

void QLbmPlotItem::setXLabel(const QString &label) {
    m_plot->xAxis->setLabel(label);
}

void QLbmPlotItem::setYLabel(const QString &label) {
    m_plot->yAxis->setLabel(label);
}

void QLbmPlotItem::setXRange(double lo, double hi) {
    m_plot->xAxis->setRange(lo, hi);
}

void QLbmPlotItem::setYRange(double lo, double hi) {
    m_plot->yAxis->setRange(lo, hi);
}

void QLbmPlotItem::autoRescale() {
    m_plot->rescaleAxes();
}

void QLbmPlotItem::triggerReplot() {
    update();
}

bool QLbmPlotItem::savePng(const QString &path, int w, int h) {
    return m_plot->savePng(path, w, h);
}

bool QLbmPlotItem::savePdf(const QString &path, int w, int h) {
    return m_plot->savePdf(path, w, h);
}

void QLbmPlotItem::paint(QPainter *painter) {
    int w = qMax(1, (int)width());
    int h = qMax(1, (int)height());
    m_plot->resize(w, h);
    // toPixmap() does a fresh full render of the current plot state.
    QPixmap pm = m_plot->toPixmap(w, h);
    painter->drawPixmap(0, 0, pm);
}

void QLbmPlotItem::geometryChanged(const QRectF &newGeometry, const QRectF &oldGeometry) {
    QQuickPaintedItem::geometryChanged(newGeometry, oldGeometry);
    update();
}

// ////////////////////////////////////////////////////////////
// Qt Quick input events → QCustomPlot forwarding.
//
// The plot is always resized to match this item's bounds, so item-local
// coordinates equal plot-widget coordinates. We construct fresh QMouseEvent /
// QWheelEvent objects rather than forwarding the originals to avoid any
// confusion with Qt Quick's own event-accepted bookkeeping.

void QLbmPlotItem::mousePressEvent(QMouseEvent *event) {
    QMouseEvent ev(QEvent::MouseButtonPress, event->localPos(),
                   event->button(), event->buttons(), event->modifiers());
    QCoreApplication::sendEvent(m_plot, &ev);
    update();
    event->accept();
}

void QLbmPlotItem::mouseMoveEvent(QMouseEvent *event) {
    QMouseEvent ev(QEvent::MouseMove, event->localPos(),
                   event->button(), event->buttons(), event->modifiers());
    QCoreApplication::sendEvent(m_plot, &ev);
    // afterReplot → update() fires if QCustomPlot replotted (e.g. during drag-pan).
    event->accept();
}

void QLbmPlotItem::mouseReleaseEvent(QMouseEvent *event) {
    QMouseEvent ev(QEvent::MouseButtonRelease, event->localPos(),
                   event->button(), event->buttons(), event->modifiers());
    QCoreApplication::sendEvent(m_plot, &ev);
    update();
    event->accept();
}

void QLbmPlotItem::mouseDoubleClickEvent(QMouseEvent *event) {
    QMouseEvent ev(QEvent::MouseButtonDblClick, event->localPos(),
                   event->button(), event->buttons(), event->modifiers());
    QCoreApplication::sendEvent(m_plot, &ev);
    update();
    event->accept();
}

void QLbmPlotItem::wheelEvent(QWheelEvent *event) {
    // Forward wheel event directly — position fields are in item-local coords.
    QCoreApplication::sendEvent(m_plot, event);
    // afterReplot → update() handles the visual refresh.
    event->accept();
}

// Hover (no button held) lets QCustomPlot emit mouseMove for cursor tracking.
void QLbmPlotItem::hoverMoveEvent(QHoverEvent *event) {
    QMouseEvent ev(QEvent::MouseMove,
#if QT_VERSION >= QT_VERSION_CHECK(5, 14, 0)
                   event->posF(),
#else
                   QPointF(event->pos()),
#endif
                   Qt::NoButton, Qt::NoButton, Qt::NoModifier);
    QCoreApplication::sendEvent(m_plot, &ev);
    event->accept();
}
