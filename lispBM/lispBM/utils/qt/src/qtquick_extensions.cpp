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

#include "qtquick_extensions.h"
#include "QLbmQuickDisplayItem.h"
#include "QLbmPlotItem.h"
#include "qcustomplot.h"

#include <QQuickItem>
#include <QQmlEngine>
#include <QQmlComponent>
#include <QQmlProperty>
#include <QMetaObject>
#include <QHash>
#include <QVector>
#include <QString>
#include <QStringList>
#include <QObject>
#include <QDebug>

extern "C" {
#include "lispbm.h"
#include "heap.h"
#include "extensions/display_extensions.h"
}

#include <cstring>

// ////////////////////////////////////////////////////////////
// Event sender — forwards widget events to the LispBM evaluator.
// Default implementation calls lbm_event / lbm_event_unboxed directly,
// which works for any host (REPL, QLispBM-based app, etc.).
// Can be overridden with lbm_qtquick_set_event_sender() if needed.

static bool default_event_sender(const QLbmValue &value) {
  if (value.isUnboxed())
    return lbm_event_unboxed(value.unboxed());
  lbm_flat_value_t fv;
  if (!value.flatten(&fv)) return false;
  if (!lbm_event(&fv)) { lbm_free(fv.buf); return false; }
  return true;
}

static bool (*s_event_sender)(const QLbmValue &) = default_event_sender;

void lbm_qtquick_set_event_sender(bool (*fn)(const QLbmValue &)) {
  s_event_sender = fn;
}

static void send_event(const QLbmValue &value) {
  if (s_event_sender) s_event_sender(value);
}

// ////////////////////////////////////////////////////////////
// Qt signals to LBM events adapter
//
// It is a Q_OBJECT and used only locally in this file.
// Q_OBJECTS needs to be MOCed  and to make that happen there
// is a hacky "#include" at the bottom of this file

class LbmSignalAdapter : public QObject {
  Q_OBJECT
public:
  enum class WidgetType {
    Button, Checkbox, Radio, SpinboxI, SpinboxF,
    Textfield, Slider, Combo
  };

  explicit LbmSignalAdapter(int handle, WidgetType type,
                             QQuickItem *source, QObject *parent = nullptr)
    : QObject(parent), m_handle(handle), m_type(type), m_source(source) {}

public slots:
  void onClicked() {
    sendSymHandle("button-pressed");
  }

  void onCheckedChanged() {
    bool v = m_source->property("checked").toBool();
    if (m_type == WidgetType::Radio && !v) return; // only fire when becoming checked
    if (m_type == WidgetType::Checkbox) {
      sendSymHandleBool("checkbox-changed", v);
    } else {
      sendSymHandle("radio-changed");
    }
  }

  void onValueModified() {
    if (m_type == WidgetType::SpinboxI) {
      int v = m_source->property("value").toInt();
      sendSymHandleInt("spinbox-changed", v);
    } else {
      float v = (float)m_source->property("value").toDouble();
      sendSymHandleFloat("spinbox-changed", v);
    }
  }

  void onMoved() {
    int v = qRound(m_source->property("value").toDouble()
                   * m_source->property("_lbmSliderScale").toDouble());
    sendSymHandleInt("slider-changed", v);
  }

  void onEditingFinished() {
    QString text = m_source->property("text").toString();
    QByteArray ba = text.toUtf8();
    ba.append('\0');
    send_event(QLbmValue::fromList({QLbmValue::fromSymbol("textfield-commit"),
                                    QLbmValue::fromI(m_handle),
                                    QLbmValue::fromByteArray(ba)}));
  }

  void onActivated(int index) {
    sendSymHandleInt("combo-changed", index);
  }

private:
  void sendSymHandle(const char *sym) {
    send_event(QLbmValue::fromList({QLbmValue::fromSymbol(sym),
                                    QLbmValue::fromI(m_handle)}));
  }

  void sendSymHandleBool(const char *sym, bool v) {
    send_event(QLbmValue::fromList({QLbmValue::fromSymbol(sym),
                                    QLbmValue::fromI(m_handle),
                                    v ? QLbmValue::fromSymbol("t") : QLbmValue()}));
  }

  void sendSymHandleInt(const char *sym, int v) {
    send_event(QLbmValue::fromList({QLbmValue::fromSymbol(sym),
                                    QLbmValue::fromI(m_handle),
                                    QLbmValue::fromI(v)}));
  }

  void sendSymHandleFloat(const char *sym, float v) {
    send_event(QLbmValue::fromList({QLbmValue::fromSymbol(sym),
                                    QLbmValue::fromI(m_handle),
                                    QLbmValue::fromFloat(v)}));
  }

  int          m_handle;
  WidgetType   m_type;
  QQuickItem  *m_source;
};

// ////////////////////////////////////////////////////////////
// Module state

enum class LbmQtWidgetType {
  Container, Display, Button, Checkbox, Radio,
  SpinboxI, SpinboxF, Textfield, Label, Slider, Combo, Stretch, Plot
};

static QQmlEngine             *s_engine     = nullptr;
static QQuickItem             *s_root       = nullptr;
static int                     s_rootHandle = -1;
static int                     s_nextHandle = 0;
static QHash<int, QQuickItem*>        s_items;
static QHash<int, LbmQtWidgetType>    s_types;
static QLbmQuickDisplayItem          *s_activeDisplay = nullptr;
class LbmPlotAdapter;
static QHash<int, LbmPlotAdapter*>    s_plotAdapters;

// ////////////////////////////////////////////////////////////
// QML creation

static const QByteArray k_qmlPrefix =
  "import QtQuick 2.15\n"
  "import QtQuick.Controls 2.15\n"
  "import QtQuick.Layouts 1.15\n";

static QQuickItem *createQmlItem(const QByteArray &body, QQuickItem *parent = nullptr) {
  if (!s_engine) return nullptr;
  QQmlComponent comp(s_engine);
  comp.setData(k_qmlPrefix + body, QUrl());
  if (comp.status() != QQmlComponent::Ready) {
    qWarning() << "qtquick_extensions: QML error:" << comp.errorString();
    return nullptr;
  }
  QObject *obj = comp.create();
  QQuickItem *item = qobject_cast<QQuickItem *>(obj);
  if (!item) { delete obj; return nullptr; }
  if (parent) item->setParentItem(parent);
  return item;
}

void lbm_qtquick_extensions_set_root(QQuickItem *root, QQmlEngine *engine) {
  s_engine = engine;
  s_root   = root;

  // root is typically a plain Item. Create a ColumnLayout that fills it so that
  // children added via LBM extensions are laid out vertically. Layout.fillWidth
  // and Layout.fillHeight only work when the direct parent is a Qt Quick Layout.
  QQuickItem *col = createQmlItem(
    "ColumnLayout {\n"
    "  anchors.fill: parent\n"
    "  spacing: 2\n"
    "}",
    root);

  QQuickItem *container = col ? col : root;
  s_rootHandle = s_nextHandle++;
  s_items.insert(s_rootHandle, container);
  s_types.insert(s_rootHandle, LbmQtWidgetType::Container);
}

// ////////////////////////////////////////////////////////////
// Attribute parsing (mirrors qt_extensions.cpp)

struct QtQuickWidgetAttrs {
  int     max_width  = -1;
  int     min_width  = -1;
  int     max_height = -1;
  int     min_height = -1;
  int     pos_x      = -1;
  int     pos_y      = -1;
  int     visible    = -1;
  int     enabled    = -1;
  QColor  bg_color;         // invalid = not set
  QColor  fg_color;         // invalid = not set
  bool    scrollV    = false;
  bool    scrollH    = false;
};

static QString lbm_dec_qstr(lbm_value v) {
  lbm_array_header_t *arr = lbm_dec_array_r(v);
  if (!arr) return QString();
  return QString::fromUtf8((const char *)arr->data);
}

static lbm_value lbm_enc_qstr(const QString &s) {
  QByteArray ba = s.toUtf8();
  lbm_value arr;
  if (!lbm_heap_allocate_array(&arr, (lbm_uint)(ba.size() + 1))) return ENC_SYM_MERROR;
  lbm_array_header_t *ahdr = lbm_dec_array_rw(arr);
  if (!ahdr) return ENC_SYM_MERROR;
  memcpy(ahdr->data, ba.constData(), (size_t)ba.size());
  ((uint8_t *)ahdr->data)[ba.size()] = 0;
  return arr;
}

static QColor lbmValueToColor(lbm_value v) {
  if (lbm_is_array_r(v)) {
    return QColor(lbm_dec_qstr(v));
  } else if (lbm_is_number(v)) {
    uint32_t rgb = (uint32_t)lbm_dec_as_u32(v);
    return QColor((int)(rgb >> 16) & 0xff, (int)(rgb >> 8) & 0xff, (int)rgb & 0xff);
  }
  return QColor();
}

static QtQuickWidgetAttrs parseAttrs(lbm_value *args, lbm_uint argn, lbm_uint start) {
  QtQuickWidgetAttrs a;
  for (lbm_uint i = start; i < argn; i++) {
    lbm_value attr = args[i];
    const char *key = nullptr;
    lbm_value   val = ENC_SYM_NIL;

    if (lbm_is_symbol(attr)) {
      key = lbm_get_name_by_symbol(lbm_dec_sym(attr));
      if (key) {
        if      (strcmp(key, "scroll")   == 0) { a.scrollV = true; a.scrollH = true; }
        else if (strcmp(key, "scroll-v") == 0)   a.scrollV = true;
        else if (strcmp(key, "scroll-h") == 0)   a.scrollH = true;
      }
    } else if (lbm_is_cons(attr)) {
      lbm_value head = lbm_car(attr);
      if (lbm_is_symbol(head)) {
        key = lbm_get_name_by_symbol(lbm_dec_sym(head));
        val = lbm_cdr(attr);
      }
    }
    if (!key) continue;

    if      (strcmp(key, "max-width")  == 0 && lbm_is_cons(val))
      a.max_width  = (int)lbm_dec_as_i32(lbm_car(val));
    else if (strcmp(key, "min-width")  == 0 && lbm_is_cons(val))
      a.min_width  = (int)lbm_dec_as_i32(lbm_car(val));
    else if (strcmp(key, "max-height") == 0 && lbm_is_cons(val))
      a.max_height = (int)lbm_dec_as_i32(lbm_car(val));
    else if (strcmp(key, "min-height") == 0 && lbm_is_cons(val))
      a.min_height = (int)lbm_dec_as_i32(lbm_car(val));
    else if (strcmp(key, "pos-x") == 0 && lbm_is_cons(val))
      a.pos_x = (int)lbm_dec_as_i32(lbm_car(val));
    else if (strcmp(key, "pos-y") == 0 && lbm_is_cons(val))
      a.pos_y = (int)lbm_dec_as_i32(lbm_car(val));
    else if (strcmp(key, "visible") == 0 && lbm_is_cons(val))
      a.visible = lbm_is_symbol_nil(lbm_car(val)) ? 0 : 1;
    else if (strcmp(key, "enabled") == 0 && lbm_is_cons(val))
      a.enabled = lbm_is_symbol_nil(lbm_car(val)) ? 0 : 1;
    else if (strcmp(key, "bg-color") == 0 && lbm_is_cons(val)) {
      QColor c = lbmValueToColor(lbm_car(val));
      if (c.isValid()) a.bg_color = c;
    } else if (strcmp(key, "fg-color") == 0 && lbm_is_cons(val)) {
      QColor c = lbmValueToColor(lbm_car(val));
      if (c.isValid()) a.fg_color = c;
    }
  }
  return a;
}

static void applyAttrs(QQuickItem *item, const QtQuickWidgetAttrs &a) {
  if (a.max_width  >= 0) item->setProperty("Layout.maximumWidth",  a.max_width);
  if (a.min_width  >= 0) item->setProperty("Layout.minimumWidth",  a.min_width);
  if (a.max_height >= 0) item->setProperty("Layout.maximumHeight", a.max_height);
  if (a.min_height >= 0) item->setProperty("Layout.minimumHeight", a.min_height);
  if (a.visible   >= 0) item->setVisible(a.visible != 0);
  if (a.enabled   >= 0) item->setEnabled(a.enabled != 0);
  if (a.bg_color.isValid()) { item->setProperty("_lbmBgColor", a.bg_color); item->setProperty("_hasCustomBg", true); }
  if (a.fg_color.isValid()) { item->setProperty("_lbmFgColor", a.fg_color); item->setProperty("_hasCustomFg", true); }
}

// Set Layout attached properties so the child fills available space in its parent layout.
static void setLayoutFill(QQuickItem *item, bool fillW, bool fillH) {
  QQmlProperty::write(item, "Layout.fillWidth",  fillW);
  QQmlProperty::write(item, "Layout.fillHeight", fillH);
}

static void addToLayout(QQuickItem *layout, QQuickItem *child,
                        const QtQuickWidgetAttrs &attrs, bool fillW = true, bool fillH = false) {
  if (attrs.pos_x >= 0 && attrs.pos_y >= 0) {
    QQmlProperty::write(child, "Layout.row",    attrs.pos_y);
    QQmlProperty::write(child, "Layout.column", attrs.pos_x);
  }
  setLayoutFill(child, fillW, fillH);
  child->setParentItem(layout);
}

static int registerItem(QQuickItem *item, LbmQtWidgetType type) {
  int handle = s_nextHandle++;
  s_items.insert(handle, item);
  s_types.insert(handle, type);
  return handle;
}

// ////////////////////////////////////////////////////////////
// Image handling (same as qt_extensions.cpp)

static QImage imageFromBuffer(const image_buffer_t *img) {
  int w = (int)img->width;
  int h = (int)img->height;
  switch (img->fmt) {
  case rgb888:
    return QImage(img->data, w, h, w * 3, QImage::Format_RGB888).copy();
  case rgb565:
    return QImage(img->data, w, h, w * 2, QImage::Format_RGB16).copy();
  case rgb332: {
    QImage out(w, h, QImage::Format_RGB32);
    for (int y = 0; y < h; y++)
      for (int x = 0; x < w; x++) {
        uint8_t p = img->data[y * w + x];
        uint8_t r = (uint8_t)(((p >> 5) & 0x7u) * 255u / 7u);
        uint8_t g = (uint8_t)(((p >> 2) & 0x7u) * 255u / 7u);
        uint8_t b = (uint8_t)((p        & 0x3u) * 255u / 3u);
        out.setPixel(x, y, qRgb(r, g, b));
      }
    return out;
  }
  default: return QImage();
  }
}

static bool qt_render_image(image_buffer_t *img, uint16_t x, uint16_t y, color_t *colors) {
  (void)colors;
  if (!s_activeDisplay) return false;
  QImage qimg = imageFromBuffer(img);
  if (qimg.isNull()) return false;
  QLbmQuickDisplayItem *pane = s_activeDisplay;
  int ix = (int)x, iy = (int)y;
  QMetaObject::invokeMethod(pane, [pane, ix, iy, qimg]() {
    pane->setImageAt(ix, iy, qimg);
  }, Qt::QueuedConnection);
  return true;
}

// ////////////////////////////////////////////////////////////
// Widget creation helpers

static int createDisplay(QQuickItem *container, int w, int h,
                         const QtQuickWidgetAttrs &attrs = QtQuickWidgetAttrs()) {
  int handle = -1;
  QMetaObject::invokeMethod(container, [container, w, h, attrs, &handle]() {
    auto *display = new QLbmQuickDisplayItem(w, h);
    applyAttrs(display, attrs);
    handle = registerItem(display, LbmQtWidgetType::Display);
    addToLayout(container, display, attrs, false, false);
  }, Qt::BlockingQueuedConnection);

  if (handle < 0) return -1;
  s_activeDisplay = qobject_cast<QLbmQuickDisplayItem*>(s_items.value(handle));
  lbm_display_extensions_set_callbacks(qt_render_image, nullptr, nullptr);
  return handle;
}

static int createContainer(QQuickItem *parent, const QByteArray &layoutQml,
                           const QtQuickWidgetAttrs &attrs = QtQuickWidgetAttrs()) {
  int handle = -1;
  QMetaObject::invokeMethod(parent, [parent, layoutQml, attrs, &handle]() {
    if (attrs.scrollV || attrs.scrollH) {
      // Create inner layout (no parent yet — it lives inside the Flickable,
      // not a Qt Quick Layout, so Layout.fill* attached props don't apply).
      QQuickItem *inner = createQmlItem(layoutQml + "\n", nullptr);
      if (!inner) return;

      QByteArray svQml = "ScrollView {\n"
                         "  Layout.fillWidth: true\n"
                         "  Layout.fillHeight: true\n"
                         "  clip: true\n";
      if (!attrs.scrollH) svQml += "  ScrollBar.horizontal.policy: ScrollBar.AlwaysOff\n";
      if (!attrs.scrollV) svQml += "  ScrollBar.vertical.policy: ScrollBar.AlwaysOff\n";
      svQml += "}";

      QQuickItem *sv = createQmlItem(svQml, parent);
      if (!sv) { delete inner; return; }
      applyAttrs(sv, attrs);
      addToLayout(parent, sv, attrs, true, true);

      // ScrollView.contentItem is the Flickable (viewport).
      // Flickable.contentItem is the item that actually gets translated on scroll.
      // Children must be parented to the latter; width tracks the former.
      QQuickItem *flickable = sv->property("contentItem").value<QQuickItem*>();
      QQuickItem *scrollContent = flickable
                                  ? flickable->property("contentItem").value<QQuickItem*>()
                                  : nullptr;
      inner->setParentItem(scrollContent ? scrollContent : sv);

      // Keep inner's width pinned to the viewport (Flickable) width.
      QQuickItem *viewport = flickable ? flickable : sv;
      QObject::connect(viewport, &QQuickItem::widthChanged, inner, [viewport, inner]() {
        inner->setWidth(viewport->width());
      });
      inner->setWidth(viewport->width());

      // Drive ScrollView.contentHeight from the inner layout's implicitHeight
      // so the scrollbar appears as soon as content overflows the viewport.
      auto syncHeight = [sv, inner]() {
        sv->setProperty("contentHeight", inner->implicitHeight());
      };
      QObject::connect(inner, &QQuickItem::implicitHeightChanged, sv, syncHeight);
      syncHeight();

      handle = registerItem(inner, LbmQtWidgetType::Container);
    } else {
      QQuickItem *layout = createQmlItem(layoutQml, parent);
      if (!layout) return;
      applyAttrs(layout, attrs);
      addToLayout(parent, layout, attrs, true, true);
      handle = registerItem(layout, LbmQtWidgetType::Container);
    }
  }, Qt::BlockingQueuedConnection);
  return handle;
}

static int createButton(QQuickItem *container, const QString &label,
                        const QtQuickWidgetAttrs &attrs = QtQuickWidgetAttrs()) {
  int handle = -1;
  QMetaObject::invokeMethod(container, [container, label, attrs, &handle]() {
    QQuickItem *btn = createQmlItem(
      "Button {\n"
      "  Layout.fillWidth: true\n"
      "  property bool  _hasCustomBg: false\n"
      "  property bool  _hasCustomFg: false\n"
      "  property color _lbmBgColor:  \"transparent\"\n"
      "  property color _lbmFgColor:  \"black\"\n"
      "  background: Rectangle {\n"
      "    color: {\n"
      "      var base = parent._hasCustomBg ? parent._lbmBgColor : parent.palette.button\n"
      "      return parent.pressed ? Qt.darker(base, 1.1) : base\n"
      "    }\n"
      "    radius: 2\n"
      "    border.color: parent.pressed ? parent.palette.dark : parent.palette.mid\n"
      "    border.width: 1\n"
      "  }\n"
      "  contentItem: Text {\n"
      "    text:                parent.text\n"
      "    color:               parent._hasCustomFg ? parent._lbmFgColor : parent.palette.buttonText\n"
      "    horizontalAlignment: Text.AlignHCenter\n"
      "    verticalAlignment:   Text.AlignVCenter\n"
      "    font:                parent.font\n"
      "    elide:               Text.ElideRight\n"
      "  }\n"
      "}",
      nullptr);
    if (!btn) return;
    btn->setProperty("text", label);
    applyAttrs(btn, attrs);
    handle = registerItem(btn, LbmQtWidgetType::Button);
    addToLayout(container, btn, attrs, true, false);

    auto *adapter = new LbmSignalAdapter(handle, LbmSignalAdapter::WidgetType::Button, btn, btn);
    QObject::connect(btn, SIGNAL(clicked()), adapter, SLOT(onClicked()));
  }, Qt::BlockingQueuedConnection);
  return handle;
}

static int createCheckbox(QQuickItem *container, const QString &label,
                          const QtQuickWidgetAttrs &attrs = QtQuickWidgetAttrs()) {
  int handle = -1;
  QMetaObject::invokeMethod(container, [container, label, attrs, &handle]() {
    QQuickItem *cb = createQmlItem("CheckBox { Layout.fillWidth: true }", nullptr);
    if (!cb) return;
    cb->setProperty("text", label);
    applyAttrs(cb, attrs);
    handle = registerItem(cb, LbmQtWidgetType::Checkbox);
    addToLayout(container, cb, attrs, true, false);

    auto *adapter = new LbmSignalAdapter(handle, LbmSignalAdapter::WidgetType::Checkbox, cb, cb);
    QObject::connect(cb, SIGNAL(checkedChanged()), adapter, SLOT(onCheckedChanged()));
  }, Qt::BlockingQueuedConnection);
  return handle;
}

static int createRadio(QQuickItem *container, const QString &label,
                       const QtQuickWidgetAttrs &attrs = QtQuickWidgetAttrs()) {
  int handle = -1;
  QMetaObject::invokeMethod(container, [container, label, attrs, &handle]() {
    QQuickItem *rb = createQmlItem("RadioButton { Layout.fillWidth: true }", nullptr);
    if (!rb) return;
    rb->setProperty("text", label);
    applyAttrs(rb, attrs);
    handle = registerItem(rb, LbmQtWidgetType::Radio);
    addToLayout(container, rb, attrs, true, false);

    auto *adapter = new LbmSignalAdapter(handle, LbmSignalAdapter::WidgetType::Radio, rb, rb);
    QObject::connect(rb, SIGNAL(checkedChanged()), adapter, SLOT(onCheckedChanged()));
  }, Qt::BlockingQueuedConnection);
  return handle;
}

static int createSpinboxI(QQuickItem *container, int min, int max,
                          const QtQuickWidgetAttrs &attrs = QtQuickWidgetAttrs()) {
  int handle = -1;
  QMetaObject::invokeMethod(container, [container, min, max, attrs, &handle]() {
    QQuickItem *sb = createQmlItem("SpinBox { Layout.fillWidth: true }", nullptr);
    if (!sb) return;
    sb->setProperty("from", min);
    sb->setProperty("to",   max);
    applyAttrs(sb, attrs);
    handle = registerItem(sb, LbmQtWidgetType::SpinboxI);
    addToLayout(container, sb, attrs, true, false);

    auto *adapter = new LbmSignalAdapter(handle, LbmSignalAdapter::WidgetType::SpinboxI, sb, sb);
    QObject::connect(sb, SIGNAL(valueModified()), adapter, SLOT(onValueModified()));
  }, Qt::BlockingQueuedConnection);
  return handle;
}

static int createSpinboxF(QQuickItem *container, double min, double max, double step,
                          const QtQuickWidgetAttrs &attrs = QtQuickWidgetAttrs()) {
  // Qt Quick's SpinBox is integer-only. We scale by 1/step to simulate float.
  // The LBM API returns float by dividing back.
  int handle = -1;
  QMetaObject::invokeMethod(container, [container, min, max, step, attrs, &handle]() {
    // Scale factor: represent floats as integers internally.
    // e.g. step=0.1 → scale=10, from=round(min*10), to=round(max*10)
    double scale = (step > 0.0) ? (1.0 / step) : 100.0;
    int iMin  = (int)qRound(min  * scale);
    int iMax  = (int)qRound(max  * scale);

    // Build QML SpinBox with a custom textFromValue that shows decimals.
    QByteArray qml = QByteArray(
      "SpinBox {\n"
      "  Layout.fillWidth: true\n"
      "  stepSize: 1\n"
      "  property real _scale: ") + QByteArray::number(scale) + "\n"
      "  property real _lbmSliderScale: _scale\n"
      "  textFromValue: function(v, locale) {\n"
      "    return (v / _scale).toLocaleString(locale, 'f', 3)\n"
      "  }\n"
      "  valueFromText: function(t, locale) {\n"
      "    return Math.round(Number.fromLocaleString(locale, t) * _scale)\n"
      "  }\n"
      "}";

    QQuickItem *sb = createQmlItem(qml, nullptr);
    if (!sb) return;
    sb->setProperty("from", iMin);
    sb->setProperty("to",   iMax);
    sb->setProperty("value", (int)qRound(min * scale));
    sb->setProperty("_scale", scale);
    applyAttrs(sb, attrs);
    handle = registerItem(sb, LbmQtWidgetType::SpinboxF);
    addToLayout(container, sb, attrs, true, false);

    auto *adapter = new LbmSignalAdapter(handle, LbmSignalAdapter::WidgetType::SpinboxF, sb, sb);
    QObject::connect(sb, SIGNAL(valueModified()), adapter, SLOT(onValueModified()));
  }, Qt::BlockingQueuedConnection);
  return handle;
}

static int createTextfield(QQuickItem *container, const QString &placeholder,
                           const QtQuickWidgetAttrs &attrs = QtQuickWidgetAttrs()) {
  int handle = -1;
  QMetaObject::invokeMethod(container, [container, placeholder, attrs, &handle]() {
    QQuickItem *tf = createQmlItem("TextField { Layout.fillWidth: true }", nullptr);
    if (!tf) return;
    tf->setProperty("placeholderText", placeholder);
    applyAttrs(tf, attrs);
    handle = registerItem(tf, LbmQtWidgetType::Textfield);
    addToLayout(container, tf, attrs, true, false);

    auto *adapter = new LbmSignalAdapter(handle, LbmSignalAdapter::WidgetType::Textfield, tf, tf);
    QObject::connect(tf, SIGNAL(editingFinished()), adapter, SLOT(onEditingFinished()));
  }, Qt::BlockingQueuedConnection);
  return handle;
}

static int createLabel(QQuickItem *container, const QString &text,
                       const QtQuickWidgetAttrs &attrs = QtQuickWidgetAttrs()) {
  int handle = -1;
  QMetaObject::invokeMethod(container, [container, text, attrs, &handle]() {
    QQuickItem *lbl = createQmlItem("Label { Layout.fillWidth: true }", nullptr);
    if (!lbl) return;
    lbl->setProperty("text", text);
    applyAttrs(lbl, attrs);
    handle = registerItem(lbl, LbmQtWidgetType::Label);
    addToLayout(container, lbl, attrs, true, false);
  }, Qt::BlockingQueuedConnection);
  return handle;
}

static int createSlider(QQuickItem *container, int min, int max,
                        bool vertical,
                        const QtQuickWidgetAttrs &attrs = QtQuickWidgetAttrs()) {
  int handle = -1;
  QMetaObject::invokeMethod(container, [container, min, max, vertical, attrs, &handle]() {
    QByteArray qml = vertical
      ? "Slider { orientation: Qt.Vertical; Layout.fillHeight: true }"
      : "Slider { orientation: Qt.Horizontal; Layout.fillWidth: true }";
    QQuickItem *sl = createQmlItem(qml, nullptr);
    if (!sl) return;

    // Slider value is [0,1] by default. Map to [min, max] via stepSize.
    // Expose integer scale via property for the adapter.
    double scale = (max > min) ? (double)(max - min) : 1.0;
    sl->setProperty("from",  0.0);
    sl->setProperty("to",    1.0);
    sl->setProperty("stepSize", 1.0 / scale);
    sl->setProperty("_lbmSliderScale", scale);
    sl->setProperty("_lbmSliderMin",   min);
    applyAttrs(sl, attrs);
    handle = registerItem(sl, LbmQtWidgetType::Slider);
    addToLayout(container, sl, attrs, !vertical, vertical);

    auto *adapter = new LbmSignalAdapter(handle, LbmSignalAdapter::WidgetType::Slider, sl, sl);
    QObject::connect(sl, SIGNAL(moved()), adapter, SLOT(onMoved()));
  }, Qt::BlockingQueuedConnection);
  return handle;
}

static QStringList lbmListToStringList(lbm_value list) {
  QStringList result;
  for (lbm_value c = list; lbm_is_cons(c); c = lbm_cdr(c)) {
    lbm_value item = lbm_car(c);
    if (lbm_is_array_r(item))
      result.append(lbm_dec_qstr(item));
  }
  return result;
}

static int createCombo(QQuickItem *container, const QStringList &items,
                       const QtQuickWidgetAttrs &attrs = QtQuickWidgetAttrs()) {
  int handle = -1;
  QMetaObject::invokeMethod(container, [container, items, attrs, &handle]() {
    QQuickItem *cb = createQmlItem("ComboBox { Layout.fillWidth: true }", nullptr);
    if (!cb) return;
    cb->setProperty("model", items);
    applyAttrs(cb, attrs);
    handle = registerItem(cb, LbmQtWidgetType::Combo);
    addToLayout(container, cb, attrs, true, false);

    auto *adapter = new LbmSignalAdapter(handle, LbmSignalAdapter::WidgetType::Combo, cb, cb);
    QObject::connect(cb, SIGNAL(activated(int)), adapter, SLOT(onActivated(int)));
  }, Qt::BlockingQueuedConnection);
  return handle;
}

static int createStretch(QQuickItem *container, bool horizontal,
                         const QtQuickWidgetAttrs &attrs = QtQuickWidgetAttrs()) {
  int handle = -1;
  QMetaObject::invokeMethod(container, [container, horizontal, attrs, &handle]() {
    QQuickItem *stretch = createQmlItem("Item {}", nullptr);
    if (!stretch) return;
    QQmlProperty::write(stretch, "Layout.fillWidth",  horizontal);
    QQmlProperty::write(stretch, "Layout.fillHeight", !horizontal);
    applyAttrs(stretch, attrs);
    handle = registerItem(stretch, LbmQtWidgetType::Stretch);
    addToLayout(container, stretch, attrs, horizontal, !horizontal);
  }, Qt::BlockingQueuedConnection);
  return handle;
}

// ////////////////////////////////////////////////////////////
// Plot signal adapter — connects QCustomPlot signals to LBM events.
//
// Events sent to LispBM:
//   (plot-click        handle graph-name data-index key value)
//   (plot-dbl-click    handle graph-name data-index key value)
//   (plot-mouse-move   handle data-x data-y)         — only when m_mouseMove enabled
//   (plot-range-done   handle x-lo x-hi y-lo y-hi)  — after user finishes pan/zoom
//
// The adapter lives as a child of the QLbmPlotItem so it is destroyed with it.

class LbmPlotAdapter : public QObject {
  Q_OBJECT
public:
  LbmPlotAdapter(int handle, QLbmPlotItem *item, bool sendMouseMove, QObject *parent = nullptr)
    : QObject(parent), m_handle(handle), m_item(item), m_sendMouseMove(sendMouseMove) {

    QCustomPlot *qcp = item->plot();

    // Repaint is handled by afterReplot→update() wired in QLbmPlotItem constructor,
    // so we only need to wire LBM-facing events here.

    connect(qcp, &QCustomPlot::plottableClick,
            this, &LbmPlotAdapter::onPlottableClick);
    connect(qcp, &QCustomPlot::plottableDoubleClick,
            this, &LbmPlotAdapter::onPlottableDblClick);
    connect(qcp, &QCustomPlot::mouseRelease,
            this, &LbmPlotAdapter::onMouseRelease);
    if (m_sendMouseMove)
      connect(qcp, &QCustomPlot::mouseMove,
              this, &LbmPlotAdapter::onMouseMove);
  }

  void setMouseMove(bool enabled) {
    QCustomPlot *qcp = m_item->plot();
    if (enabled && !m_sendMouseMove) {
      connect(qcp, &QCustomPlot::mouseMove, this, &LbmPlotAdapter::onMouseMove);
    } else if (!enabled && m_sendMouseMove) {
      disconnect(qcp, &QCustomPlot::mouseMove, this, &LbmPlotAdapter::onMouseMove);
    }
    m_sendMouseMove = enabled;
  }

public slots:
  void onPlottableClick(QCPAbstractPlottable *plottable, int dataIndex, QMouseEvent *) {
    sendPlottableEvent("plot-click", plottable, dataIndex);
  }

  void onPlottableDblClick(QCPAbstractPlottable *plottable, int dataIndex, QMouseEvent *) {
    sendPlottableEvent("plot-dbl-click", plottable, dataIndex);
  }

  void onMouseRelease(QMouseEvent *) {
    QCustomPlot *qcp = m_item->plot();
    QCPRange xr = qcp->xAxis->range();
    QCPRange yr = qcp->yAxis->range();
    send_event(QLbmValue::fromList({
      QLbmValue::fromSymbol("plot-range-done"),
      QLbmValue::fromI(m_handle),
      QLbmValue::fromDouble(xr.lower),
      QLbmValue::fromDouble(xr.upper),
      QLbmValue::fromDouble(yr.lower),
      QLbmValue::fromDouble(yr.upper)
    }));
  }

  void onMouseMove(QMouseEvent *event) {
    QCustomPlot *qcp = m_item->plot();
    double x = qcp->xAxis->pixelToCoord(event->pos().x());
    double y = qcp->yAxis->pixelToCoord(event->pos().y());
    send_event(QLbmValue::fromList({
      QLbmValue::fromSymbol("plot-mouse-move"),
      QLbmValue::fromI(m_handle),
      QLbmValue::fromDouble(x),
      QLbmValue::fromDouble(y)
    }));
  }

private:
  void sendPlottableEvent(const char *sym, QCPAbstractPlottable *plottable, int dataIndex) {
    QCPGraph *graph = qobject_cast<QCPGraph*>(plottable);
    double key = 0.0, value = 0.0;
    if (graph) {
      auto it = graph->data()->at(dataIndex);
      if (it != graph->data()->constEnd()) {
        key   = it->key;
        value = it->value;
      }
    }
    QByteArray nameBytes = plottable->name().toUtf8();
    nameBytes.append('\0');
    send_event(QLbmValue::fromList({
      QLbmValue::fromSymbol(sym),
      QLbmValue::fromI(m_handle),
      QLbmValue::fromByteArray(nameBytes),
      QLbmValue::fromI(dataIndex),
      QLbmValue::fromDouble(key),
      QLbmValue::fromDouble(value)
    }));
  }

  int           m_handle;
  QLbmPlotItem *m_item;
  bool          m_sendMouseMove;
};

// ////////////////////////////////////////////////////////////
// Registry helpers

static QQuickItem *getContainer(int handle) {
  auto it = s_types.find(handle);
  if (it == s_types.end() || it.value() != LbmQtWidgetType::Container) return nullptr;
  return s_items.value(handle, nullptr);
}

static QLbmPlotItem *getPlot(int handle) {
  if (s_types.value(handle) != LbmQtWidgetType::Plot) return nullptr;
  return qobject_cast<QLbmPlotItem*>(s_items.value(handle, nullptr));
}

static int createPlot(QQuickItem *container, bool sendMouseMove,
                      const QtQuickWidgetAttrs &attrs = QtQuickWidgetAttrs()) {
  int handle = -1;
  QMetaObject::invokeMethod(container, [container, sendMouseMove, attrs, &handle]() {
    auto *plot = new QLbmPlotItem();
    applyAttrs(plot, attrs);
    handle = registerItem(plot, LbmQtWidgetType::Plot);
    // Fill available space in the parent layout by default.
    addToLayout(container, plot, attrs, true, true);
    // Wire up QCustomPlot signals → LBM events.
    auto *adapter = new LbmPlotAdapter(handle, plot, sendMouseMove, plot);
    s_plotAdapters.insert(handle, adapter);
  }, Qt::BlockingQueuedConnection);
  return handle;
}

static void removeFromRegistry(QQuickItem *w) {
  QList<int> toRemove;
  for (auto it = s_items.begin(); it != s_items.end(); ++it) {
    QQuickItem *candidate = it.value();
    if (candidate == w || w->isAncestorOf(candidate))
      toRemove.append(it.key());
  }
  for (int h : toRemove) {
    s_items.remove(h);
    s_types.remove(h);
    s_plotAdapters.remove(h);
  }
  if (s_activeDisplay && (s_activeDisplay == w || w->isAncestorOf(s_activeDisplay)))
    s_activeDisplay = nullptr;
}

// ////////////////////////////////////////////////////////////
// LBM extensions

static lbm_value ext_qt_root(lbm_value *args, lbm_uint argn) {
  (void)args; (void)argn;
  if (s_rootHandle < 0) {
    lbm_set_error_reason("qt-root: no root item registered");
    return ENC_SYM_EERROR;
  }
  return lbm_enc_i(s_rootHandle);
}

static lbm_value ext_qt_set_display(lbm_value *args, lbm_uint argn) {
  if (argn < 1 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;
  int handle = (int)lbm_dec_as_i32(args[0]);
  auto *d = qobject_cast<QLbmQuickDisplayItem *>(s_items.value(handle, nullptr));
  if (!d) {
    lbm_set_error_reason("qt-set-display: invalid handle");
    return ENC_SYM_EERROR;
  }
  s_activeDisplay = d;
  lbm_display_extensions_set_callbacks(qt_render_image, nullptr, nullptr);
  return ENC_SYM_TRUE;
}

static lbm_value ext_qt_set_label(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_number(args[0]) || !lbm_is_array_r(args[1]))
    return ENC_SYM_TERROR;
  int handle = (int)lbm_dec_as_i32(args[0]);
  QQuickItem *item = s_items.value(handle, nullptr);
  if (!item) {
    lbm_set_error_reason("qt-set-label: invalid handle");
    return ENC_SYM_EERROR;
  }
  QString label = lbm_dec_qstr(args[1]);
  QMetaObject::invokeMethod(item, [item, label]() { item->setProperty("text", label); },
                            Qt::QueuedConnection);
  return ENC_SYM_TRUE;
}

static lbm_value ext_qt_display(lbm_value *args, lbm_uint argn) {
  if (argn < 3 || !lbm_is_number(args[0]) ||
      !lbm_is_number(args[1]) || !lbm_is_number(args[2]))
    return ENC_SYM_TERROR;
  QQuickItem *container = getContainer((int)lbm_dec_as_i32(args[0]));
  if (!container) {
    lbm_set_error_reason("qt-display: invalid container handle");
    return ENC_SYM_EERROR;
  }
  int w = (int)lbm_dec_as_i32(args[1]);
  int h = (int)lbm_dec_as_i32(args[2]);
  if (w <= 0 || h <= 0) return ENC_SYM_TERROR;
  int handle = createDisplay(container, w, h, parseAttrs(args, argn, 3));
  return handle >= 0 ? lbm_enc_i(handle) : ENC_SYM_EERROR;
}

static lbm_value ext_qt_button(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_number(args[0]) || !lbm_is_array_r(args[1]))
    return ENC_SYM_TERROR;
  QQuickItem *container = getContainer((int)lbm_dec_as_i32(args[0]));
  if (!container) {
    lbm_set_error_reason("qt-button: invalid container handle");
    return ENC_SYM_EERROR;
  }
  int handle = createButton(container, lbm_dec_qstr(args[1]), parseAttrs(args, argn, 2));
  return handle >= 0 ? lbm_enc_i(handle) : ENC_SYM_EERROR;
}

static lbm_value ext_qt_container(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_number(args[0]) || !lbm_is_symbol(args[1]))
    return ENC_SYM_TERROR;
  QQuickItem *parent = getContainer((int)lbm_dec_as_i32(args[0]));
  if (!parent) {
    lbm_set_error_reason("qt-container: invalid container handle");
    return ENC_SYM_EERROR;
  }
  const char *layoutName = lbm_get_name_by_symbol(lbm_dec_sym(args[1]));
  QByteArray layoutQml;
  if      (layoutName && strcmp(layoutName, "hbox") == 0) layoutQml = "RowLayout {}";
  else if (layoutName && strcmp(layoutName, "grid") == 0) layoutQml = "GridLayout {}";
  else                                                     layoutQml = "ColumnLayout {}";

  int handle = createContainer(parent, layoutQml, parseAttrs(args, argn, 2));
  return handle >= 0 ? lbm_enc_i(handle) : ENC_SYM_EERROR;
}

static lbm_value ext_qt_stretch(lbm_value *args, lbm_uint argn) {
  if (argn < 1 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;
  QQuickItem *container = getContainer((int)lbm_dec_as_i32(args[0]));
  if (!container) {
    lbm_set_error_reason("qt-stretch: invalid container handle");
    return ENC_SYM_EERROR;
  }
  bool horizontal = false;
  lbm_uint attr_start = 1;
  if (argn >= 2 && lbm_is_symbol(args[1])) {
    const char *name = lbm_get_name_by_symbol(lbm_dec_sym(args[1]));
    if (name && strcmp(name, "horizontal") == 0) { horizontal = true; attr_start = 2; }
  }
  int handle = createStretch(container, horizontal, parseAttrs(args, argn, attr_start));
  return handle >= 0 ? lbm_enc_i(handle) : ENC_SYM_EERROR;
}

static lbm_value ext_qt_remove(lbm_value *args, lbm_uint argn) {
  if (argn < 1 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;
  int handle = (int)lbm_dec_as_i32(args[0]);
  if (handle == s_rootHandle) {
    lbm_set_error_reason("qt-remove: cannot remove root item");
    return ENC_SYM_EERROR;
  }
  QQuickItem *w = s_items.value(handle, nullptr);
  if (!w) {
    lbm_set_error_reason("qt-remove: invalid handle");
    return ENC_SYM_EERROR;
  }
  removeFromRegistry(w);
  QMetaObject::invokeMethod(w, [w]() { w->deleteLater(); }, Qt::BlockingQueuedConnection);
  return ENC_SYM_TRUE;
}

static lbm_value ext_qt_set_visible(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;
  QQuickItem *w = s_items.value((int)lbm_dec_as_i32(args[0]), nullptr);
  if (!w) { lbm_set_error_reason("qt-set-visible: invalid handle"); return ENC_SYM_EERROR; }
  bool v = !lbm_is_symbol_nil(args[1]);
  QMetaObject::invokeMethod(w, [w, v]() { w->setVisible(v); }, Qt::QueuedConnection);
  return ENC_SYM_TRUE;
}

static lbm_value ext_qt_get_visible(lbm_value *args, lbm_uint argn) {
  if (argn < 1 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;
  QQuickItem *w = s_items.value((int)lbm_dec_as_i32(args[0]), nullptr);
  if (!w) { lbm_set_error_reason("qt-get-visible: invalid handle"); return ENC_SYM_EERROR; }
  bool v = false;
  QMetaObject::invokeMethod(w, [w, &v]() { v = w->isVisible(); }, Qt::BlockingQueuedConnection);
  return v ? ENC_SYM_TRUE : ENC_SYM_NIL;
}

static lbm_value ext_qt_set_enabled(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;
  QQuickItem *w = s_items.value((int)lbm_dec_as_i32(args[0]), nullptr);
  if (!w) { lbm_set_error_reason("qt-set-enabled: invalid handle"); return ENC_SYM_EERROR; }
  bool v = !lbm_is_symbol_nil(args[1]);
  QMetaObject::invokeMethod(w, [w, v]() { w->setEnabled(v); }, Qt::QueuedConnection);
  return ENC_SYM_TRUE;
}

static lbm_value ext_qt_get_enabled(lbm_value *args, lbm_uint argn) {
  if (argn < 1 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;
  QQuickItem *w = s_items.value((int)lbm_dec_as_i32(args[0]), nullptr);
  if (!w) { lbm_set_error_reason("qt-get-enabled: invalid handle"); return ENC_SYM_EERROR; }
  bool v = false;
  QMetaObject::invokeMethod(w, [w, &v]() { v = w->isEnabled(); }, Qt::BlockingQueuedConnection);
  return v ? ENC_SYM_TRUE : ENC_SYM_NIL;
}

static lbm_value ext_qt_set_bg_color(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;
  QQuickItem *w = s_items.value((int)lbm_dec_as_i32(args[0]), nullptr);
  if (!w) { lbm_set_error_reason("qt-set-bg-color: invalid handle"); return ENC_SYM_EERROR; }
  if (lbm_is_symbol_nil(args[1])) {
    QMetaObject::invokeMethod(w, [w]() { w->setProperty("_hasCustomBg", false); }, Qt::QueuedConnection);
    return ENC_SYM_TRUE;
  }
  QColor c = lbmValueToColor(args[1]);
  if (!c.isValid()) return ENC_SYM_TERROR;
  QMetaObject::invokeMethod(w, [w, c]() {
    w->setProperty("_lbmBgColor", c);
    w->setProperty("_hasCustomBg", true);
  }, Qt::QueuedConnection);
  return ENC_SYM_TRUE;
}

static lbm_value ext_qt_set_fg_color(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;
  QQuickItem *w = s_items.value((int)lbm_dec_as_i32(args[0]), nullptr);
  if (!w) { lbm_set_error_reason("qt-set-fg-color: invalid handle"); return ENC_SYM_EERROR; }
  if (lbm_is_symbol_nil(args[1])) {
    QMetaObject::invokeMethod(w, [w]() { w->setProperty("_hasCustomFg", false); }, Qt::QueuedConnection);
    return ENC_SYM_TRUE;
  }
  QColor c = lbmValueToColor(args[1]);
  if (!c.isValid()) return ENC_SYM_TERROR;
  QMetaObject::invokeMethod(w, [w, c]() {
    w->setProperty("_lbmFgColor", c);
    w->setProperty("_hasCustomFg", true);
  }, Qt::QueuedConnection);
  return ENC_SYM_TRUE;
}

// Dimension setters share the same pattern — use a macro to reduce repetition.
#define MAKE_DIM_SETTER(fname, lbmname, propname)                               \
static lbm_value fname(lbm_value *args, lbm_uint argn) {                       \
  if (argn < 2 || !lbm_is_number(args[0]) || !lbm_is_number(args[1]))          \
    return ENC_SYM_TERROR;                                                      \
  QQuickItem *w = s_items.value((int)lbm_dec_as_i32(args[0]), nullptr);         \
  if (!w) { lbm_set_error_reason(lbmname ": invalid handle"); return ENC_SYM_EERROR; } \
  int dim = (int)lbm_dec_as_i32(args[1]);                                       \
  QMetaObject::invokeMethod(w, [w, dim]() {                                     \
    QQmlProperty::write(w, propname, dim);                                      \
  }, Qt::QueuedConnection);                                                     \
  return ENC_SYM_TRUE;                                                          \
}

MAKE_DIM_SETTER(ext_qt_set_max_width,  "qt-set-max-width",  "Layout.maximumWidth")
MAKE_DIM_SETTER(ext_qt_set_min_width,  "qt-set-min-width",  "Layout.minimumWidth")
MAKE_DIM_SETTER(ext_qt_set_max_height, "qt-set-max-height", "Layout.maximumHeight")
MAKE_DIM_SETTER(ext_qt_set_min_height, "qt-set-min-height", "Layout.minimumHeight")

static lbm_value ext_qt_checkbox(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_number(args[0]) || !lbm_is_array_r(args[1]))
    return ENC_SYM_TERROR;
  QQuickItem *container = getContainer((int)lbm_dec_as_i32(args[0]));
  if (!container) {
    lbm_set_error_reason("qt-checkbox: invalid container handle");
    return ENC_SYM_EERROR;
  }
  int handle = createCheckbox(container, lbm_dec_qstr(args[1]), parseAttrs(args, argn, 2));
  return handle >= 0 ? lbm_enc_i(handle) : ENC_SYM_EERROR;
}

static lbm_value ext_qt_radio(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_number(args[0]) || !lbm_is_array_r(args[1]))
    return ENC_SYM_TERROR;
  QQuickItem *container = getContainer((int)lbm_dec_as_i32(args[0]));
  if (!container) {
    lbm_set_error_reason("qt-radio: invalid container handle");
    return ENC_SYM_EERROR;
  }
  int handle = createRadio(container, lbm_dec_qstr(args[1]), parseAttrs(args, argn, 2));
  return handle >= 0 ? lbm_enc_i(handle) : ENC_SYM_EERROR;
}

static lbm_value ext_qt_spinbox_i(lbm_value *args, lbm_uint argn) {
  if (argn < 3 || !lbm_is_number(args[0]) ||
      !lbm_is_number(args[1]) || !lbm_is_number(args[2]))
    return ENC_SYM_TERROR;
  QQuickItem *container = getContainer((int)lbm_dec_as_i32(args[0]));
  if (!container) {
    lbm_set_error_reason("qt-spinbox-i: invalid container handle");
    return ENC_SYM_EERROR;
  }
  int handle = createSpinboxI(container,
                              (int)lbm_dec_as_i32(args[1]),
                              (int)lbm_dec_as_i32(args[2]),
                              parseAttrs(args, argn, 3));
  return handle >= 0 ? lbm_enc_i(handle) : ENC_SYM_EERROR;
}

static lbm_value ext_qt_spinbox_f(lbm_value *args, lbm_uint argn) {
  if (argn < 4 || !lbm_is_number(args[0]) || !lbm_is_number(args[1]) ||
      !lbm_is_number(args[2]) || !lbm_is_number(args[3]))
    return ENC_SYM_TERROR;
  QQuickItem *container = getContainer((int)lbm_dec_as_i32(args[0]));
  if (!container) {
    lbm_set_error_reason("qt-spinbox-f: invalid container handle");
    return ENC_SYM_EERROR;
  }
  int handle = createSpinboxF(container,
                              (double)lbm_dec_as_float(args[1]),
                              (double)lbm_dec_as_float(args[2]),
                              (double)lbm_dec_as_float(args[3]),
                              parseAttrs(args, argn, 4));
  return handle >= 0 ? lbm_enc_i(handle) : ENC_SYM_EERROR;
}

static lbm_value ext_qt_textfield(lbm_value *args, lbm_uint argn) {
  if (argn < 1 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;
  QQuickItem *container = getContainer((int)lbm_dec_as_i32(args[0]));
  if (!container) {
    lbm_set_error_reason("qt-textfield: invalid container handle");
    return ENC_SYM_EERROR;
  }
  QString placeholder;
  lbm_uint attr_start = 1;
  if (argn >= 2 && lbm_is_array_r(args[1])) {
    placeholder = lbm_dec_qstr(args[1]);
    attr_start = 2;
  }
  int handle = createTextfield(container, placeholder, parseAttrs(args, argn, attr_start));
  return handle >= 0 ? lbm_enc_i(handle) : ENC_SYM_EERROR;
}

static lbm_value ext_qt_label(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_number(args[0]) || !lbm_is_array_r(args[1]))
    return ENC_SYM_TERROR;
  QQuickItem *container = getContainer((int)lbm_dec_as_i32(args[0]));
  if (!container) {
    lbm_set_error_reason("qt-label: invalid container handle");
    return ENC_SYM_EERROR;
  }
  int handle = createLabel(container, lbm_dec_qstr(args[1]), parseAttrs(args, argn, 2));
  return handle >= 0 ? lbm_enc_i(handle) : ENC_SYM_EERROR;
}

static lbm_value ext_qt_slider(lbm_value *args, lbm_uint argn) {
  if (argn < 3 || !lbm_is_number(args[0]) ||
      !lbm_is_number(args[1]) || !lbm_is_number(args[2]))
    return ENC_SYM_TERROR;
  QQuickItem *container = getContainer((int)lbm_dec_as_i32(args[0]));
  if (!container) {
    lbm_set_error_reason("qt-slider: invalid container handle");
    return ENC_SYM_EERROR;
  }
  bool vertical = false;
  lbm_uint attr_start = 3;
  if (argn >= 4 && lbm_is_symbol(args[3])) {
    const char *name = lbm_get_name_by_symbol(lbm_dec_sym(args[3]));
    if (name && strcmp(name, "vertical") == 0) { vertical = true; attr_start = 4; }
  }
  int handle = createSlider(container,
                            (int)lbm_dec_as_i32(args[1]),
                            (int)lbm_dec_as_i32(args[2]),
                            vertical,
                            parseAttrs(args, argn, attr_start));
  return handle >= 0 ? lbm_enc_i(handle) : ENC_SYM_EERROR;
}

static lbm_value ext_qt_combo(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;
  QQuickItem *container = getContainer((int)lbm_dec_as_i32(args[0]));
  if (!container) {
    lbm_set_error_reason("qt-combo: invalid container handle");
    return ENC_SYM_EERROR;
  }
  int handle = createCombo(container, lbmListToStringList(args[1]),
                           parseAttrs(args, argn, 2));
  return handle >= 0 ? lbm_enc_i(handle) : ENC_SYM_EERROR;
}

static lbm_value ext_qt_get_item(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_number(args[0]) || !lbm_is_number(args[1]))
    return ENC_SYM_TERROR;
  int handle = (int)lbm_dec_as_i32(args[0]);
  int index  = (int)lbm_dec_as_i32(args[1]);
  QQuickItem *cb = s_items.value(handle, nullptr);
  if (!cb || s_types.value(handle) != LbmQtWidgetType::Combo) {
    lbm_set_error_reason("qt-get-item: invalid handle");
    return ENC_SYM_EERROR;
  }
  QString text;
  QMetaObject::invokeMethod(cb, [cb, index, &text]() {
    QVariant model = cb->property("model");
    QStringList sl = model.toStringList();
    if (index >= 0 && index < sl.size()) text = sl.at(index);
  }, Qt::BlockingQueuedConnection);
  return lbm_enc_qstr(text);
}

static lbm_value ext_qt_get_value(lbm_value *args, lbm_uint argn) {
  if (argn < 1 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;
  int handle = (int)lbm_dec_as_i32(args[0]);
  QQuickItem *w = s_items.value(handle, nullptr);
  if (!w) { lbm_set_error_reason("qt-get-value: invalid handle"); return ENC_SYM_EERROR; }

  LbmQtWidgetType type = s_types.value(handle, LbmQtWidgetType::Container);

  if (type == LbmQtWidgetType::Checkbox || type == LbmQtWidgetType::Radio) {
    bool v = false;
    QMetaObject::invokeMethod(w, [w, &v]() { v = w->property("checked").toBool(); },
                              Qt::BlockingQueuedConnection);
    return v ? ENC_SYM_TRUE : ENC_SYM_NIL;
  }
  if (type == LbmQtWidgetType::SpinboxI) {
    int v = 0;
    QMetaObject::invokeMethod(w, [w, &v]() { v = w->property("value").toInt(); },
                              Qt::BlockingQueuedConnection);
    return lbm_enc_i(v);
  }
  if (type == LbmQtWidgetType::SpinboxF) {
    double scale = w->property("_scale").toDouble();
    double v = 0.0;
    QMetaObject::invokeMethod(w, [w, scale, &v]() {
      v = w->property("value").toDouble() / scale;
    }, Qt::BlockingQueuedConnection);
    return lbm_enc_float((float)v);
  }
  if (type == LbmQtWidgetType::Textfield || type == LbmQtWidgetType::Label) {
    QString v;
    QMetaObject::invokeMethod(w, [w, &v]() { v = w->property("text").toString(); },
                              Qt::BlockingQueuedConnection);
    return lbm_enc_qstr(v);
  }
  if (type == LbmQtWidgetType::Slider) {
    double scale = w->property("_lbmSliderScale").toDouble();
    int    vmin  = w->property("_lbmSliderMin").toInt();
    int v = 0;
    QMetaObject::invokeMethod(w, [w, scale, vmin, &v]() {
      v = vmin + (int)qRound(w->property("value").toDouble() * scale);
    }, Qt::BlockingQueuedConnection);
    return lbm_enc_i(v);
  }
  if (type == LbmQtWidgetType::Combo) {
    int v = 0;
    QMetaObject::invokeMethod(w, [w, &v]() { v = w->property("currentIndex").toInt(); },
                              Qt::BlockingQueuedConnection);
    return lbm_enc_i(v);
  }
  lbm_set_error_reason("qt-get-value: widget type has no gettable value");
  return ENC_SYM_EERROR;
}

static lbm_value ext_qt_set_value(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;
  int handle = (int)lbm_dec_as_i32(args[0]);
  QQuickItem *w = s_items.value(handle, nullptr);
  if (!w) { lbm_set_error_reason("qt-set-value: invalid handle"); return ENC_SYM_EERROR; }

  LbmQtWidgetType type = s_types.value(handle, LbmQtWidgetType::Container);

  if (type == LbmQtWidgetType::Checkbox || type == LbmQtWidgetType::Radio) {
    bool v = !lbm_is_symbol_nil(args[1]);
    QMetaObject::invokeMethod(w, [w, v]() { w->setProperty("checked", v); },
                              Qt::QueuedConnection);
    return ENC_SYM_TRUE;
  }
  if (type == LbmQtWidgetType::SpinboxI) {
    if (!lbm_is_number(args[1])) return ENC_SYM_TERROR;
    int v = (int)lbm_dec_as_i32(args[1]);
    QMetaObject::invokeMethod(w, [w, v]() { w->setProperty("value", v); }, Qt::QueuedConnection);
    return ENC_SYM_TRUE;
  }
  if (type == LbmQtWidgetType::SpinboxF) {
    if (!lbm_is_number(args[1])) return ENC_SYM_TERROR;
    double scale = w->property("_scale").toDouble();
    int iv = (int)qRound((double)lbm_dec_as_float(args[1]) * scale);
    QMetaObject::invokeMethod(w, [w, iv]() { w->setProperty("value", iv); }, Qt::QueuedConnection);
    return ENC_SYM_TRUE;
  }
  if (type == LbmQtWidgetType::Textfield || type == LbmQtWidgetType::Label) {
    if (!lbm_is_array_r(args[1])) return ENC_SYM_TERROR;
    QString v = lbm_dec_qstr(args[1]);
    QMetaObject::invokeMethod(w, [w, v]() { w->setProperty("text", v); }, Qt::QueuedConnection);
    return ENC_SYM_TRUE;
  }
  if (type == LbmQtWidgetType::Slider) {
    if (!lbm_is_number(args[1])) return ENC_SYM_TERROR;
    double scale = w->property("_lbmSliderScale").toDouble();
    int    vmin  = w->property("_lbmSliderMin").toInt();
    double fv = ((double)lbm_dec_as_i32(args[1]) - vmin) / scale;
    QMetaObject::invokeMethod(w, [w, fv]() { w->setProperty("value", fv); }, Qt::QueuedConnection);
    return ENC_SYM_TRUE;
  }
  if (type == LbmQtWidgetType::Combo) {
    if (!lbm_is_number(args[1])) return ENC_SYM_TERROR;
    int v = (int)lbm_dec_as_i32(args[1]);
    QMetaObject::invokeMethod(w, [w, v]() { w->setProperty("currentIndex", v); },
                              Qt::QueuedConnection);
    return ENC_SYM_TRUE;
  }
  lbm_set_error_reason("qt-set-value: widget type has no settable value");
  return ENC_SYM_EERROR;
}

// ////////////////////////////////////////////////////////////
// Plot LBM extensions

static QVector<double> decodeNumberList(lbm_value list) {
  QVector<double> out;
  for (lbm_value c = list; lbm_is_cons(c); c = lbm_cdr(c)) {
    lbm_value v = lbm_car(c);
    if (lbm_is_number(v))
      out.append((double)lbm_dec_as_float(v));
  }
  return out;
}

// (qt-plot container [mouse-move] [attrs...]) → handle
static lbm_value ext_qt_plot(lbm_value *args, lbm_uint argn) {
  if (argn < 1 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;
  QQuickItem *container = getContainer((int)lbm_dec_as_i32(args[0]));
  if (!container) { lbm_set_error_reason("qt-plot: invalid container handle"); return ENC_SYM_EERROR; }

  bool sendMouseMove = false;
  lbm_uint attr_start = 1;
  if (argn >= 2 && lbm_is_symbol(args[1])) {
    const char *name = lbm_get_name_by_symbol(lbm_dec_sym(args[1]));
    if (name && strcmp(name, "mouse-move") == 0) { sendMouseMove = true; attr_start = 2; }
  }
  int handle = createPlot(container, sendMouseMove, parseAttrs(args, argn, attr_start));
  return handle >= 0 ? lbm_enc_i(handle) : ENC_SYM_EERROR;
}

// (qt-plot-add-graph handle "name" [color]) → graph-index
static lbm_value ext_qt_plot_add_graph(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_number(args[0]) || !lbm_is_array_r(args[1])) return ENC_SYM_TERROR;
  QLbmPlotItem *plot = getPlot((int)lbm_dec_as_i32(args[0]));
  if (!plot) { lbm_set_error_reason("qt-plot-add-graph: invalid handle"); return ENC_SYM_EERROR; }

  QString name = lbm_dec_qstr(args[1]);
  QColor  color;
  if (argn >= 3) color = lbmValueToColor(args[2]);

  int idx = -1;
  QMetaObject::invokeMethod(plot, [plot, name, color, &idx]() {
    idx = plot->addGraph(name, color);
  }, Qt::BlockingQueuedConnection);
  return idx >= 0 ? lbm_enc_i(idx) : ENC_SYM_EERROR;
}

// (qt-plot-set-data handle graph-idx keys-list values-list)
static lbm_value ext_qt_plot_set_data(lbm_value *args, lbm_uint argn) {
  if (argn < 4 || !lbm_is_number(args[0]) || !lbm_is_number(args[1])) return ENC_SYM_TERROR;
  QLbmPlotItem *plot = getPlot((int)lbm_dec_as_i32(args[0]));
  if (!plot) { lbm_set_error_reason("qt-plot-set-data: invalid handle"); return ENC_SYM_EERROR; }

  int idx = (int)lbm_dec_as_i32(args[1]);
  QVector<double> keys   = decodeNumberList(args[2]);
  QVector<double> values = decodeNumberList(args[3]);
  if (keys.size() != values.size()) return ENC_SYM_TERROR;

  QMetaObject::invokeMethod(plot, [plot, idx, keys, values]() {
    plot->setData(idx, keys, values);
  }, Qt::QueuedConnection);
  return ENC_SYM_TRUE;
}

// (qt-plot-append handle graph-idx key value)
static lbm_value ext_qt_plot_append(lbm_value *args, lbm_uint argn) {
  if (argn < 4 || !lbm_is_number(args[0]) || !lbm_is_number(args[1]) ||
      !lbm_is_number(args[2]) || !lbm_is_number(args[3])) return ENC_SYM_TERROR;
  QLbmPlotItem *plot = getPlot((int)lbm_dec_as_i32(args[0]));
  if (!plot) { lbm_set_error_reason("qt-plot-append: invalid handle"); return ENC_SYM_EERROR; }

  int    idx = (int)lbm_dec_as_i32(args[1]);
  double k   = (double)lbm_dec_as_float(args[2]);
  double v   = (double)lbm_dec_as_float(args[3]);
  QMetaObject::invokeMethod(plot, [plot, idx, k, v]() {
    plot->appendPoint(idx, k, v);
  }, Qt::QueuedConnection);
  return ENC_SYM_TRUE;
}

// (qt-plot-clear handle [graph-idx])
static lbm_value ext_qt_plot_clear(lbm_value *args, lbm_uint argn) {
  if (argn < 1 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;
  QLbmPlotItem *plot = getPlot((int)lbm_dec_as_i32(args[0]));
  if (!plot) { lbm_set_error_reason("qt-plot-clear: invalid handle"); return ENC_SYM_EERROR; }

  if (argn >= 2 && lbm_is_number(args[1])) {
    int idx = (int)lbm_dec_as_i32(args[1]);
    QMetaObject::invokeMethod(plot, [plot, idx]() { plot->clearGraph(idx); }, Qt::QueuedConnection);
  } else {
    QMetaObject::invokeMethod(plot, [plot]() { plot->clearAll(); }, Qt::QueuedConnection);
  }
  return ENC_SYM_TRUE;
}

// (qt-plot-replot handle)
static lbm_value ext_qt_plot_replot(lbm_value *args, lbm_uint argn) {
  if (argn < 1 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;
  QLbmPlotItem *plot = getPlot((int)lbm_dec_as_i32(args[0]));
  if (!plot) { lbm_set_error_reason("qt-plot-replot: invalid handle"); return ENC_SYM_EERROR; }
  QMetaObject::invokeMethod(plot, [plot]() { plot->triggerReplot(); }, Qt::QueuedConnection);
  return ENC_SYM_TRUE;
}

// (qt-plot-rescale handle)
static lbm_value ext_qt_plot_rescale(lbm_value *args, lbm_uint argn) {
  if (argn < 1 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;
  QLbmPlotItem *plot = getPlot((int)lbm_dec_as_i32(args[0]));
  if (!plot) { lbm_set_error_reason("qt-plot-rescale: invalid handle"); return ENC_SYM_EERROR; }
  QMetaObject::invokeMethod(plot, [plot]() {
    plot->autoRescale();
    plot->triggerReplot();
  }, Qt::QueuedConnection);
  return ENC_SYM_TRUE;
}

// (qt-plot-set-x-range handle lo hi)
static lbm_value ext_qt_plot_set_x_range(lbm_value *args, lbm_uint argn) {
  if (argn < 3 || !lbm_is_number(args[0]) || !lbm_is_number(args[1]) || !lbm_is_number(args[2]))
    return ENC_SYM_TERROR;
  QLbmPlotItem *plot = getPlot((int)lbm_dec_as_i32(args[0]));
  if (!plot) { lbm_set_error_reason("qt-plot-set-x-range: invalid handle"); return ENC_SYM_EERROR; }
  double lo = (double)lbm_dec_as_float(args[1]);
  double hi = (double)lbm_dec_as_float(args[2]);
  QMetaObject::invokeMethod(plot, [plot, lo, hi]() { plot->setXRange(lo, hi); }, Qt::QueuedConnection);
  return ENC_SYM_TRUE;
}

// (qt-plot-set-y-range handle lo hi)
static lbm_value ext_qt_plot_set_y_range(lbm_value *args, lbm_uint argn) {
  if (argn < 3 || !lbm_is_number(args[0]) || !lbm_is_number(args[1]) || !lbm_is_number(args[2]))
    return ENC_SYM_TERROR;
  QLbmPlotItem *plot = getPlot((int)lbm_dec_as_i32(args[0]));
  if (!plot) { lbm_set_error_reason("qt-plot-set-y-range: invalid handle"); return ENC_SYM_EERROR; }
  double lo = (double)lbm_dec_as_float(args[1]);
  double hi = (double)lbm_dec_as_float(args[2]);
  QMetaObject::invokeMethod(plot, [plot, lo, hi]() { plot->setYRange(lo, hi); }, Qt::QueuedConnection);
  return ENC_SYM_TRUE;
}

// (qt-plot-set-title handle "title")
static lbm_value ext_qt_plot_set_title(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_number(args[0]) || !lbm_is_array_r(args[1])) return ENC_SYM_TERROR;
  QLbmPlotItem *plot = getPlot((int)lbm_dec_as_i32(args[0]));
  if (!plot) { lbm_set_error_reason("qt-plot-set-title: invalid handle"); return ENC_SYM_EERROR; }
  QString title = lbm_dec_qstr(args[1]);
  QMetaObject::invokeMethod(plot, [plot, title]() { plot->setTitle(title); }, Qt::QueuedConnection);
  return ENC_SYM_TRUE;
}

// (qt-plot-set-x-label handle "label")
static lbm_value ext_qt_plot_set_x_label(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_number(args[0]) || !lbm_is_array_r(args[1])) return ENC_SYM_TERROR;
  QLbmPlotItem *plot = getPlot((int)lbm_dec_as_i32(args[0]));
  if (!plot) { lbm_set_error_reason("qt-plot-set-x-label: invalid handle"); return ENC_SYM_EERROR; }
  QString label = lbm_dec_qstr(args[1]);
  QMetaObject::invokeMethod(plot, [plot, label]() { plot->setXLabel(label); }, Qt::QueuedConnection);
  return ENC_SYM_TRUE;
}

// (qt-plot-set-y-label handle "label")
static lbm_value ext_qt_plot_set_y_label(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_number(args[0]) || !lbm_is_array_r(args[1])) return ENC_SYM_TERROR;
  QLbmPlotItem *plot = getPlot((int)lbm_dec_as_i32(args[0]));
  if (!plot) { lbm_set_error_reason("qt-plot-set-y-label: invalid handle"); return ENC_SYM_EERROR; }
  QString label = lbm_dec_qstr(args[1]);
  QMetaObject::invokeMethod(plot, [plot, label]() { plot->setYLabel(label); }, Qt::QueuedConnection);
  return ENC_SYM_TRUE;
}

// (qt-plot-save-png handle "path" [width height])
static lbm_value ext_qt_plot_save_png(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_number(args[0]) || !lbm_is_array_r(args[1])) return ENC_SYM_TERROR;
  QLbmPlotItem *plot = getPlot((int)lbm_dec_as_i32(args[0]));
  if (!plot) { lbm_set_error_reason("qt-plot-save-png: invalid handle"); return ENC_SYM_EERROR; }
  QString path = lbm_dec_qstr(args[1]);
  int w = (argn >= 3 && lbm_is_number(args[2])) ? (int)lbm_dec_as_i32(args[2]) : 0;
  int h = (argn >= 4 && lbm_is_number(args[3])) ? (int)lbm_dec_as_i32(args[3]) : 0;
  bool ok = false;
  QMetaObject::invokeMethod(plot, [plot, path, w, h, &ok]() {
    ok = plot->savePng(path, w, h);
  }, Qt::BlockingQueuedConnection);
  return ok ? ENC_SYM_TRUE : ENC_SYM_NIL;
}

// (qt-plot-save-pdf handle "path" [width height])
static lbm_value ext_qt_plot_save_pdf(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_number(args[0]) || !lbm_is_array_r(args[1])) return ENC_SYM_TERROR;
  QLbmPlotItem *plot = getPlot((int)lbm_dec_as_i32(args[0]));
  if (!plot) { lbm_set_error_reason("qt-plot-save-pdf: invalid handle"); return ENC_SYM_EERROR; }
  QString path = lbm_dec_qstr(args[1]);
  int w = (argn >= 3 && lbm_is_number(args[2])) ? (int)lbm_dec_as_i32(args[2]) : 0;
  int h = (argn >= 4 && lbm_is_number(args[3])) ? (int)lbm_dec_as_i32(args[3]) : 0;
  bool ok = false;
  QMetaObject::invokeMethod(plot, [plot, path, w, h, &ok]() {
    ok = plot->savePdf(path, w, h);
  }, Qt::BlockingQueuedConnection);
  return ok ? ENC_SYM_TRUE : ENC_SYM_NIL;
}

// (qt-plot-set-mouse-move handle t/nil)  — enable/disable plot-mouse-move events
static lbm_value ext_qt_plot_set_mouse_move(lbm_value *args, lbm_uint argn) {
  if (argn < 2 || !lbm_is_number(args[0])) return ENC_SYM_TERROR;
  int handle = (int)lbm_dec_as_i32(args[0]);
  LbmPlotAdapter *adapter = s_plotAdapters.value(handle, nullptr);
  if (!adapter) { lbm_set_error_reason("qt-plot-set-mouse-move: invalid handle"); return ENC_SYM_EERROR; }
  bool enabled = !lbm_is_symbol_nil(args[1]);
  QLbmPlotItem *plot = getPlot(handle);
  QMetaObject::invokeMethod(plot, [adapter, enabled]() {
    adapter->setMouseMove(enabled);
  }, Qt::QueuedConnection);
  return ENC_SYM_TRUE;
}

void lbm_qtquick_extensions_init(void) {
  lbm_add_extension("qt-root",                  ext_qt_root);
  lbm_add_extension("qt-set-display",           ext_qt_set_display);
  lbm_add_extension("qt-set-label",      ext_qt_set_label);
  lbm_add_extension("qt-display",    ext_qt_display);
  lbm_add_extension("qt-button",     ext_qt_button);
  lbm_add_extension("qt-container",  ext_qt_container);
  lbm_add_extension("qt-stretch",    ext_qt_stretch);
  lbm_add_extension("qt-set-visible",    ext_qt_set_visible);
  lbm_add_extension("qt-get-visible",    ext_qt_get_visible);
  lbm_add_extension("qt-set-enabled",    ext_qt_set_enabled);
  lbm_add_extension("qt-get-enabled",    ext_qt_get_enabled);
  lbm_add_extension("qt-set-bg-color",   ext_qt_set_bg_color);
  lbm_add_extension("qt-set-fg-color",   ext_qt_set_fg_color);
  lbm_add_extension("qt-set-max-width",  ext_qt_set_max_width);
  lbm_add_extension("qt-set-min-width",  ext_qt_set_min_width);
  lbm_add_extension("qt-set-max-height", ext_qt_set_max_height);
  lbm_add_extension("qt-set-min-height", ext_qt_set_min_height);
  lbm_add_extension("qt-remove",         ext_qt_remove);
  lbm_add_extension("qt-checkbox",   ext_qt_checkbox);
  lbm_add_extension("qt-radio",      ext_qt_radio);
  lbm_add_extension("qt-spinbox-i",  ext_qt_spinbox_i);
  lbm_add_extension("qt-spinbox-f",  ext_qt_spinbox_f);
  lbm_add_extension("qt-textfield",  ext_qt_textfield);
  lbm_add_extension("qt-label",      ext_qt_label);
  lbm_add_extension("qt-slider",     ext_qt_slider);
  lbm_add_extension("qt-combo",      ext_qt_combo);
  lbm_add_extension("qt-get-item",        ext_qt_get_item);
  lbm_add_extension("qt-get-value",      ext_qt_get_value);
  lbm_add_extension("qt-set-value",      ext_qt_set_value);
  lbm_add_extension("qt-plot",                ext_qt_plot);
  lbm_add_extension("qt-plot-add-graph",      ext_qt_plot_add_graph);
  lbm_add_extension("qt-plot-set-data",       ext_qt_plot_set_data);
  lbm_add_extension("qt-plot-append",         ext_qt_plot_append);
  lbm_add_extension("qt-plot-clear",          ext_qt_plot_clear);
  lbm_add_extension("qt-plot-replot",         ext_qt_plot_replot);
  lbm_add_extension("qt-plot-rescale",        ext_qt_plot_rescale);
  lbm_add_extension("qt-plot-set-x-range",    ext_qt_plot_set_x_range);
  lbm_add_extension("qt-plot-set-y-range",    ext_qt_plot_set_y_range);
  lbm_add_extension("qt-plot-set-title",      ext_qt_plot_set_title);
  lbm_add_extension("qt-plot-set-x-label",    ext_qt_plot_set_x_label);
  lbm_add_extension("qt-plot-set-y-label",    ext_qt_plot_set_y_label);
  lbm_add_extension("qt-plot-save-png",       ext_qt_plot_save_png);
  lbm_add_extension("qt-plot-save-pdf",       ext_qt_plot_save_pdf);
  lbm_add_extension("qt-plot-set-mouse-move", ext_qt_plot_set_mouse_move);
}

// Required so that moc processes LbmSignalAdapter defined in this .cpp file.
#include "qtquick_extensions.moc"
