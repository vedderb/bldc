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

#ifndef QTQUICK_EXTENSIONS_H_
#define QTQUICK_EXTENSIONS_H_

#include "QLbmValue.h"

class QQuickItem;
class QQmlEngine;

// Register the root QQuickItem and the QQmlEngine used to create controls.
// Must be called before lbm_qtquick_extensions_init().
// A ColumnLayout anchored to fill root is created internally as the root container.
void lbm_qtquick_extensions_set_root(QQuickItem *root, QQmlEngine *engine);

// Register all qt-* extensions with the LispBM runtime.
// Call after lbm_init() and before lbm_run_eval().
//
// Layout / structure:
//   (qt-root)                                  -> handle
//   (qt-container  ch layout)                  -> handle   layout: 'vbox 'hbox 'grid
//   (qt-stretch    ch ['horizontal])           -> handle
//   (qt-remove     ch)                         -> t
//   (qt-set-visible    handle t/nil)           -> t
//   (qt-get-visible    handle)                 -> t/nil
//   (qt-set-enabled    handle t/nil)           -> t
//   (qt-get-enabled    handle)                 -> t/nil
//   (qt-set-max-width  handle w)               -> t
//   (qt-set-min-width  handle w)               -> t
//   (qt-set-max-height handle h)               -> t
//   (qt-set-min-height handle h)               -> t
//   (qt-set-bg-color   handle "#rrggbb"|0xRRGGBB|nil)  -> t
//   (qt-set-fg-color   handle "#rrggbb"|0xRRGGBB|nil)  -> t
//
// Display:
//   (qt-display    ch w h)                     -> handle
//   (qt-set-display handle)                    -> t
//
// Widgets:
//   (qt-button     ch label)                   -> handle
//   (qt-checkbox   ch label)                   -> handle
//   (qt-radio      ch label)                   -> handle
//   (qt-spinbox-i  ch min max)                 -> handle
//   (qt-spinbox-f  ch min max step)            -> handle
//   (qt-textfield  ch [placeholder])           -> handle
//   (qt-label      ch "text")                  -> handle
//   (qt-slider     ch min max ['vertical])     -> handle
//   (qt-combo      ch '("a" "b" ...))          -> handle
//   (qt-set-label  handle "text")              -> t
//
// Value access:
//   (qt-get-value  handle)                     -> t/nil | int | float | byte-array
//   (qt-set-value  handle val)                 -> t
//   (qt-get-item   handle index)               -> byte-array  (combo only)
//
// Trailing attribute args accepted by all creation calls:
//   '(max-width N)  '(min-width N)  '(max-height N)  '(min-height N)
//   '(pos-x N)      '(pos-y N)      (grid placement)
//   '(visible nil)  '(enabled nil)
//   '(bg-color "#rrggbb"|0xRRGGBB)  '(fg-color "#rrggbb"|0xRRGGBB)
//   'scroll         'scroll-v       'scroll-h         (container only)
//
// Events sent to the LispBM event handler:
//   (button-pressed   handle)
//   (checkbox-changed handle t/nil)
//   (radio-changed    handle)
//   (spinbox-changed  handle value)
//   (textfield-commit handle text)
//   (slider-changed   handle value)
//   (combo-changed    handle index)
void lbm_qtquick_extensions_init(void);

// Register the function used to forward widget events (button-pressed,
// slider-changed, …) to the LispBM evaluator.  Must be called before
// lbm_qtquick_extensions_init().  When not set, events are silently dropped.
void lbm_qtquick_set_event_sender(bool (*fn)(const QLbmValue &));

#endif
