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

#ifndef QLBMVALUE_H_
#define QLBMVALUE_H_

#include <QString>
#include <QList>
#include <QByteArray>

extern "C" {
#include "lispbm.h"
#include "lbm_flat_value.h"
}

class QLbmValue {
public:
  enum Type {
    Nil,
    Symbol,
    Char,
    I,
    U,
    I32,
    U32,
    I64,
    U64,
    Float,
    Double,
    Cons,
    ByteArray,
    LispArray
  };

  QLbmValue();  // Nil

  static QLbmValue fromSymbol(const QString &name);
  static QLbmValue fromChar(uint8_t c);
  static QLbmValue fromI(lbm_int v);
  static QLbmValue fromU(lbm_uint v);
  static QLbmValue fromI32(int32_t v);
  static QLbmValue fromU32(uint32_t v);
  static QLbmValue fromI64(int64_t v);
  static QLbmValue fromU64(uint64_t v);
  static QLbmValue fromFloat(float v);
  static QLbmValue fromDouble(double v);
  static QLbmValue fromCons(const QLbmValue &car, const QLbmValue &cdr);
  // Convenience: builds a proper list (cons tree terminated by nil).
  static QLbmValue fromList(const QList<QLbmValue> &elts);
  static QLbmValue fromByteArray(const QByteArray &data);
  static QLbmValue fromLispArray(const QList<QLbmValue> &elts);

  // Decode from a live lbm_value.
  // Safe to call from extension functions (which run on the eval thread).
  // If called from a thread other than the evaluator thread, the evaluator
  // must be paused first!
  static QLbmValue fromLbmValue(lbm_value v);

  Type type() const { return m_type; }

  bool     isUnboxed() const;
  lbm_value unboxed()   const;

  uint32_t flatSize() const;
  bool flatten(lbm_flat_value_t *fv) const;

  QString          asSymbol()    const { return m_symbol; }
  uint8_t          asChar()      const { return (uint8_t)m_i64; }
  lbm_int          asI()         const { return (lbm_int)m_i64; }
  lbm_uint         asU()         const { return (lbm_uint)m_u64; }
  int32_t          asI32()       const { return (int32_t)m_i64; }
  uint32_t         asU32()       const { return (uint32_t)m_u64; }
  int64_t          asI64()       const { return m_i64; }
  uint64_t         asU64()       const { return m_u64; }
  float            asFloat()     const { return m_f; }
  double           asDouble()    const { return m_d; }
  QLbmValue        car()         const { return m_list.value(0); }
  QLbmValue        cdr()         const { return m_list.value(1); }
  QByteArray       asByteArray() const { return m_bytes; }
  QList<QLbmValue> asLispArray() const { return m_list; }

private:
  bool flattenValue(lbm_flat_value_t *fv) const;

  Type             m_type   = Nil;
  QString          m_symbol;
  bool             m_symResolved = false;
  lbm_uint         m_symId  = 0;
  int64_t          m_i64    = 0;
  uint64_t         m_u64    = 0;
  float            m_f     = 0.0f;
  double           m_d     = 0.0;
  QList<QLbmValue> m_list;
  QByteArray       m_bytes;
};

#endif
