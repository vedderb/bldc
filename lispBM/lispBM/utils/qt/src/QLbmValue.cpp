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

#include "QLbmValue.h"

#include "heap.h"
#include "symrepr.h"

QLbmValue::QLbmValue() : m_type(Nil) {}

QLbmValue QLbmValue::fromSymbol(const QString &name) {
  QLbmValue v;
  v.m_type   = Symbol;
  v.m_symbol = name;
  QByteArray ba = name.toUtf8();
  lbm_uint id;
  if (lbm_get_symbol_by_name(ba.data(), &id)) {
    v.m_symResolved = true;
    v.m_symId       = id;
  }
  return v;
}

QLbmValue QLbmValue::fromChar(uint8_t c) {
  QLbmValue v;
  v.m_type = Char;
  v.m_i64  = c;
  return v;
}

QLbmValue QLbmValue::fromI(lbm_int i) {
  QLbmValue v;
  v.m_type = I;
  v.m_i64  = i;
  return v;
}

QLbmValue QLbmValue::fromU(lbm_uint u) {
  QLbmValue v;
  v.m_type = U;
  v.m_u64  = u;
  return v;
}

QLbmValue QLbmValue::fromI32(int32_t i) {
  QLbmValue v;
  v.m_type = I32;
  v.m_i64  = i;
  return v;
}

QLbmValue QLbmValue::fromU32(uint32_t u) {
  QLbmValue v;
  v.m_type = U32;
  v.m_u64  = u;
  return v;
}

QLbmValue QLbmValue::fromI64(int64_t i) {
  QLbmValue v;
  v.m_type = I64;
  v.m_i64  = i;
  return v;
}

QLbmValue QLbmValue::fromU64(uint64_t u) {
  QLbmValue v;
  v.m_type = U64;
  v.m_u64  = u;
  return v;
}

QLbmValue QLbmValue::fromFloat(float f) {
  QLbmValue v;
  v.m_type = Float;
  v.m_f    = f;
  return v;
}

QLbmValue QLbmValue::fromDouble(double d) {
  QLbmValue v;
  v.m_type = Double;
  v.m_d    = d;
  return v;
}

QLbmValue QLbmValue::fromCons(const QLbmValue &car, const QLbmValue &cdr) {
  QLbmValue v;
  v.m_type = Cons;
  v.m_list = {car, cdr};
  return v;
}

QLbmValue QLbmValue::fromList(const QList<QLbmValue> &elts) {
  QLbmValue result; // Nil
  for (int i = elts.size() - 1; i >= 0; i--)
    result = fromCons(elts[i], result);
  return result;
}

QLbmValue QLbmValue::fromByteArray(const QByteArray &data) {
  QLbmValue v;
  v.m_type  = ByteArray;
  v.m_bytes = data;
  return v;
}

QLbmValue QLbmValue::fromLispArray(const QList<QLbmValue> &elts) {
  QLbmValue v;
  v.m_type = LispArray;
  v.m_list = elts;
  return v;
}

QLbmValue QLbmValue::fromLbmValue(lbm_value val) {
  if (lbm_is_symbol_nil(val)) {
    return QLbmValue(); // Nil
  }

  if (lbm_is_symbol(val)) {
    const char *name = lbm_get_name_by_symbol(lbm_dec_sym(val));
    return fromSymbol(name ? QString::fromUtf8(name) : QString());
  }

  if (lbm_is_char(val)) {
    return fromChar(lbm_dec_char(val));
  }

  lbm_type t = lbm_type_of(val);

  switch (t) {
  case LBM_TYPE_I:     return fromI(lbm_dec_i(val));
  case LBM_TYPE_U:     return fromU(lbm_dec_u(val));
  case LBM_TYPE_I32:   return fromI32(lbm_dec_as_i32(val));
  case LBM_TYPE_U32:   return fromU32(lbm_dec_as_u32(val));
  case LBM_TYPE_I64:   return fromI64(lbm_dec_as_i64(val));
  case LBM_TYPE_U64:   return fromU64(lbm_dec_as_u64(val));
  case LBM_TYPE_FLOAT: return fromFloat(lbm_dec_as_float(val));
  case LBM_TYPE_DOUBLE: return fromDouble(lbm_dec_as_double(val));

  case LBM_TYPE_CONS:
  case LBM_TYPE_CONS_CONST:
    return fromCons(fromLbmValue(lbm_car(val)), fromLbmValue(lbm_cdr(val)));

  case LBM_TYPE_ARRAY:
  case LBM_TYPE_ARRAY_CONST: {
    lbm_array_header_t *arr = lbm_dec_array_r(val);
    if (arr) {
      return fromByteArray(QByteArray((const char *)arr->data, (int)arr->size));
    }
    return QLbmValue();
  }

  case LBM_TYPE_LISPARRAY:
  case LBM_TYPE_LISPARRAY_CONST: {
    lbm_array_header_t *arr = lbm_dec_array_r(val);
    if (arr) {
      lbm_value *data = (lbm_value *)arr->data;
      lbm_uint n = arr->size / sizeof(lbm_value);
      QList<QLbmValue> elts;
      for (lbm_uint i = 0; i < n; i++) {
        elts.append(fromLbmValue(data[i]));
      }
      return fromLispArray(elts);
    }
    return QLbmValue();
  }

  default:
    return QLbmValue();
  }
}

bool QLbmValue::isUnboxed() const {
  switch (m_type) {
  case Nil:
  case Char:
  case I:
  case U:
    return true;
  case Symbol:
    return m_symResolved;
  default:
    return false;
  }
}

lbm_value QLbmValue::unboxed() const {
  switch (m_type) {
  case Nil:    return ENC_SYM_NIL;
  case Char:   return lbm_enc_char((uint8_t)m_i64);
  case I:      return lbm_enc_i((lbm_int)m_i64);
  case U:      return lbm_enc_u((lbm_uint)m_u64);
  case Symbol: return lbm_enc_sym(m_symId);
  default:     return ENC_SYM_NIL;
  }
}

bool QLbmValue::flattenValue(lbm_flat_value_t *fv) const {
  switch (m_type) {
  case Nil:
    return f_sym(fv, SYM_NIL);
  case Symbol: {
    if (m_symResolved)
      return f_sym(fv, m_symId);
    QByteArray ba = m_symbol.toUtf8();
    return f_sym_string(fv, ba.data());
  }
  case Char:   return f_b(fv, (uint8_t)m_i64);
  case I:      return f_i(fv, (lbm_int)m_i64);
  case U:      return f_u(fv, (lbm_uint)m_u64);
  case I32:    return f_i32(fv, (int32_t)m_i64);
  case U32:    return f_u32(fv, (uint32_t)m_u64);
  case I64:    return f_i64(fv, m_i64);
  case U64:    return f_u64(fv, m_u64);
  case Float:  return f_float(fv, m_f);
  case Double: return f_double(fv, m_d);
  case Cons:
    return f_cons(fv) && car().flattenValue(fv) && cdr().flattenValue(fv);
  case ByteArray: {
    return f_lbm_array(fv, (uint32_t)m_bytes.size(), (uint8_t *)m_bytes.constData());
  }
  case LispArray: {
    if (!f_lisp_array(fv, (uint32_t)m_list.size())) return false;
    for (const QLbmValue &elt : m_list) {
      if (!elt.flattenValue(fv)) return false;
    }
    return true;
  }
  }
  return false;
}

uint32_t QLbmValue::flatSize() const {
  switch (m_type) {
  case Nil:    return 1 + sizeof(lbm_uint);
  case Symbol:
    if (m_symResolved) return 1 + sizeof(lbm_uint);
    return 1 + (uint32_t)m_symbol.toUtf8().size() + 1;
  case Char:   return 1 + 1;
  case I:      return 1 + sizeof(lbm_int);
  case U:      return 1 + sizeof(lbm_uint);
  case I32:    return 1 + 4;
  case U32:    return 1 + 4;
  case I64:    return 1 + 8;
  case U64:    return 1 + 8;
  case Float:  return 1 + 4;
  case Double: return 1 + 8;
  case Cons:
    return 1 + car().flatSize() + cdr().flatSize();
  case ByteArray:
    return 1 + 4 + (uint32_t)m_bytes.size();
  case LispArray: {
    uint32_t size = 1 + 4;
    for (const QLbmValue &elt : m_list)
      size += elt.flatSize();
    return size;
  }
  }
  return 0;
}

bool QLbmValue::flatten(lbm_flat_value_t *fv) const {
  if (!lbm_start_flatten(fv, flatSize())) return false;
  if (!flattenValue(fv)) {
    lbm_finish_flatten(fv);
    free(fv->buf);
    return false;
  }
  return lbm_finish_flatten(fv);
}
