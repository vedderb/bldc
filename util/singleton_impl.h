/*
 * Copyright 2016 Andrew Rossignol andrew.rossignol@gmail.com
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <utility>

namespace util {

template<typename T>
template<typename... Args>
void Singleton<T>::Init(Args&&... args) {
  if (!is_initialized_) {
    new (&instance_) T(std::forward<Args>(args)...);
    is_initialized_ = true;
  }
}

template<typename T>
void Singleton<T>::DeInit() {
  if (is_initialized_) {
    Instance()->~T();
    is_initialized_ = false;
  }
}

template<typename T>
T *Singleton<T>::Instance() {
  if (is_initialized_) {
    return reinterpret_cast<T *>(&instance_);
  } else {
    return nullptr;
  }
}

template<typename T>
bool Singleton<T>::IsInitialized() {
  return is_initialized_;
}

template<typename T>
typename std::aligned_storage<sizeof(T), alignof(T)>::type
    Singleton<T>::instance_;

template<typename T>
bool Singleton<T>::is_initialized_ = false;

}  // namespace util
