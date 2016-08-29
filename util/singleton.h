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

#ifndef UTIL_SINGLETON_H_
#define UTIL_SINGLETON_H_

#include <cstdint>
#include <type_traits>

namespace util {

/*
 * A singleton template that reserves space for an object and provides a means
 * for obtaining the instance. This is primarily intended for objects that
 * provide a service for which there should only be one instance of.
 */
template<typename T>
class Singleton {
 public:
  /*
   * Initializes the singleton by invoking T's constructor with matching
   * arguments.
   */
  template<typename... Args>
  static void Init(Args&&... args);

  /*
   * Invokes the Singleton deconstructor and allows the object to be
   * re-initialized.
   */
  static void DeInit();

  /*
   * Obtains a pointer to the underlying object.
   *
   * If the object is not initialized first, nullptr will be returned.
   */
  static T* Instance();

  /*
   * Obtains whether or not the underlying object is currently initialized.
   */
  static bool IsInitialized();

 private:
  /*
   * The static instance of the Singleton. Allocated in aligned_storage to
   * avoid C++ static initialization and force it to be explicit through the
   * static Init function above.
   */
  static typename std::aligned_storage<sizeof(T), alignof(T)>::type instance_;

  /*
   * Maintains the state of whether or not the Singleton was initialized.
   */
  static bool is_initialized_;
};

} // namespace util

#include "util/singleton_impl.h"

#endif  // UTIL_SINGLETON_H_
