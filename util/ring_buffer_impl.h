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

namespace util {

template<typename T, size_t capacity_>
RingBuffer<T, capacity_>::RingBuffer()
    : read_index_(0),
      write_index_(0),
      size_(0) {}

template<typename T, size_t capacity_>
void RingBuffer<T, capacity_>::Enqueue(const T& element) {
  buffer_[write_index_] = element;

  write_index_++;
  if (write_index_ == capacity_) {
    write_index_ = 0;
  }

  if (IsFull()) {
    // If the buffer was full, the oldest element was overwritten. This means
    // that the read index needs to be advanced.
    read_index_++;
    if (read_index_ == capacity_) {
      read_index_ = 0;
    }
  } else {
    // The buffer was not full, just increment the io_count.
    size_++;
  }
}

template<typename T, size_t capacity_>
bool RingBuffer<T, capacity_>::Dequeue(T *element) {
  if (IsEmpty()) {
    return false;
  }

  *element = buffer_[read_index_];

  size_--;
  read_index_++;
  if (read_index_ == capacity_) {
    read_index_ = 0;
  }

  return true;
}

template<typename T, size_t capacity_>
bool RingBuffer<T, capacity_>::IsFull() const {
  return size_ == capacity_;
}

template<typename T, size_t capacity_>
bool RingBuffer<T, capacity_>::IsEmpty() const {
  return size_ == 0;
}

template<typename T, size_t capacity_>
size_t RingBuffer<T, capacity_>::capacity() const {
  return capacity_;
}

template<typename T, size_t capacity_>
size_t RingBuffer<T, capacity_>::size() const {
  return size_;
}

}  // namespace util
