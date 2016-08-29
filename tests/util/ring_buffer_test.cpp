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

#include <gtest/gtest.h>

#include "util/ring_buffer.h"

TEST(RingBufferTest, Capacity) {
  util::RingBuffer<int, 4> ring_buffer;
  ASSERT_EQ(ring_buffer.capacity(), 4);
  ASSERT_EQ(ring_buffer.size(), 0);
}

TEST(RingBufferTest, EmptyByDefault) {
  util::RingBuffer<int, 4> ring_buffer;

  ASSERT_TRUE(ring_buffer.IsEmpty());
}

TEST(RingBufferTest, NotFullByDefault) {
  util::RingBuffer<int, 4> ring_buffer;

  ASSERT_FALSE(ring_buffer.IsFull());
}

TEST(RingBufferTest, EmptyDequeue) {
  util::RingBuffer<int, 4> ring_buffer;

  int value = 1234;
  ASSERT_FALSE(ring_buffer.Dequeue(&value));
  ASSERT_EQ(value, 1234);
}

TEST(RingBufferTest, EnqueueDequeue) {
  util::RingBuffer<int, 4> ring_buffer;
  ring_buffer.Enqueue(42);

  ASSERT_EQ(ring_buffer.size(), 1);
  ASSERT_FALSE(ring_buffer.IsEmpty());
  ASSERT_FALSE(ring_buffer.IsFull());

  int value;
  ASSERT_TRUE(ring_buffer.Dequeue(&value));
  ASSERT_EQ(value, 42);
}

TEST(RingBufferTest, FullEnqueue) {
  util::RingBuffer<int, 4> ring_buffer;
  ring_buffer.Enqueue(10);
  ring_buffer.Enqueue(20);
  ring_buffer.Enqueue(30);
  ring_buffer.Enqueue(40);

  ASSERT_FALSE(ring_buffer.IsEmpty());
  ASSERT_TRUE(ring_buffer.IsFull());

  ring_buffer.Enqueue(50);

  int value;
  ring_buffer.Dequeue(&value);
  ASSERT_EQ(value, 20);
}
