#include <cstdbool>
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
