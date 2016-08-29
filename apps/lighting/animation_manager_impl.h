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

namespace apps {
namespace lighting {

template<typename T, size_t max_animations_>
AnimationManager<T, max_animations_>::AnimationManager(
    uint32_t frame_duration_ms,
    AnimationManagerEventHandler<T, max_animations_> *event_handler)
        : frame_duration_ms_(frame_duration_ms),
          event_handler_(event_handler) {
}

template<typename T, size_t max_animations_>
void AnimationManager<T, max_animations_>::Start() {
  while (1) {
    systime_t frame_begin_time = chVTGetSystemTimeX();

    // Step all animations.
    event_handler_->AnimationManagerOnFrameStart(this);

    bool step_required = false;

    for (size_t i = 0; i < max_animations_; i++) {
      Animation<T> *animation = &animations_[i];
      step_required |= animation->Step();
    }

    event_handler_->AnimationManagerOnFrameEnd(this);

    // Invoke the frame complete handler if any of the animations are running.
    if (step_required) {
      event_handler_->AnimationManagerOnFrameRendered(this);
    }

    // Sleep for the duration of the frame to maintain a constant frame rate.
    systime_t frame_compute_time = chVTTimeElapsedSinceX(frame_begin_time);
    chThdSleepMilliseconds(frame_duration_ms_ - ST2MS(frame_compute_time));
  }
}

template<typename T, size_t max_animations_>
template<typename... Args>
Animation<T> *AnimationManager<T, max_animations_>::NewAnimation(
    Args&&... args) {
  Animation<T> *new_animation = nullptr;

  // Find a free slot in the animations pool.
  for (size_t i = 0; i < max_animations_; i++) {
    Animation<T> *animation = &animations_[i];
    if (!animation->IsInitialized()) {
      new_animation = animation;
      break;
    }
  }

  if (new_animation) {
    new_animation = new (new_animation)
        Animation<T>(std::forward<Args>(args)...);
  }

  return new_animation;
}

}  // namespace lighting
}  // namespace apps
