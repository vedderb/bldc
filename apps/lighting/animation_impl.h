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

namespace apps {
namespace lighting {

constexpr uint32_t kMinDurationMs(1);

template<typename T>
Animation<T>::Animation() : state_(AnimationState::Uninitialized) {
}

template<typename T>
Animation<T>::Animation(float start_value, float end_value,
                     AnimationUpdateCallback *update_callback,
                     AnimationCallback *complete_callback,
                     AnimationCallback *cancel_callback,
                     Interpolator *interpolator,
                     uint32_t duration_ms,
                     T *cookie)
    : start_value_(start_value), end_value_(end_value),
      update_callback_(update_callback),
      complete_callback_(complete_callback),
      cancel_callback_(cancel_callback),
      interpolator_(interpolator),
      value_(start_value_),
      state_(AnimationState::Stopped),
      cookie_(cookie) {
  set_duration_ms(duration_ms);
}

template<typename T>
void Animation<T>::Start() {
  start_time_ = chVTGetSystemTimeX();
  state_ = AnimationState::Started;
}

template<typename T>
void Animation<T>::Cancel() {
  if (state_ == AnimationState::Started && cancel_callback_) {
    state_ = AnimationState::Stopped;
    cancel_callback_(cookie_);
  }
}

template<typename T>
bool Animation<T>::Step() {
  // Only step animations that are started.
  if (state_ != AnimationState::Started) {
    return false;
  }

  // Compute the current progress through the animation.
  systime_t elapsed_time = chVTTimeElapsedSinceX(start_time_);
  uint32_t elapsed_time_ms = ST2MS(elapsed_time);
  float progress = static_cast<float>(elapsed_time_ms) / duration_ms_;

  // Clip the progress against 0.0f.
  if (progress < 0.0f) {
    progress = 0.0f;
  }

  // Clip the progress against 1.0f;
  if (progress >= 1.0f) {
    progress = 1.0f;
    state_ = AnimationState::Stopped;
  }

  // Compute the value passed to the step callback.
  value_ = interpolator_(start_value_, end_value_, progress);
  update_callback_(value_, cookie_);

  // Invoke the complete handler if this is the last frame.
  if (progress >= 1.0f) {
    complete_callback_(cookie_);
  }

  return true;
}

template<typename T>
void Animation<T>::Clear() {
  state_ = AnimationState::Uninitialized;
}

template<typename T>
bool Animation<T>::IsInitialized() const {
  return (state_ != AnimationState::Uninitialized);
}

template<typename T>
void Animation<T>::set_start_value(float start_value) {
  start_value_ = start_value;
}

template<typename T>
void Animation<T>::set_end_value(float end_value) {
  end_value_ = end_value;
}

template<typename T>
void Animation<T>::set_duration_ms(uint32_t duration_ms) {
  duration_ms_ = duration_ms == 0 ? kMinDurationMs : duration_ms;
}

template<typename T>
float Animation<T>::value() const {
  return value_;
}

}  // namespace lighting
}  // namespace apps
