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

#ifndef APPS_LIGHTING_ANIMATION_MANAGER_H_
#define APPS_LIGHTING_ANIMATION_MANAGER_H_

#include <ch.h>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <utility>

#include "apps/lighting/animation.h"
#include "util/noncopyable.h"

namespace apps {
namespace lighting {

template<typename T, size_t max_animations_>
class AnimationManager;

template<typename T, size_t max_animations_>
class AnimationManagerEventHandler {
 public:
  ~AnimationManagerEventHandler() {}

  virtual void AnimationManagerOnFrameStart(
      AnimationManager<T, max_animations_> *animation_manager) = 0;
  virtual void AnimationManagerOnFrameEnd(
      AnimationManager<T, max_animations_> *animation_manager) = 0;
  virtual void AnimationManagerOnFrameRendered(
      AnimationManager<T, max_animations_> *animation_manager) = 0;
};

/*
 * Manages a set of animations and plays them back.
 */
template<typename T, size_t max_animations_>
class AnimationManager : public util::NonCopyable {
 public:
  /*
   * Construct an animation manager.
   */
  AnimationManager(
      uint32_t frame_duration_ms,
      AnimationManagerEventHandler<T, max_animations_> *event_handler);

  /*
   * Starts the animation looper.
   */
  void Start();

  /*
   * Creates a new animation and returns a pointer to it. Returns nullptr if
   * there are no available animations slots.
   */
  template<typename... Args>
  Animation<T> *NewAnimation(Args&&... args);

 private:
  // The list of animations being managed by the AnimationManager.
  Animation<T> animations_[max_animations_];

  // The duration of each frame.
  uint32_t frame_duration_ms_;

  // The event handler associated with this animation manager.
  AnimationManagerEventHandler<T, max_animations_> *event_handler_;
};

}  // namespace lighting
}  // namespace apps

#include "apps/lighting/animation_manager_impl.h"

#endif  // APPS_LIGHTING_ANIMATION_MANAGER_H_
