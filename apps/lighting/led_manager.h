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

#ifndef APPS_LIGHTING_LED_MANAGER_H_
#define APPS_LIGHTING_LED_MANAGER_H_

#include "apps/lighting/animation_manager.h"
#include "drivers/ws2812.h"
#include "util/noncopyable.h"

namespace apps {
namespace lighting {

/*
 * The states possible for the turn signals.
 */
enum class TurnSignalState {
  Disabled,
  Left,
  Right,
  FourWay,
};

/*
 * The states possible for the brake light.
 */
enum class BrakeState {
  Idle,
  Braking
};

/*
 * Manages a framebuffer for a WS2812 LED strip and animations associated with
 * it.
 */
class LedManager : public util::NonCopyable,
                   public AnimationManagerEventHandler<LedManager, 16> {
 public:
  /*
   * Shorthand for template types.
   */
  typedef AnimationManager<LedManager, 16> LedAnimationManager;
  typedef Animation<LedManager> LedAnimation;

  /*
   * Construct an LED manager to control board lighting.
   */
  LedManager();

  /*
   * Start the animation looper.
   */
  void Start();

  /*
   * Sets the state of the brake light.
   */
  void SetBrakeState(BrakeState brake_state);

  /*
   * Sets the state of the turn signals.
   */
  void SetTurnSignalState(TurnSignalState state);

  /*
   * Obtains the state of the turn signal.
   */
  TurnSignalState turn_signal_state() const;

  // AnimationManagerEventHandler methods.
  void AnimationManagerOnFrameStart(
      LedAnimationManager *animation_manager) override;
  void AnimationManagerOnFrameEnd(
      LedAnimationManager *animation_manager) override;
  void AnimationManagerOnFrameRendered(
      LedAnimationManager *animation_manager) override;

 private:
  // Guard the LED buffer with a lock.
  // TODO(aarossig): Consider implementing a job queue to cut down on latency.
  // It is better to block the animation thread rather than the thread calling
  // into the LedManager.
  mutex_t animations_mutex_;

  // The manager of animations for the LEDs.
  LedAnimationManager animation_manager_;

  // The frame buffer for the LED strip.
  drivers::ws2812::Color leds_[74];

  // Animations.
  LedAnimation *side_fade_in_;
  LedAnimation *side_fade_out_;
  LedAnimation *left_turn_signal_;
  LedAnimation *right_turn_signal_;
  LedAnimation *brake_start_;
  LedAnimation *brake_end_;

  // Lighting states.
  BrakeState brake_state_;
  TurnSignalState turn_signal_state_;

  // Signal that a frame repaint is required.
  bool frame_dirty_;

  /*
   * Sets a range of LEDs to the same color.
   */
  void SetRange(size_t start, size_t count,
      const drivers::ws2812::Color& color);

  /*
   * Initializes the ground effects lighting.
   */
  void InitGroundEffectsLight();

  /*
   * Initializes the headlight.
   */
  void InitFrontLight();

  /*
   * Clears the left signal to the default color.
   */
  void ClearLeftSignal();

  /*
   * Initializes the left signal.
   */
  void InitLeftSignal();
  
  /*
   * Clears thr right signal to the default color.
   */
  void ClearRightSignal();

  /*
   * Initializes the right signal.
   */
  void InitRightSignal();

  /*
   * Initializes the brake light.
   */
  void InitBrakeLight();
};

}  // namespace lighting
}  // namespace apps

#endif  // APPS_LIGHTING_LED_MANAGER_H_
