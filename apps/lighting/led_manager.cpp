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

#include "apps/lighting/led_manager.h"

namespace apps {
namespace lighting {

using namespace drivers;

// Positions of various lighting features in the WS2812 strip.
struct LightFeature {
  constexpr LightFeature(size_t start_index, size_t length)
      : start_index(start_index), length(length) {}

  size_t start_index;
  size_t length;
};

// Lighting feature constants.
constexpr LightFeature kHeadlight(41, 10);
constexpr LightFeature kLeftSignal(38, 3);
constexpr LightFeature kRightSignal(51, 3);
constexpr LightFeature kLeftSide(18, 20);
constexpr LightFeature kRightSide(54, 20);
constexpr LightFeature kBrakeLight(0, 18);

// Duation constants.
constexpr uint32_t kGroundEffectsAnimationMs(2000);
constexpr uint32_t kBrakeAnimationMs(200);
constexpr uint32_t kSignalAnimationMs(800);

// Color constants.
namespace color {

const ws2812::Color kIdleGround = { .blue =  32, .red =  32, .green =   32, };
const ws2812::Color kGround     = { .blue = 120, .red = 120, .green =  120, };
const ws2812::Color kIdleBrake  = { .blue =   1, .red =  40, .green =    1, };
const ws2812::Color kBrake      = { .blue =   4, .red = 255, .green =    8, };
const ws2812::Color kIdleSignal = { .blue =   0, .red =  25, .green =    7, };
const ws2812::Color kSignal     = { .blue =   0, .red = 255, .green =   70, };

}  // namespace color

LedManager::LedManager() : animation_manager_(20, this),
    frame_dirty_(false) {
  chMtxObjectInit(&animations_mutex_);
}

void LedManager::Start() {
  ws2812::Init();

  InitGroundEffectsLight();
  InitFrontLight();
  InitLeftSignal();
  InitRightSignal();
  InitBrakeLight();

  turn_signal_state_ = TurnSignalState::Disabled;
  frame_dirty_ = true;

  animation_manager_.Start();
}

void LedManager::SetBrakeState(BrakeState brake_state) {
  if (brake_state_ == brake_state) {
    return;
  }

  chMtxLock(&animations_mutex_);

  if (brake_state == BrakeState::Idle) {
    brake_start_->Cancel();
    brake_end_->set_duration_ms(brake_start_->value() * kBrakeAnimationMs);
    brake_end_->set_start_value(1.0f - brake_start_->value());
    brake_end_->Start();
  } else if (brake_state == BrakeState::Braking) {
    brake_end_->Cancel();
    brake_start_->set_duration_ms(brake_end_->value() * kBrakeAnimationMs);
    brake_start_->set_start_value(1.0f - brake_end_->value());
    brake_start_->Start();
  }

  brake_state_ = brake_state;
  chMtxUnlock(&animations_mutex_);
}

void LedManager::SetTurnSignalState(TurnSignalState state) {
  if (state == turn_signal_state_) {
    return;
  }

  chMtxLock(&animations_mutex_);

  if (state == TurnSignalState::Left) {
    left_turn_signal_->Start();
  } else if (state == TurnSignalState::Right) {
    right_turn_signal_->Start();
  } else if (state == TurnSignalState::FourWay) {
    left_turn_signal_->Start();
    right_turn_signal_->Start();
  } else if (state == TurnSignalState::Disabled) {
    left_turn_signal_->Cancel();
    right_turn_signal_->Cancel();
  }

  turn_signal_state_ = state;
  chMtxUnlock(&animations_mutex_);
}

TurnSignalState LedManager::turn_signal_state() const {
  return turn_signal_state_;
}

void LedManager::AnimationManagerOnFrameStart(
    LedAnimationManager *animation_manager) {
  chMtxLock(&animations_mutex_);
}

void LedManager::AnimationManagerOnFrameEnd(
    LedAnimationManager *animation_manager) {
  if (frame_dirty_) {
    ws2812::SendFrame(leds_, 74);
    frame_dirty_ = false;
  }

  chMtxUnlock(&animations_mutex_);
}

void LedManager::AnimationManagerOnFrameRendered(
    LedAnimationManager *animation_manager) {
  frame_dirty_ = true;
}

void LedManager::SetRange(size_t start, size_t count,
    const ws2812::Color& color) {
  for (size_t i = start; i < start + count; i++) {
    leds_[i] = color;
  }
}

void LedManager::InitGroundEffectsLight() {
  side_fade_in_ = animation_manager_.NewAnimation(0.0f, 1.0f,
      [](float v, LedManager *manager) {
        ws2812::Color color;
        ws2812::InterpolateColor(&color,
            color::kIdleGround, color::kGround, v);
        manager->SetRange(kLeftSide.start_index, kLeftSide.length, color);
        manager->SetRange(kRightSide.start_index, kRightSide.length, color);
      },
      [](LedManager *manager) {
        manager->side_fade_out_->Start();
      },
      [](LedManager *manager) {},
      LinearInterpolate, kGroundEffectsAnimationMs, this);

  side_fade_out_ = animation_manager_.NewAnimation(1.0f, 0.0f,
      [](float v, LedManager *manager) {
        ws2812::Color color;
        ws2812::InterpolateColor(&color, color::kIdleGround, color::kGround, v);
        manager->SetRange(18, 20, color);
        manager->SetRange(54, 20, color);
      },
      [](LedManager *manager) {
        manager->side_fade_in_->Start();
      },
      [](LedManager *manager) {},
      LinearInterpolate, kGroundEffectsAnimationMs, this);
  side_fade_in_->Start();
}

void LedManager::InitFrontLight() {
  SetRange(kHeadlight.start_index, kHeadlight.length, ws2812::white);
}

void LedManager::ClearLeftSignal() {
   SetRange(kLeftSignal.start_index, kLeftSignal.length, ws2812::white);
 
}

void LedManager::InitLeftSignal() {
  ClearLeftSignal();

  left_turn_signal_ = animation_manager_.NewAnimation(1.0f, 0.0f,
      [](float v, LedManager *manager) {
        ws2812::Color color;
        ws2812::InterpolateColor(&color, color::kIdleSignal, color::kSignal, v);
        manager->SetRange(kLeftSide.start_index, kLeftSide.length, color);
      },
      [](LedManager *manager) {
        manager->left_turn_signal_->Start();
      },
      [](LedManager *manager) {},
      StepInterpolate, kSignalAnimationMs, this);
}

void LedManager::ClearRightSignal() {
  SetRange(kRightSignal.start_index, kRightSignal.length, ws2812::white);
}

void LedManager::InitRightSignal() {
  ClearRightSignal();

  right_turn_signal_ = animation_manager_.NewAnimation(1.0f, 0.0f,
      [](float v, LedManager *manager) {
        ws2812::Color color;
        ws2812::InterpolateColor(&color, color::kIdleSignal, color::kSignal, v);
        manager->SetRange(kRightSide.start_index, kRightSide.length, color);
      },
      [](LedManager *manager) {
        manager->right_turn_signal_->Start();
      },
      [](LedManager *manager) {},
      StepInterpolate, kSignalAnimationMs, this);
}

void LedManager::InitBrakeLight() {
  brake_state_ = BrakeState::Idle;
  SetRange(kBrakeLight.start_index, kBrakeLight.length, color::kIdleBrake);

  brake_start_ = animation_manager_.NewAnimation(0.0f, 1.0f,
      [](float v, LedManager *manager) {
        ws2812::Color brake_color;
        ws2812::InterpolateColor(&brake_color, color::kIdleBrake,
            color::kBrake, v);
        manager->SetRange(kBrakeLight.start_index,
            kBrakeLight.length, brake_color);
      },
      [](LedManager *manager) {},
      [](LedManager *manager) {},
      CubicEaseOutInterpolate, kBrakeAnimationMs, this);

  /*
   * Initialize the brake end animation to interoplate from 1.0f to 1.0f. The
   * initial transtiion is always an idle to braking. Interpolating from 1.0f
   * to 1.0f makes the initial return of the animation value be equal to 1.0f.
   * This prevents the initial brake start animation from starting at full red
   * when the lights are at idle state already.
   */
  brake_end_ = animation_manager_.NewAnimation(1.0f, 1.0f,
      [](float v, LedManager *manager) {
        ws2812::Color brake_color;
        ws2812::InterpolateColor(&brake_color, color::kBrake,
            color::kIdleBrake, v);
        manager->SetRange(kBrakeLight.start_index,
            kBrakeLight.length, brake_color);
      },
      [](LedManager *manager) {},
      [](LedManager *manager) {},
      CubicEaseOutInterpolate, kBrakeAnimationMs, this);
}

}  // namespace lighting
}  // namespace apps
