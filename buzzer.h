#ifndef BUZZER_H_
#define BUZZER_H_

#include "ch.h" // ChibiOS
#include "hal.h" // ChibiOS HAL

void buzzer_enable(bool enable);
bool is_buzzer_enabled(void);

void update_beep_alert(void);
void beep_alert(int num_beeps, bool longbeep);
void beep_off(bool force);
void beep_on(bool force);

#endif /* BUZZER_H_ */
