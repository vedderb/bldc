#ifndef CH_H
#define CH_H

#include <stdint.h>
#include <stdbool.h>

// Minimal ChibiOS stand-ins for building canard_driver.c on the host. Time is in ms.
typedef uint32_t systime_t;
extern systime_t test_time_ms;

#define chVTGetSystemTimeX()		(test_time_ms)
#define chVTTimeElapsedSinceX(t)	(chVTGetSystemTimeX() - (t))
#define ST2MS(x)					((uint32_t)(x))
#define ST2US(x)					((uint64_t)(x) * 1000)
#define ST2S(x)						((uint32_t)(x) / 1000)

typedef struct {
	int locked;
} mutex_t;

#define MUTEX_DECL(name)			mutex_t name = {0}
#define chMtxLock(m)				((m)->locked++)
#define chMtxUnlock(m)				((m)->locked--)

#define NORMALPRIO					64
#define THD_WORKING_AREA(s, n)		uint8_t s[n]
#define THD_FUNCTION(tname, arg)	void tname(void *arg)
#define chThdCreateStatic(wa, size, prio, fn, arg)	((void)(wa), (void)(fn))
#define chRegSetThreadName(name)	((void)(name))
#define chThdSleepMilliseconds(ms)	(test_time_ms += (ms))

#endif // CH_H
