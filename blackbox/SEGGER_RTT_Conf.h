/*********************************************************************
*                   (c) SEGGER Microcontroller GmbH                  *
*                        The Embedded Experts                        *
*                           www.segger.com                           *
**********************************************************************
*                                                                    *
*        SEGGER RTT * Real Time Transfer for embedded targets        *
*                  https://github.com/SEGGERMicro/RTT                *
*                                                                    *
**********************************************************************

---------------------------END-OF-HEADER------------------------------
Purpose : User configuration file for RTT.
          For available configuration,
          refer to SEGGER_RTT_ConfDefaults.h.

----------------------------------------------------------------------
*/

#ifndef SEGGER_RTT_CONF_H
#define SEGGER_RTT_CONF_H


/*********************************************************************
*
*       Defines, configurable
*
**********************************************************************
*/

// VESC blackbox: minimal RTT setup. One up buffer (target -> host),
// non-blocking so the FOC ISR / threads are never stalled by a slow host.
#define SEGGER_RTT_MAX_NUM_UP_BUFFERS     (1)
#define SEGGER_RTT_MAX_NUM_DOWN_BUFFERS   (1)
#define BUFFER_SIZE_UP                    (1024)
#define BUFFER_SIZE_DOWN                  (16)
#define SEGGER_RTT_MODE_DEFAULT           SEGGER_RTT_MODE_NO_BLOCK_SKIP

#endif
/*************************** End of file ****************************/
