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
Purpose : Default configuration for RTT.
          Do not change this file! Use SEGGER_RTT_Conf.h instead.
----------------------------------------------------------------------
*/

#ifndef SEGGER_RTT_CONF_DEFAULTS_H
#define SEGGER_RTT_CONF_DEFAULTS_H

#include "SEGGER_RTT_Conf.h"

/*********************************************************************
*
*       Defines, configurable
*
**********************************************************************
*/

//
// Take in and set to correct values for Cortex-A systems with CPU cache
//
//#define SEGGER_RTT_CPU_CACHE_LINE_SIZE            (32)          // Largest cache line size (in bytes) in the current system
//#define SEGGER_RTT_UNCACHED_OFF                   (0xFB000000)  // Address alias where RTT CB and buffers can be accessed uncached
//
/*********************************************************************
*
*      SEGGER_RTT_MAX_NUM_UP_BUFFERS 
*
*  Description
*    Maximum number of RTT up-buffers (Target -> Host).
*
*  Additional information
*    Common use case:
*      Up-buffer channel 0: RTT Terminal I/O.
*      Up-buffer channel 1: SystemView.
*/
#ifndef   SEGGER_RTT_MAX_NUM_UP_BUFFERS
  #define SEGGER_RTT_MAX_NUM_UP_BUFFERS             (3)
#endif
/*********************************************************************
*
*      SEGGER_RTT_MAX_NUM_DOWN_BUFFERS 
*
*  Description
*    Maximum number of RTT down-buffers (Host -> Target).
*
*  Additional information
*    Common use case:
*      Down-buffer channel 0: RTT Terminal I/O.
*      Down-buffer channel 1: SystemView.
*
*    The number of up- and down-buffers may differ.
*/
#ifndef   SEGGER_RTT_MAX_NUM_DOWN_BUFFERS
  #define SEGGER_RTT_MAX_NUM_DOWN_BUFFERS           (3)
#endif

/*********************************************************************
*
*      BUFFER_SIZE_UP
*
*  Description
*    Size of (auto-installed) up-buffer channel 0.
*
*  Additional information
*    Channel 0 is commonly used for Terminal I/O.
*    Buffer should be large enough for all terminal (to host) output
*    messages.
*/
#ifndef   BUFFER_SIZE_UP
  #define BUFFER_SIZE_UP                            (1024)
#endif

/*********************************************************************
*
*      BUFFER_SIZE_DOWN
*
*  Description
*    Size of (auto-installed) down-buffer channel 0.
*
*  Additional information
*    Channel 0 is commonly used for Terminal I/O.
*    Buffer should be large enough to receive all terminal
*    (from host) input.
*/
#ifndef   BUFFER_SIZE_DOWN
  #define BUFFER_SIZE_DOWN                          (16)
#endif

/*********************************************************************
*
*     SEGGER_RTT_MODE_DEFAULT 
*
*  Description
*    RTT Mode for channel 0.
*/
#ifndef   SEGGER_RTT_MODE_DEFAULT
  #define SEGGER_RTT_MODE_DEFAULT                   SEGGER_RTT_MODE_NO_BLOCK_SKIP
#endif

/*********************************************************************
*
*     SEGGER_RTT_PRINTF_BUFFER_SIZE 
*
*  Description
*    Size of temporary buffer for RTT printf bulk-send.
*/
#ifndef   SEGGER_RTT_PRINTF_BUFFER_SIZE
  #define SEGGER_RTT_PRINTF_BUFFER_SIZE             (64u)
#endif

/*********************************************************************
*
*       RTT memcpy configuration
*
*       memcpy() is good for large amounts of data,
*       but the overhead is big for small amounts, which are usually stored via RTT.
*       With SEGGER_RTT_MEMCPY_USE_BYTELOOP a simple byte loop can be used instead.
*
*       SEGGER_RTT_MEMCPY() can be used to replace standard memcpy() in RTT functions.
*       This is may be required with memory access restrictions,
*       such as on Cortex-A devices with MMU.
*/
#ifndef   SEGGER_RTT_MEMCPY_USE_BYTELOOP
  #define SEGGER_RTT_MEMCPY_USE_BYTELOOP              0 // 0: Use memcpy/SEGGER_RTT_MEMCPY, 1: Use a simple byte-loop
#endif
//
// Example definition of SEGGER_RTT_MEMCPY to external memcpy with GCC toolchains and Cortex-A targets
//
//#if ((defined __SES_ARM) || (defined __CROSSWORKS_ARM) || (defined __GNUC__)) && (defined (__ARM_ARCH_7A__))
//  #define SEGGER_RTT_MEMCPY(pDest, pSrc, NumBytes)      SEGGER_memcpy((pDest), (pSrc), (NumBytes))
//#endif

//
// Target is not allowed to perform other RTT operations while string still has not been stored completely.
// Otherwise we would probably end up with a mixed string in the buffer.
// If using  RTT from within interrupts, multiple tasks or multi processors, define the SEGGER_RTT_LOCK() and SEGGER_RTT_UNLOCK() function here.
//
// SEGGER_RTT_MAX_INTERRUPT_PRIORITY can be used in the sample lock routines on Cortex-M3/4.
// Make sure to mask all interrupts which can send RTT data, i.e. generate SystemView events, or cause task switches.
// When high-priority interrupts must not be masked while sending RTT data, SEGGER_RTT_MAX_INTERRUPT_PRIORITY needs to be adjusted accordingly.
// (Higher priority = lower priority number)
// Default value for embOS: 128u
// Default configuration in FreeRTOS: configMAX_SYSCALL_INTERRUPT_PRIORITY: ( configLIBRARY_MAX_SYSCALL_INTERRUPT_PRIORITY << (8 - configPRIO_BITS) )
// In case of doubt mask all interrupts: 1 << (8 - BASEPRI_PRIO_BITS) i.e. 1 << 5 when 3 bits are implemented in NVIC
// or define SEGGER_RTT_LOCK() to completely disable interrupts.
//
#ifndef   SEGGER_RTT_MAX_INTERRUPT_PRIORITY
  #define SEGGER_RTT_MAX_INTERRUPT_PRIORITY         (0x20)   // Interrupt priority to lock on SEGGER_RTT_LOCK on Cortex-M3/4 (Default: 0x20)
#endif

/*********************************************************************
*
*       RTT lock configuration for SEGGER Embedded Studio,
*       Rowley CrossStudio and GCC
*/
#if !defined(SEGGER_RTT_LOCK) || !defined (SEGGER_RTT_UNLOCK)
  #if ((defined(__SES_ARM) || defined(__SES_RISCV) || defined(__CROSSWORKS_ARM) || defined(__GNUC__) || defined(__clang__)) && !defined (__CC_ARM) && !defined(WIN32))
    #if (defined(__ARM_ARCH_6M__) || defined(__ARM_ARCH_8M_BASE__))
      #define SEGGER_RTT_LOCK()   {                                                                   \
                                      unsigned int _SEGGER_RTT__LockState;                                         \
                                    __asm volatile ("mrs   %0, primask  \n\t"                         \
                                                    "movs  r1, #1       \n\t"                         \
                                                    "msr   primask, r1  \n\t"                         \
                                                    : "=r" (_SEGGER_RTT__LockState)                                \
                                                    :                                                 \
                                                    : "r1", "cc"                                      \
                                                    );

      #define SEGGER_RTT_UNLOCK()   __asm volatile ("msr   primask, %0  \n\t"                         \
                                                    :                                                 \
                                                    : "r" (_SEGGER_RTT__LockState)                                 \
                                                    :                                                 \
                                                    );                                                \
                                  }
    #elif (defined(__ARM_ARCH_7M__) || defined(__ARM_ARCH_7EM__) || defined(__ARM_ARCH_8M_MAIN__) || defined(__ARM_ARCH_8_1M_MAIN__))
      #ifndef   SEGGER_RTT_MAX_INTERRUPT_PRIORITY
        #define SEGGER_RTT_MAX_INTERRUPT_PRIORITY   (0x20)
      #endif
      #define SEGGER_RTT_LOCK()   {                                                                   \
                                      unsigned int _SEGGER_RTT__LockState;                                         \
                                    __asm volatile ("mrs   %0, basepri  \n\t"                         \
                                                    "mov   r1, %1       \n\t"                         \
                                                    "msr   basepri, r1  \n\t"                         \
                                                    : "=r" (_SEGGER_RTT__LockState)                                \
                                                    : "i"(SEGGER_RTT_MAX_INTERRUPT_PRIORITY)          \
                                                    : "r1", "cc"                                      \
                                                    );

      #define SEGGER_RTT_UNLOCK()   __asm volatile ("msr   basepri, %0  \n\t"                         \
                                                    :                                                 \
                                                    : "r" (_SEGGER_RTT__LockState)                                 \
                                                    :                                                 \
                                                    );                                                \
                                  }

    #elif (defined(__ARM_ARCH_7A__) || defined(__ARM_ARCH_7R__))
      #define SEGGER_RTT_LOCK() {                                                \
                                   unsigned int _SEGGER_RTT__LockState;                       \
                                   __asm volatile ("mrs r1, CPSR \n\t"           \
                                                   "mov %0, r1 \n\t"             \
                                                   "orr r1, r1, #0xC0 \n\t"      \
                                                   "msr CPSR_c, r1 \n\t"         \
                                                   : "=r" (_SEGGER_RTT__LockState)            \
                                                   :                             \
                                                   : "r1", "cc"                  \
                                                   );

      #define SEGGER_RTT_UNLOCK() __asm volatile ("mov r0, %0 \n\t"              \
                                                  "mrs r1, CPSR \n\t"            \
                                                  "bic r1, r1, #0xC0 \n\t"       \
                                                  "and r0, r0, #0xC0 \n\t"       \
                                                  "orr r1, r1, r0 \n\t"          \
                                                  "msr CPSR_c, r1 \n\t"          \
                                                  :                              \
                                                  : "r" (_SEGGER_RTT__LockState)              \
                                                  : "r0", "r1", "cc"             \
                                                  );                             \
                              }
    #elif defined(__riscv) || defined(__riscv_xlen)
      #define SEGGER_RTT_LOCK()  {                                               \
                                   unsigned int _SEGGER_RTT__LockState;                       \
                                   __asm volatile ("csrr  %0, mstatus  \n\t"     \
                                                   "csrci mstatus, 8   \n\t"     \
                                                   "andi  %0, %0,  8   \n\t"     \
                                                   : "=r" (_SEGGER_RTT__LockState)            \
                                                   :                             \
                                                   :                             \
                                                  );

    #define SEGGER_RTT_UNLOCK()    __asm volatile ("csrr  a1, mstatus  \n\t"     \
                                                   "or    %0, %0, a1   \n\t"     \
                                                   "csrs  mstatus, %0  \n\t"     \
                                                   :                             \
                                                   : "r"  (_SEGGER_RTT__LockState)            \
                                                   : "a1"                        \
                                                  );                             \
                                 }
    #endif
  #endif
#endif

/*********************************************************************
*
*       RTT lock configuration for IAR EWARM
*/
#if !defined(SEGGER_RTT_LOCK) || !defined (SEGGER_RTT_UNLOCK)
  #ifdef __ICCARM__
    #ifdef __IAR_SYSTEMS_ICC__
      #include <intrinsics.h>
    #endif
    #if (defined (__ARM6M__)          && (__CORE__ == __ARM6M__))             ||                      \
        (defined (__ARM8M_BASELINE__) && (__CORE__ == __ARM8M_BASELINE__))
      #define SEGGER_RTT_LOCK()   {                                                                   \
                                    unsigned int _SEGGER_RTT__LockState;                                           \
                                    _SEGGER_RTT__LockState = __get_PRIMASK();                                      \
                                    __set_PRIMASK(1);

      #define SEGGER_RTT_UNLOCK()   __set_PRIMASK(_SEGGER_RTT__LockState);                                         \
                                  }
    #elif (defined (__ARM7EM__)         && (__CORE__ == __ARM7EM__))          ||                      \
          (defined (__ARM7M__)          && (__CORE__ == __ARM7M__))           ||                      \
          (defined (__ARM8M_MAINLINE__) && (__CORE__ == __ARM8M_MAINLINE__))  ||                      \
          (defined (__ARM8M_MAINLINE__) && (__CORE__ == __ARM8M_MAINLINE__))
      #ifndef   SEGGER_RTT_MAX_INTERRUPT_PRIORITY
        #define SEGGER_RTT_MAX_INTERRUPT_PRIORITY   (0x20)
      #endif
      #define SEGGER_RTT_LOCK()   {                                                                   \
                                    unsigned int _SEGGER_RTT__LockState;                                           \
                                    _SEGGER_RTT__LockState = __get_BASEPRI();                                      \
                                    __set_BASEPRI(SEGGER_RTT_MAX_INTERRUPT_PRIORITY);

      #define SEGGER_RTT_UNLOCK()   __set_BASEPRI(_SEGGER_RTT__LockState);                                         \
                                  }
    #elif (defined (__ARM7A__) && (__CORE__ == __ARM7A__))                    ||                      \
          (defined (__ARM7R__) && (__CORE__ == __ARM7R__))
      #define SEGGER_RTT_LOCK() {                                                                     \
                                   unsigned int _SEGGER_RTT__LockState;                                            \
                                   __asm volatile ("mrs r1, CPSR \n\t"                                \
                                                   "mov %0, r1 \n\t"                                  \
                                                   "orr r1, r1, #0xC0 \n\t"                           \
                                                   "msr CPSR_c, r1 \n\t"                              \
                                                   : "=r" (_SEGGER_RTT__LockState)                                 \
                                                   :                                                  \
                                                   : "r1", "cc"                                       \
                                                   );

      #define SEGGER_RTT_UNLOCK() __asm volatile ("mov r0, %0 \n\t"                                   \
                                                  "mrs r1, CPSR \n\t"                                 \
                                                  "bic r1, r1, #0xC0 \n\t"                            \
                                                  "and r0, r0, #0xC0 \n\t"                            \
                                                  "orr r1, r1, r0 \n\t"                               \
                                                  "msr CPSR_c, r1 \n\t"                               \
                                                  :                                                   \
                                                  : "r" (_SEGGER_RTT__LockState)                                   \
                                                  : "r0", "r1", "cc"                                  \
                                                  );                                                  \
                              }
    #endif
  #endif
#endif

/*********************************************************************
*
*       RTT lock configuration for IAR RX
*/
#if !defined(SEGGER_RTT_LOCK) || !defined (SEGGER_RTT_UNLOCK)
  #ifdef __ICCRX__
    #ifdef __IAR_SYSTEMS_ICC__
      #include <intrinsics.h>
    #endif
    #define SEGGER_RTT_LOCK()   {                                                                     \
                                  unsigned long _SEGGER_RTT__LockState;                                            \
                                  _SEGGER_RTT__LockState = __get_interrupt_state();                                \
                                  __disable_interrupt();

    #define SEGGER_RTT_UNLOCK()   __set_interrupt_state(_SEGGER_RTT__LockState);                                   \
                                }
  #endif
#endif

/*********************************************************************
*
*       RTT lock configuration for IAR RL78
*/
#if !defined(SEGGER_RTT_LOCK) || !defined (SEGGER_RTT_UNLOCK)
  #ifdef __ICCRL78__
    #ifdef __IAR_SYSTEMS_ICC__
      #include <intrinsics.h>
    #endif
    #define SEGGER_RTT_LOCK()   {                                                                     \
                                  __istate_t _SEGGER_RTT__LockState;                                               \
                                  _SEGGER_RTT__LockState = __get_interrupt_state();                                \
                                  __disable_interrupt();

    #define SEGGER_RTT_UNLOCK()   __set_interrupt_state(_SEGGER_RTT__LockState);                                   \
                                }
  #endif
#endif

/*********************************************************************
*
*       RTT lock configuration for KEIL ARM
*/
#if !defined(SEGGER_RTT_LOCK) || !defined (SEGGER_RTT_UNLOCK)
  #ifdef __CC_ARM
    #if (defined __TARGET_ARCH_6S_M)
      #define SEGGER_RTT_LOCK()   {                                                                   \
                                    unsigned int _SEGGER_RTT__LockState;                                           \
                                    register unsigned char _SEGGER_RTT__PRIMASK __asm( "primask");                 \
                                    _SEGGER_RTT__LockState = _SEGGER_RTT__PRIMASK;                                              \
                                    _SEGGER_RTT__PRIMASK = 1u;                                                     \
                                    __schedule_barrier();

      #define SEGGER_RTT_UNLOCK()   _SEGGER_RTT__PRIMASK = _SEGGER_RTT__LockState;                                              \
                                    __schedule_barrier();                                             \
                                  }
    #elif (defined(__TARGET_ARCH_7_M) || defined(__TARGET_ARCH_7E_M))
      #ifndef   SEGGER_RTT_MAX_INTERRUPT_PRIORITY
        #define SEGGER_RTT_MAX_INTERRUPT_PRIORITY   (0x20)
      #endif
      #define SEGGER_RTT_LOCK()   {                                                                   \
                                    unsigned int _SEGGER_RTT__LockState;                                           \
                                    register unsigned char BASEPRI __asm( "basepri");                 \
                                    _SEGGER_RTT__LockState = BASEPRI;                                              \
                                    BASEPRI = SEGGER_RTT_MAX_INTERRUPT_PRIORITY;                      \
                                    __schedule_barrier();

      #define SEGGER_RTT_UNLOCK()   BASEPRI = _SEGGER_RTT__LockState;                                              \
                                    __schedule_barrier();                                             \
                                  }
    #endif
  #endif
#endif

/*********************************************************************
*
*       RTT lock configuration for TI ARM
*/
#if !defined(SEGGER_RTT_LOCK) || !defined (SEGGER_RTT_UNLOCK)
  #ifdef __TI_ARM__
    #if defined (__TI_ARM_V6M0__)
      #define SEGGER_RTT_LOCK()   {                                                                   \
                                    unsigned int _SEGGER_RTT__LockState;                                           \
                                    _SEGGER_RTT__LockState = __get_PRIMASK();                                      \
                                    __set_PRIMASK(1);

      #define SEGGER_RTT_UNLOCK()   __set_PRIMASK(_SEGGER_RTT__LockState);                                         \
                                  }
    #elif (defined (__TI_ARM_V7M3__) || defined (__TI_ARM_V7M4__))
      #ifndef   SEGGER_RTT_MAX_INTERRUPT_PRIORITY
        #define SEGGER_RTT_MAX_INTERRUPT_PRIORITY   (0x20)
      #endif
      #define SEGGER_RTT_LOCK()   {                                                                   \
                                    unsigned int _SEGGER_RTT__LockState;                                           \
                                    _SEGGER_RTT__LockState = _set_interrupt_priority(SEGGER_RTT_MAX_INTERRUPT_PRIORITY);

      #define SEGGER_RTT_UNLOCK()   _set_interrupt_priority(_SEGGER_RTT__LockState);                               \
                                  }
    #endif
  #endif
#endif

/*********************************************************************
*
*       RTT lock configuration for CCRX
*/
#if !defined(SEGGER_RTT_LOCK) || !defined (SEGGER_RTT_UNLOCK)
  #ifdef __RX
    #include <machine.h>
    #define SEGGER_RTT_LOCK()   {                                                                     \
                                  unsigned long _SEGGER_RTT__LockState;                                            \
                                  _SEGGER_RTT__LockState = get_psw() & 0x010000;                                   \
                                  clrpsw_i();

    #define SEGGER_RTT_UNLOCK()   set_psw(get_psw() | _SEGGER_RTT__LockState);                                     \
                                }
  #endif
#endif

/*********************************************************************
*
*       RTT lock configuration for embOS Simulation on Windows
*       (Can also be used for generic RTT locking with embOS)
*/

#if !defined(SEGGER_RTT_LOCK) || !defined (SEGGER_RTT_UNLOCK)
  #if defined(WIN32) || defined(SEGGER_RTT_LOCK_EMBOS)

    void OS_SIM_EnterCriticalSection(void);
    void OS_SIM_LeaveCriticalSection(void);

    #define SEGGER_RTT_LOCK()       {                                                                   \
                                      OS_SIM_EnterCriticalSection();

    #define SEGGER_RTT_UNLOCK()       OS_SIM_LeaveCriticalSection();                                    \
                                    }
  #endif
#endif

/*********************************************************************
*
*       RTT lock configuration fallback
*/
#ifndef   SEGGER_RTT_LOCK
  #define SEGGER_RTT_LOCK()                // Lock RTT (nestable)   (i.e. disable interrupts)
#endif

#ifndef   SEGGER_RTT_UNLOCK
  #define SEGGER_RTT_UNLOCK()              // Unlock RTT (nestable) (i.e. enable previous interrupt lock state)
#endif

/*********************************************************************
*
*       If SEGGER_RTT_SECTION is defined but SEGGER_RTT_BUFFER_SECTION
*       is not, use the same section for SEGGER_RTT_BUFFER_SECTION.
*/
#ifndef SEGGER_RTT_BUFFER_SECTION
  #if defined(SEGGER_RTT_SECTION)
    #define SEGGER_RTT_BUFFER_SECTION SEGGER_RTT_SECTION
  #endif
#endif

#endif
/*************************** End of file ****************************/
