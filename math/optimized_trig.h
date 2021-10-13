/*
	Copyright 2021 - Kenn Sebesta

	This file is part of the VESC firmware.

	The VESC firmware is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    The VESC firmware is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
    */

#ifndef OPTIMIZED_TRIG_H_
#define OPTIMIZED_TRIG_H_

#include <stdbool.h>
#include <stdint.h>
#include <math.h>
#include <stdlib.h>

/**
 * @brief cosine6TaylorApproximation Fixed-point 6th-order taylor approximation which returns cosine(x)
 * @param AQ     aq * 2^qN
 * @param BQm   -bq * 2^qN
 * @param CQ     cq * 2^qN
 * @param DQm   -dq * 2^(16 + qN - 2*qX)
 * @param X2    x^2 * 2^(2qX)
 * @param X2_16 x^2 * 2^16
 * @return
 */
__attribute__((always_inline))
inline static int32_t cosine6TaylorApproximation(const int32_t AQ, const int32_t BQm, const int32_t CQ, const int32_t DQm, const int32_t X2, const int32_t X2_16) {
   /*
    * out = a - bx^2 + cx^4 - dx^6
    *     = a - x^2*((b - x^2*(c - d*x^2))
    *     = a - x^2*(((-bm) - x^2*(c - (-dm)*x^2))
    *     = a + x^2*(bm + x^2*(c + dm*x^2))
    */

   int32_t output;
#if __thumb2__  // Only compile this branch when the thumb instructions are present
   // ARM THUMB-2 assembly blob which takes advantage of the SMLAWb operator, which performs a multily and accumulate
   asm ("SMLAWb %[Y3], %[X2], %[D], %[C]\n"       // (y1 = cq  + dqm*(x)^2)
        "SMLAWb %[Y3], %[Y3], %[X2_16], %[B]\n"   // (y2 = bqm +  y1*(x16)^2)
        "SMLAWb %[Y3], %[Y3], %[X2_16], %[A]\n"   // (y3 = aq  +  y2*(x16)^2)
        : [Y3] "=&r" (output)
        : [A] "r" (AQ), [B] "r" (BQm), [C] "r" (CQ), [D] "r" (DQm), [X2] "r" (X2), [X2_16] "r" (X2_16)
        :);

#else
   int Y3=(((int64_t)X2*DQm)>>16)+CQ; // (y1=c-d*x^2)
   Y3=(((int64_t)Y3*X2_16)>>16)+BQm; // (y2=-(b-y1*x16^2))
   Y3=(((int64_t)Y3*X2_16)>>16)+AQ; // (y3=y2*x16^2+a)
   output = Y3;

#endif

   return output;
}


/**
 * @brief _SinCos66_FP This function uses a 6th order taylor expansion for atomically calculating
 *                     sine(x) and cos(x). It uses fixed-point calculations, and thus is sensitive
 *                     to the scale of the arguments. Please see the below notes.
 *
 *                     Coefficients were chosen by setting:
 *                       *) cos(0) = 1
 *                       *) cos(pi/2) = 0
 *                       *) d/dx[cos(x=pi/2)] = -1
 *                       *) Minimizing the root-mean square error
 *
 *                     For the system
 *
 *                        cos(x) ~= a - x^2*((b - x^2*(c - d*x^2))
 *
 *                     the following constants are found through closed-form analysis
 *
 *                        G=pi/2
 *                        a=1
 *                        b = (-45045/G^4*(G^6+2*G^4-156*G^2+360)/(G^3) - (-655*G-4504))/(952*G^2);  // 0.499833652837794
 *                        c = 1/(2*G^4)*(G-6 + 4*b*G^2);  // 0.041388888146338884
 *                        d = 1/(G^6)*(G/2 - 2 + b*G^2);  // 0.0012440842722145661
 *
 *                     Sine is calculated with the same equaiton by recognizing that sin(x) = cos(x+pi/2)
 *
 *                     A transformation of z = x/pi is used to map the circle into the field
 *                     of [-1, 1), which leads itself to easy fixed-point processing.
 *
 *
 *                     NOTE: Only arguments between (-2*pi and +2pi) are valid. Values outside
 *                           that range will cause integer overflow and the algorithm will silently
 *                           fail.
 *                     NOTENOTE: Please read the above note on the valid range.
 *                     NOTENOTENOTE: Srsly, only values in [pi, -pi) should be used, and never, ever
 *                                   exceed (-2*pi+1) and (2*pi-1).
 * @param angle_rad[float] The angle in [radians], in the range of [-pi,pi)
 * @param sin_out[float] The 6th order approximation of sin(x)
 * @param cos_out[float] The 6th order approximation of cos(x)
 */
__attribute__((always_inline))
inline static void _SinCos66_FP(const float angle_rad, float *sin_out, float *cos_out) {
   const uint8_t qN = 28;
   const uint8_t qX = 16;
   const uint8_t qQ = 30;

   const float PI_INVERSE = 1.0f/M_PI;  // Forces the compiler to precompute the inverse of PI
   const int32_t FP_PI_Q = (1<<qQ);  // qQ
   const int32_t FP_PI_HALVES_Q = FP_PI_Q>>1; // qQ
   const int32_t FP_PI = (1<<qX);  // qX
   const int32_t FP_PI_HALVES = FP_PI>>1;  // qX

   // This is the point, in [%/100], where the 1st order approximation is more
   // accurate than the 6th order fixed-point approximation.
   const float approximationCrossoverPoint = 0.9658377177;

   const double G=M_PI/2;
   const double G2=G*G;
   const double G3=G2*G;
   const double G4=G2*G2;
   const double G6=G4*G2;

   // a - x^2*((b - x^2*(c - d*x^2))
   // Coefficients found by minimizing the RMS error, and ensuring that the sine wave passes perfectly by 0 and 1.
   const double a=1;
   const double b = (-45045/G4*(G6+2*G4-156*G2+360)/(G3) - (-655*G-4504))/(952*G2); //# 4.933160419860462,;  // 0.499833652837794
   const double c = 1/(2*G4)*(G - 6 + 4*b*G2);  // 0.041388888146338884
   const double d = 1/(G6)*(G/2 - 2 + b*G2);  // 0.0012440842722145661

   // Transform x to q, where q = x/pi
   // aq - q^2*((bq - q^2*(cq - dq*q^2))
   const double aq = a;
   const double bq = b * G2*4;
   const double cq = c * G4*16;
   const double dq = d * G6*64;

   // Convert the coefficients to fixed point, and inject a negative sign for certain ones. This negative
   // sign will be useful when using the multiply & accumulate assembly instruction.
   const int32_t AQ = aq * (1UL<<qN);  // qN
   const int32_t BQm = -(bq * (1UL<<qN) + 0.5L);  // qN
   const int32_t CQ = cq * (1UL<<qN) + 0.5L;  // qN
   const int32_t DQm = -(dq * (1UL<<((48-(32-qN)-2*qX))) + 0.5L);  // ((16 + qN - 2*qX)))

   // Convert the angle to fixed-point.
   int32_t angle_FP = (angle_rad*PI_INVERSE)  * (1<<qQ); //  Do not round, as it is important not to reach +2^15

   // By checking thebit in the PI place, we can determine if the angle is in the 3rd or 4th quadrant
   bool is3rdOr4thQuadrant = angle_FP & FP_PI_Q;

   // By checking whether there is a bit in the PI/2 place, we can determine if the angle is in the 2nd or 4th quadrant
   bool is2ndOr4thQuadrant = angle_FP & FP_PI_HALVES_Q;

   // Use the absolute value since cos(x) = cos(-x). Use a mask to truncate the value between 0 and pi.
   int angle_truncated_FP = abs(angle_FP) & (FP_PI_Q-1);  // qQ

   // Handle the special case of n*pi/2, which causes trouble for integer overflow
   if (angle_truncated_FP == FP_PI_HALVES_Q) {
      // Sine(+-pi/2) changes sign based on the quadrant
      if (is3rdOr4thQuadrant == true) {
         *sin_out = -1;
      } else {
         *sin_out = 1;
      }

      *cos_out = 0;
      return;
   }

   // Use symmetry to calculate across [pi/2, pi). This is handy because the taylor coefficients are optimized for in this range.
   const int32_t Q = angle_truncated_FP < FP_PI_HALVES_Q ? angle_truncated_FP : FP_PI_Q - angle_truncated_FP;  // qQ

   // Shift X for proper scaling for use in the fixed-point math
   int32_t X = Q >> (qQ-qX);  //qX

   // Cosine(x). This MUST be done first
   {
      int32_t output;  // qN

      // If the angle is close to pi/2, then use the simple approximation cos(pi/2-eps) ~= pi/2-eps
      if (Q > (int32_t)(FP_PI_HALVES_Q * approximationCrossoverPoint)) {
         // Ensure angle is in [-pi,pi)
         while (angle_rad < -M_PI) { angle_rad += 2.0 * M_PI; }
         while (angle_rad >  M_PI) { angle_rad -= 2.0 * M_PI; }

         // Check if the original angle is in the 3rd or 4th quadrant
         if (is3rdOr4thQuadrant == true) {
            cos_out = M_PI/2+angle_rad;  // = -(-M_PI/2-angle_rad)
         } else {
            cos_out = M_PI/2-angle_rad;
         }
      } else {
         // --Calculate the 6th order approximation of cos(x)--//

         int32_t X2 = X*X;                 // 2*qX
         int32_t X2_16 = X2 >> (2*qX-16);  // 16

         output = cosine6TaylorApproximation(AQ, BQm, CQ, DQm, X2, X2_16);

         // Check if the original angle is in the 2nd or 3rd quadrant
         if ((is2ndOr4thQuadrant && !is3rdOr4thQuadrant) || // the 2nd quadrant
             (!is2ndOr4thQuadrant && is3rdOr4thQuadrant)) { // the 3rd quadrant
            output = -output;
         }

         // Scale the output from qN to a float
#if __thumb2__  // Only compile this branch when the thumb instructions are present
         *cos_out = (float)output / (1<<qN);
#else
         const float den = 1/(1<<qN);
         *cos_out = output * den;
#endif
      }
   }

   // Sine *must* be done after cosine. This is because the sine routine modifies `X`.
   {
      int32_t output = -1;

      // If the angle is close to 0, then use the simple approximation sin(eps) ~= eps
      if (Q < (int32_t)(FP_PI_HALVES_Q * (1 - approximationCrossoverPoint))) {
         // Ensure angle is in [-pi,pi)
         while (angle_rad < -M_PI) { angle_rad += 2.0 * M_PI; }
         while (angle_rad >  M_PI) { angle_rad -= 2.0 * M_PI; }

         if ((is2ndOr4thQuadrant && !is3rdOr4thQuadrant) || // the 2nd quadrant
             (!is2ndOr4thQuadrant && is3rdOr4thQuadrant)) { // the 3rd quadrant
            sin_out = -M_PI + angle_rad;
         } else {
            sin_out = angle_rad;
         }
      } else {
         // --Calculate the 6th order approximation of sin(x)--//

         // Subtract pi/2, exploiting sin(x) = cos(x-pi/2)
         X -= FP_PI_HALVES;

         int32_t X2 = X*X;                 // 2*qX
         int32_t X2_16 = X2 >> (2*qX-16);  // 16

         output = cosine6TaylorApproximation(AQ, BQm, CQ, DQm, X2, X2_16);

         // Check if the original angle is in the 3rd or 4th quadrant
         if (is3rdOr4thQuadrant == true) {
            output = -output;
         }


         // Scale the output from qN to a float
#if __thumb2__  // Only compile this branch when the thumb instructions are present
         *sin_out = (float)output / (1<<qN);
#else
         const float den = 1/(1<<qN);
         *sin_out = output * den;
#endif
      }
   }
}

#endif   // OPTIMIZED_TRIG_H_
