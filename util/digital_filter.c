/*
	Copyright 2016 Benjamin Vedder	benjamin@vedder.se

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

#include  "digital_filter.h"
#include  <math.h>
#include  <stdint.h>

// Found at http://paulbourke.net/miscellaneous//dft/
void filter_fft(int dir, int m, float *real, float *imag) {
	long n,i,i1,j,k,i2,l,l1,l2;
	float c1,c2,tx,ty,t1,t2,u1,u2,z;

	// Calculate the number of points
	n = 1 << m;

	// Do the bit reversal
	i2 = n >> 1;
	j = 0;
	for (i=0;i<n-1;i++) {
		if (i < j) {
			tx = real[i];
			ty = imag[i];
			real[i] = real[j];
			imag[i] = imag[j];
			real[j] = tx;
			imag[j] = ty;
		}
		k = i2;
		while (k <= j) {
			j -= k;
			k >>= 1;
		}
		j += k;
	}

	// Compute the FFT
	c1 = -1.0;
	c2 = 0.0;
	l2 = 1;
	for (l=0;l<m;l++) {
		l1 = l2;
		l2 <<= 1;
		u1 = 1.0;
		u2 = 0.0;
		for (j=0;j < l1;j++) {
			for (i=j;i < n;i += l2) {
				i1 = i + l1;
				t1 = u1 * real[i1] - u2 * imag[i1];
				t2 = u1 * imag[i1] + u2 * real[i1];
				real[i1] = real[i] - t1;
				imag[i1] = imag[i] - t2;
				real[i] += t1;
				imag[i] += t2;
			}
			z =  u1 * c1 - u2 * c2;
			u2 = u1 * c2 + u2 * c1;
			u1 = z;
		}
		c2 = sqrtf((1.0 - c1) / 2.0);
		if (dir) {
			c2 = -c2;
		}
		c1 = sqrtf((1.0 + c1) / 2.0);
	}

	// Scaling for reverse transform
	if (dir) {
		for (i=0;i < n;i++) {
			real[i] /= n;
			imag[i] /= n;
		}
	}
}

// Found at http://paulbourke.net/miscellaneous//dft/
void filter_dft(int dir, int len, float *real, float *imag) {
	long i,k;
	float arg;
	float cosarg, sinarg;

	if(dir) {
		dir = 1;
	} else {
		dir = -1;
	}

	float x2[len];
	float y2[len];

	for (i=0;i < len;i++) {
		x2[i] = 0;
		y2[i] = 0;
		arg = -(float)dir * 2.0 * M_PI * (float)i / (float)len;
		for (k=0;k<len;k++) {
			cosarg = cosf(k * arg);
			sinarg = sinf(k * arg);
			x2[i] += (real[k] * cosarg - imag[k] * sinarg);
			y2[i] += (real[k] * sinarg + imag[k] * cosarg);
		}
	}

	// Copy the data back
	if (dir == 1) {
		for (i=0;i<len;i++) {
			real[i] = x2[i] / (float)len;
			imag[i] = y2[i] / (float)len;
		}
	} else {
		for (i=0;i<len;i++) {
			real[i] = x2[i];
			imag[i] = y2[i];
		}
	}
}

void filter_fftshift(float *data, int len) {
	for (int i = 0;i < (len / 2);i++) {
		float r1 = data[i];
		float r2 = data[len/2 + i];

		data[i] = r2;
		data[len / 2 + i] = r1;
	}
}

void filter_hamming(float *data, int len) {
	if (len % 2 == 0) {
		for (int i = 0;i < (len / 2);i++) {
			float val = 0.54 - 0.46 * cosf((2.0 * M_PI * (float)i)/(float)(len - 1));
			data[i] *= val;
			data[len - i - 1] *= val;
		}
	} else {
		for (int i = 0;i < len;i++) {
			data[i] *= 0.54 - 0.46 * cosf((2.0 * M_PI * (float)i)/(float)(len - 1));
		}
	}
}

void filter_zeroPad(float *data, float *result, int dataLen, int resultLen) {
	for (int i = 0;i < resultLen;i++) {
		if (i < dataLen) {
			result[i] = data[i];
		} else {
			result[i] = 0;
		}
	}
}

void filter_create_fir_lowpass(float *filter_vector, float f_break, int bits, int use_hamming) {
	int taps = 1 << bits;
	float imag[taps];

	for(int i = 0;i < taps;i++) {
		if (i < (int)((float)taps * f_break)) {
			filter_vector[i] = 1;
		} else {
			filter_vector[i] = 0;
		}
		imag[i] = 0;
	}

	// Make filter symmetric
	for (int i = 0;i < taps / 2;i++) {
		filter_vector[taps - i - 1] = filter_vector[i];
	}

	filter_fft(1, bits, filter_vector, imag);
	filter_fftshift(filter_vector, taps);

	if (use_hamming) {
		filter_hamming(filter_vector, taps);
	}
}

/*
 * Run FIR filter iteration.
 *
 * bits: A power of two representing the length of the filter
 * filter: The FIR filter coefficients
 * offset: an offset into the vector buffer. Will wrap around when going past
 * length while filtering. Useful for keeping a circular buffer with samples
 * and avoiding to shift the whole buffer.
 *
 * returns: The filtered result sample.
 */
float filter_run_fir_iteration(float *vector, float *filter, int bits, uint32_t offset) {
	float result = 0;
	int size = 1 << bits;
	uint32_t cnt_mask = 0xFFFFFFFF >> (32 - bits);

	for (int i = 0;i < size;i++) {
		result += filter[i] * vector[offset];
		offset++;
		offset &= cnt_mask;
	}

	return result;
}

/**
 * Add sample to buffer
 * @param buffer
 * The buffer to add the sample to
 * @param sample
 * The sample to add
 * @param bits
 * The length of the buffer in bits
 * @param offset
 * Pointer to the current offset in the buffer. Will be updated in this call
 * and wrapped at the length of this buffer.
 */
void filter_add_sample(float *buffer, float sample, int bits, uint32_t *offset) {
	uint32_t cnt_mask = 0xFFFFFFFF >> (32 - bits);
	buffer[*offset] = sample;
	*offset += 1;
	*offset &= cnt_mask;
}

/**
 * Biquad filter
 */
float biquad_process(Biquad *biquad, float in) {
    float out = in * biquad->a0 + biquad->z1;
    biquad->z1 = in * biquad->a1 + biquad->z2 - biquad->b1 * out;
    biquad->z2 = in * biquad->a2 - biquad->b2 * out;
    return out;
}
void biquad_config(Biquad *biquad, BiquadType type, float Fc) {
	float K = tanf(M_PI * Fc);	// -0.0159;
	float Q = 0.707; // maximum sharpness (0.5 = maximum smoothness)
	float norm = 1 / (1 + K / Q + K * K);
	if (type == BQ_LOWPASS) {
		biquad->a0 = K * K * norm;
		biquad->a1 = 2 * biquad->a0;
		biquad->a2 = biquad->a0;
	}
	else if (type == BQ_HIGHPASS) {
		biquad->a0 = 1 * norm;
		biquad->a1 = -2 * biquad->a0;
		biquad->a2 = biquad->a0;
	}
	biquad->b1 = 2 * (K * K - 1) * norm;
	biquad->b2 = (1 - K / Q + K * K) * norm;
}
void biquad_reset(Biquad *biquad) {
	biquad->z1 = 0;
	biquad->z2 = 0;
}
