/*
    Copyright 2025 Joel Svensson        svenssonjoel@yahoo.se

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef DSP_EXTENSIONS_H_
#define DSP_EXTENSIONS_H_

#ifdef __cplusplus
extern "C" {
#endif

void lbm_dsp_extensions_init(void);


// For internal use
void lbm_fft(float *real, float *imag, int n, int inverse);

void lbm_complex_convolve(float *signal_re,
                          float *signal_im,
                          unsigned int signal_len,
                          float *filter_re,
                          float *filter_im,
                          unsigned int filter_len,
                          float *output_re,
                          float *output_im,
                          unsigned int output_len,
                          bool swap_byte_order);

#ifdef __cplusplus
}
#endif
#endif

