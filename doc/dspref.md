# LispBM DSP Extensions Reference Manual

The DSP extensions provide digital signal processing functions such as correlation, convolution and fast Fourier transform. These extensions may or may not be present depending on the platform and configuration of LispBM. 

All DSP functions operate on byte arrays containing 32-bit floats. The byte order of float data in the arrays can be specified with the `'little-endian` symbol. If not specified, big-endian is assumed. On most embedded platforms (ARM, x86) floats are stored in little-endian byte order, so `'little-endian` should typically be passed. 

## Real-valued Operations


### correlate

`correlate` computes the cross-correlation of two real-valued signals. The form is `(correlate s1 s2)` or `(correlate s1 s2 'little-endian)`. Both `s1` and `s2` must be byte arrays containing 32-bit floats. Returns a new byte array of floats with length `(+ (len s1) (len s2) -1)`, where lengths are measured in number of floats. Cross-correlation measures the similarity between two signals as a function of the time-lag applied to one of them. 

The optional `'little-endian` argument specifies the byte order of the float data in the input arrays. If omitted the data is assumed to be big-endian. On most embedded systems floats are stored in little-endian format, so `'little-endian` should typically be passed. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>


```clj
(define s1 (bufcreate 16))
(bufset-f32 s1 0 1.000000f32 'little-endian)
(bufset-f32 s1 4 2.000000f32 'little-endian)
(bufset-f32 s1 8 3.000000f32 'little-endian)
(bufset-f32 s1 12 4.000000f32 'little-endian)
(define s2 (bufcreate 8))
(bufset-f32 s2 0 1.000000f32 'little-endian)
(bufset-f32 s2 4 1.000000f32 'little-endian)
(correlate s1 s2 'little-endian)
```


</td>
<td>


```clj
[0 0 64 64 0 0 160 64 0 0 224 64 0 0 128 64 0 0 0 0]
```


</td>
</tr>
<tr>
<td>


```clj
(define s1 (bufcreate 16))
(bufset-f32 s1 0 1.000000f32)
(bufset-f32 s1 4 2.000000f32)
(bufset-f32 s1 8 3.000000f32)
(bufset-f32 s1 12 4.000000f32)
(define s2 (bufcreate 8))
(bufset-f32 s2 0 1.000000f32)
(bufset-f32 s2 4 1.000000f32)
(correlate s1 s2)
```


</td>
<td>


```clj
[64 64 0 0 64 160 0 0 64 224 0 0 64 128 0 0 0 0 0 0]
```


</td>
</tr>
</table>




---


### convolve

`convolve` computes the convolution of a signal with a filter kernel. The form is `(convolve signal filter)` or `(convolve signal filter 'little-endian)`. Both arguments must be byte arrays containing 32-bit floats. Returns a new byte array of floats with length `(+ (len signal) (len filter) -1)`, where lengths are measured in number of floats. Convolution is commonly used for applying FIR filters to signals. 

The optional `'little-endian` argument specifies the byte order of the float data in the input arrays. If omitted the data is assumed to be big-endian. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>


```clj
(define signal (bufcreate 16))
(bufset-f32 signal 0 1.000000f32 'little-endian)
(bufset-f32 signal 4 2.000000f32 'little-endian)
(bufset-f32 signal 8 3.000000f32 'little-endian)
(bufset-f32 signal 12 4.000000f32 'little-endian)
(define kernel (bufcreate 8))
(bufset-f32 kernel 0 0.500000f32 'little-endian)
(bufset-f32 kernel 4 0.500000f32 'little-endian)
(convolve signal kernel 'little-endian)
```


</td>
<td>


```clj
[0 0 0 63 0 0 192 63 0 0 32 64 0 0 96 64 0 0 0 64]
```


</td>
</tr>
<tr>
<td>


```clj
(define signal (bufcreate 16))
(bufset-f32 signal 0 1.000000f32)
(bufset-f32 signal 4 2.000000f32)
(bufset-f32 signal 8 3.000000f32)
(bufset-f32 signal 12 4.000000f32)
(define kernel (bufcreate 8))
(bufset-f32 kernel 0 0.500000f32)
(bufset-f32 kernel 4 0.500000f32)
(convolve signal kernel)
```


</td>
<td>


```clj
[63 0 0 0 63 192 0 0 64 32 0 0 64 96 0 0 64 0 0 0]
```


</td>
</tr>
</table>




---

## Complex-valued Operations


### complex-correlate

`complex-correlate` computes the cross-correlation of two complex-valued signals. The form is `(complex-correlate s1-re s1-im s2-re s2-im)` or with an optional `'little-endian` as a fifth argument. All four arguments must be byte arrays of equal length containing 32-bit floats, representing the real and imaginary parts of the two signals. Returns a cons pair `(output-re . output-im)` of byte arrays. The correlation uses the conjugate of `s2`, consistent with the standard definition of complex cross-correlation. 

The real and imaginary arrays of each signal must have the same length. The optional `'little-endian` argument specifies the byte order of the float data. 




---


### complex-convolve

`complex-convolve` computes the convolution of two complex-valued signals. The form is `(complex-convolve sig-re sig-im fil-re fil-im)` or with an optional `'little-endian` as a fifth argument. All four arguments must be byte arrays of equal length containing 32-bit floats, representing the real and imaginary parts of the signal and filter. Returns a cons pair `(output-re . output-im)` of byte arrays. 

The real and imaginary arrays of each input must have the same length. The optional `'little-endian` argument specifies the byte order of the float data. 




---

## Fourier Transform


### fft

`fft` computes the Fast Fourier Transform of a signal. The form is `(fft real-arr imag-arr)` with optional additional arguments. Both `real-arr` and `imag-arr` must be byte arrays of equal size containing 32-bit floats, representing the real and imaginary parts of the input signal. Returns a cons pair `(real-output . imag-output)` of byte arrays. 

If the input length is not a power of two, the arrays are zero-padded to the next power of two before the transform is applied. The output length will therefore be at least as large as the input. 

Optional arguments: 

   - Pass `'inverse` to compute the inverse FFT. The result is scaled by 1/N.
   - Pass `'little-endian` to specify that float data is in little-endian byte order.
   - Both can be combined: `(fft re im 'inverse 'little-endian)`.

For a real-valued signal, set all values in `imag-arr` to zero. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>


```clj
(define n 8)
(define re (bufcreate (* n 4)))
(define im (bufcreate (* n 4)))
(bufset-f32 re 0 1.000000f32 'little-endian)
(bufset-f32 re 4 1.000000f32 'little-endian)
(bufset-f32 re 8 1.000000f32 'little-endian)
(bufset-f32 re 12 1.000000f32 'little-endian)
(define result (fft re im 'little-endian))
(car result)
```


</td>
<td>


```clj
[0 0 128 64 254 255 127 63 0 0 0 0 0 0 128 63 0 0 0 0 0 0 128 63 0 0 0 0 0 0 128 63]
```


</td>
</tr>
<tr>
<td>


```clj
(define n 8)
(define re (bufcreate (* n 4)))
(define im (bufcreate (* n 4)))
(bufset-f32 re 0 1.000000f32)
(bufset-f32 re 4 1.000000f32)
(bufset-f32 re 8 1.000000f32)
(bufset-f32 re 12 1.000000f32)
(define result (fft re im))
(car result)
```


</td>
<td>


```clj
[64 128 0 0 63 127 255 254 0 0 0 0 63 128 0 0 0 0 0 0 63 128 0 0 0 0 0 0 63 128 0 0]
```


</td>
</tr>
</table>




---

This document was generated by LispBM version 0.36.0 

