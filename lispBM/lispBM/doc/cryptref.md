# LispBM Crypto Extensions Reference Manual

The crypto extensions provide cryptographic hash and block cipher primitives. These are low-level building blocks — padding, block chaining modes and higher-level constructions are left to the caller. These extensions may or may not be present depending on the platform and configuration of LispBM. 

## SHA-256


### sha256

`sha256` computes the SHA-256 hash of a byte array. The form of a `sha256` expression is `(sha256 buf)` where `buf` is a byte array of any length. Returns a 32-byte array containing the hash. Specified in FIPS PUB 180-4. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(sha256 [])
```


</td>
<td>

```clj
[227 176 196 66 152 252 28 20 154 251 244 200 153 111 185 36 39 174 65 228 100 155 147 76 164 149 153 27 120 82 184 85]
```


</td>
</tr>
<tr>
<td>

```clj
(sha256 [1 2 3])
```


</td>
<td>

```clj
[3 144 88 198 242 192 203 73 44 83 59 10 77 20 239 119 204 15 120 171 204 206 213 40 125 132 161 162 1 28 251 129]
```


</td>
</tr>
<tr>
<td>

```clj
(bytes-to-hex (sha256 []))
```


</td>
<td>

```clj
"e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
```


</td>
</tr>
<tr>
<td>

```clj
(bytes-to-hex (sha256 [1 2 3]))
```


</td>
<td>

```clj
"039058c6f2c0cb492c533b0a4d14ef77cc0f78abccced5287d84a1a2011cfb81"
```


</td>
</tr>
</table>




---


### sha256-str

`sha256-str` computes the SHA-256 hash of a string. The form of a `sha256-str` expression is `(sha256-str str)` where `str` is a string. The null terminator is not included in the hash. Returns a 32-byte array containing the hash. Specified in FIPS PUB 180-4. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(bytes-to-hex (sha256-str [0]))
```


</td>
<td>

```clj
"e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
```


</td>
</tr>
<tr>
<td>

```clj
(bytes-to-hex (sha256-str "abc"))
```


</td>
<td>

```clj
"ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
```


</td>
</tr>
<tr>
<td>

```clj
(bytes-to-hex (sha256-str "hello world"))
```


</td>
<td>

```clj
"b94d27b9934d3e08a52e52d7da7dabfac484efe37a5380ee9088f7ace2efcde9"
```


</td>
</tr>
</table>




---

## AES Block Cipher


### aes128-enc

`aes128-enc` encrypts a single 16-byte block using AES-128. The form of an `aes128-enc` expression is `(aes128-enc key data)` where `key` is a 16-byte array and `data` is a 16-byte array. Returns a 16-byte array containing the encrypted block. This is a raw block cipher — the caller is responsible for padding and any block chaining mode. Specified in FIPS 197. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(bytes-to-hex (aes128-enc [43 126 21 22 40 174 210 166 171 247 21 136 9 207 79 60] [50 67 246 168 136 90 48 141 49 49 152 162 224 55 7 52]))
```


</td>
<td>

```clj
"3925841d02dc09fbdc118597196a0b32"
```


</td>
</tr>
</table>




---


### aes128-dec

`aes128-dec` decrypts a single 16-byte block using AES-128. The form of an `aes128-dec` expression is `(aes128-dec key data)` where `key` is a 16-byte array and `data` is a 16-byte array. Returns a 16-byte array containing the decrypted block. This is a raw block cipher — the caller is responsible for unpadding and any block chaining mode. Specified in FIPS 197. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(bytes-to-hex (aes128-dec [43 126 21 22 40 174 210 166 171 247 21 136 9 207 79 60] [57 37 132 29 2 220 9 251 220 17 133 151 25 106 11 50]))
```


</td>
<td>

```clj
"3243f6a8885a308d313198a2e0370734"
```


</td>
</tr>
</table>




---


### aes256-enc

`aes256-enc` encrypts a single 16-byte block using AES-256. The form of an `aes256-enc` expression is `(aes256-enc key data)` where `key` is a 32-byte array and `data` is a 16-byte array. Returns a 16-byte array containing the encrypted block. This is a raw block cipher — the caller is responsible for padding and any block chaining mode. Specified in FIPS 197. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(bytes-to-hex (aes256-enc [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31] [0 17 34 51 68 85 102 119 136 153 170 187 204 221 238 255]))
```


</td>
<td>

```clj
"8ea2b7ca516745bfeafc49904b496089"
```


</td>
</tr>
</table>




---


### aes256-dec

`aes256-dec` decrypts a single 16-byte block using AES-256. The form of an `aes256-dec` expression is `(aes256-dec key data)` where `key` is a 32-byte array and `data` is a 16-byte array. Returns a 16-byte array containing the decrypted block. This is a raw block cipher — the caller is responsible for unpadding and any block chaining mode. Specified in FIPS 197. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(bytes-to-hex (aes256-dec [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31] [142 162 183 202 81 103 69 191 234 252 73 144 75 73 96 137]))
```


</td>
<td>

```clj
"00112233445566778899aabbccddeeff"
```


</td>
</tr>
</table>




---

## Utility functions


### bytes-to-hex

`bytes-to-hex` converts a byte array to a lowercase hexadecimal string. The form of a `bytes-to-hex` expression is `(bytes-to-hex buf)` where `buf` is a byte array. Each byte is represented as exactly two hex digits. Returns a string of length `2 * (buflen buf)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(bytes-to-hex [222 173 190 239])
```


</td>
<td>

```clj
"deadbeef"
```


</td>
</tr>
<tr>
<td>

```clj
(bytes-to-hex [0 1 2 3 255])
```


</td>
<td>

```clj
"00010203ff"
```


</td>
</tr>
<tr>
<td>

```clj
(bytes-to-hex (sha256-str "abc"))
```


</td>
<td>

```clj
"ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
```


</td>
</tr>
</table>




---


### hex-to-bytes

`hex-to-bytes` converts a lowercase or uppercase hexadecimal string to a byte array. The form of a `hex-to-bytes` expression is `(hex-to-bytes str)` where `str` is a string containing an even number of hex digits. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(hex-to-bytes "deadbeef")
```


</td>
<td>

```clj
[222 173 190 239]
```


</td>
</tr>
<tr>
<td>

```clj
(hex-to-bytes "DEADBEEF")
```


</td>
<td>

```clj
[222 173 190 239]
```


</td>
</tr>
<tr>
<td>

```clj
(hex-to-bytes "000102ff")
```


</td>
<td>

```clj
[0 1 2 255]
```


</td>
</tr>
<tr>
<td>

```clj
(hex-to-bytes (bytes-to-hex (sha256-str "abc")))
```


</td>
<td>

```clj
[186 120 22 191 143 1 207 234 65 65 64 222 93 174 34 35 176 3 97 163 150 23 122 156 180 16 255 97 242 0 21 173]
```


</td>
</tr>
</table>




---

## Bignum Library

The bignum library is meant as an entry point for implementation of, for example, RSA Crypto. This can be used for encrypted data transfers or for cryptographically signed files for Integrity and Authenticity! 


### bn-add

`bn-add` adds two bignum values The form of a `bn-add` expression is `(bn-add a b)` where `a` and `b` are bignum. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(bn-add a b)
```


</td>
<td>

```clj
[102 0 1 0 0 0 0 0]
```


</td>
</tr>
<tr>
<td>

```clj
(bn-add (bn-from-u32 4294967295u32) (bn-from-u32 102))
```


</td>
<td>

```clj
[101 0 0 0 1 0 0 0]
```


</td>
</tr>
<tr>
<td>

```clj
(bn-add (bn-from-bytes (hex-to-bytes "FFFFFFFF")) (bn-from-u32 102))
```


</td>
<td>

```clj
[101 0 0 0 1 0 0 0]
```


</td>
</tr>
</table>



---


### bn-cmp

`bn-cmp` compares two bignum values and return 1,0 or -1 depending on if the first argument is larger, equal, smaller than the second. The form of a `bn-cmp` expression is `(bn-cmp a b)` where `a` and ´b` are bignum. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(bn-cmp a b)
```


</td>
<td>

```clj
-1
```


</td>
</tr>
<tr>
<td>

```clj
(bn-cmp b a)
```


</td>
<td>

```clj
1
```


</td>
</tr>
<tr>
<td>

```clj
(bn-cmp a a)
```


</td>
<td>

```clj
0
```


</td>
</tr>
</table>



---


### bn-divmod

`bn-divmod` computes the quotient and remainder of an bignum (integer) division. The form of a `bn-divmod` expression is `(bn-divmod a b)` where `a` and `b` are bignum. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(bn-divmod a b)
```


</td>
<td>

```clj
([0 0 0 0] . [102 0 0 0])
```


</td>
</tr>
<tr>
<td>

```clj
(bn-divmod b a)
```


</td>
<td>

```clj
([130 2 0 0] . [52 0 0 0])
```


</td>
</tr>
</table>



---


### bn-from-bytes

`bn-from-bytes` creates a bignum from a bytearray. The internal representation of a  bignum is as a little-endian value for implementation efficiency reasons and converting a bytearray to a bignum reverses the byte order The form of a `bn-from-bytes` expression is `(bn-from-bytes ba)` where `ba` is a bytearray that is a multiple of 4 bytes long 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(bn-from-bytes [255 170 0 17])
```


</td>
<td>

```clj
[17 0 170 255]
```


</td>
</tr>
</table>



---


### bn-from-u32

`bn-from-u32` creates a bignum from an u 32. The form of a `bn-from-u32` expression is `(bn-from-u32 a)` where `a` is a number that will be interpreted as a u32. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(bn-from-u32 1)
```


</td>
<td>

```clj
[1 0 0 0]
```


</td>
</tr>
<tr>
<td>

```clj
(bn-from-u32 4294967295u32)
```


</td>
<td>

```clj
[255 255 255 255]
```


</td>
</tr>
</table>



---


### bn-modexp

`bn-modexp` computes bignum exponentiation modulo a modulus. The form of a `bn-modexp` expression is `(bn-modexp a e m)` where a is a bignum value, e is bignum exponent and m is bignum modulo. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(bn-modexp a b c)
```


</td>
<td>

```clj
[24 2 0 0]
```


</td>
</tr>
</table>



---


### bn-mul

`bn-mul` multiplies two bignum values The form of a `bn-mul` expression is `(bn-mul a b)` where `a` and `b` are bignum. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(bn-mul a b)
```


</td>
<td>

```clj
[0 0 102 0 0 0 0 0]
```


</td>
</tr>
<tr>
<td>

```clj
(bn-mul (bn-from-u32 4294967295u32) (bn-from-u32 102))
```


</td>
<td>

```clj
[154 255 255 255 101 0 0 0]
```


</td>
</tr>
<tr>
<td>

```clj
(bn-mul (bn-from-bytes (hex-to-bytes "FFFFFFFF")) (bn-from-u32 102))
```


</td>
<td>

```clj
[154 255 255 255 101 0 0 0]
```


</td>
</tr>
</table>



---


### bn-sub

`bn-sub` subtracts a bignum from another bignum value The form of a `bn-sub` expression is `(bn-sub a b)` where `a` and `b` are bignum. Requires that the first argument is larger than the second. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(bn-sub b a)
```


</td>
<td>

```clj
[154 255 0 0]
```


</td>
</tr>
</table>



---


### bn-to-bytes

`bn-to-bytes` converts a bignum to a bytearray that is compatible with `bn-from-bytes`. the form of a `bn-to-bytes` expression is `(bn-to-bytes a)` where `a` is a bignum. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(bn-to-bytes a)
```


</td>
<td>

```clj
[0 0 0 102]
```


</td>
</tr>
<tr>
<td>

```clj
(bn-to-bytes b)
```


</td>
<td>

```clj
[0 1 0 0]
```


</td>
</tr>
</table>



---


### bn-to-u32

`bn-to-u32` converts a bignum to a u32 value. the form of a `bn-to-u32` expression is `(bn-to-u32 a)` where `a` is a bignum. Note that this requires that the bignum is a so-called single-limb value, that is it needs to <= 0xFFFFFFFF. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(bn-to-u32 a)
```


</td>
<td>

```clj
102u32
```


</td>
</tr>
<tr>
<td>

```clj
(bn-to-u32 b)
```


</td>
<td>

```clj
65536u32
```


</td>
</tr>
</table>



---

This document was generated by LispBM version 0.36.0 

