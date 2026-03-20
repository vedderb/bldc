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
