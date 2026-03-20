# LispBM Math Extensions Reference Manual

The math extensions provide mathematical functions such as trigonometry, logarithms, rounding and related utilities. These extensions may or may not be present depending on the platform and configuration of LispBM. 

## Trigonometry


### sin

Computes the sine of a given angle (in radians). The form of a `sin` expression is `(sin expr)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(sin 0.500000f32)
```


</td>
<td>

```clj
0.479426f32
```


</td>
</tr>
<tr>
<td>

```clj
(sin 3.141593f32)
```


</td>
<td>

```clj
0.000000f32
```


</td>
</tr>
<tr>
<td>

```clj
(sin (* 0.500000f32 3.141592f32))
```


</td>
<td>

```clj
1.000000f32
```


</td>
</tr>
</table>




---


### cos

Computes the cosine of a given angle (in radians). The form of a `cos` expression is `(cos expr)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(cos 0.500000f32)
```


</td>
<td>

```clj
0.877583f32
```


</td>
</tr>
<tr>
<td>

```clj
(cos 3.141593f32)
```


</td>
<td>

```clj
-1.000000f32
```


</td>
</tr>
<tr>
<td>

```clj
(cos (* 0.500000f32 3.141592f32))
```


</td>
<td>

```clj
0.000000f32
```


</td>
</tr>
</table>




---


### tan

Computes the tangent of a given angle (in radians). The form of a `tan` expression is `(tan expr)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(tan 0.500000f32)
```


</td>
<td>

```clj
0.546302f32
```


</td>
</tr>
<tr>
<td>

```clj
(tan 3.141593f32)
```


</td>
<td>

```clj
-0.000000f32
```


</td>
</tr>
<tr>
<td>

```clj
(tan (* 0.500000f32 3.141592f32))
```


</td>
<td>

```clj
3185560.750000f32
```


</td>
</tr>
</table>




---


### asin

Computes the arcsine of a value, returning an angle in radians. The form of an `asin` expression is `(asin expr)`. The argument should be in the range [-1, 1]. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(asin 0.000000f32)
```


</td>
<td>

```clj
0.000000f32
```


</td>
</tr>
<tr>
<td>

```clj
(asin 1.000000f32)
```


</td>
<td>

```clj
1.570796f32
```


</td>
</tr>
<tr>
<td>

```clj
(asin 0.500000f32)
```


</td>
<td>

```clj
0.523599f32
```


</td>
</tr>
</table>




---


### acos

Computes the arccosine of a value, returning an angle in radians. The form of an `acos` expression is `(acos expr)`. The argument should be in the range [-1, 1]. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(acos 0.000000f32)
```


</td>
<td>

```clj
1.570796f32
```


</td>
</tr>
<tr>
<td>

```clj
(acos 1.000000f32)
```


</td>
<td>

```clj
0.000000f32
```


</td>
</tr>
<tr>
<td>

```clj
(acos 0.500000f32)
```


</td>
<td>

```clj
1.047198f32
```


</td>
</tr>
</table>




---


### atan

Computes the arctangent of a value, returning an angle in radians. The form of an `atan` expression is `(atan expr)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(atan 0.000000f32)
```


</td>
<td>

```clj
0.000000f32
```


</td>
</tr>
<tr>
<td>

```clj
(atan 1.000000f32)
```


</td>
<td>

```clj
0.785398f32
```


</td>
</tr>
<tr>
<td>

```clj
(atan -1.000000f32)
```


</td>
<td>

```clj
-0.785398f32
```


</td>
</tr>
</table>




---


### atan2

Computes the arctangent of `y/x`, using the signs of both arguments to determine the correct quadrant. Returns an angle in radians in the range [-pi, pi]. The form of an `atan2` expression is `(atan2 y x)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(atan2 1.000000f32 1.000000f32)
```


</td>
<td>

```clj
0.785398f32
```


</td>
</tr>
<tr>
<td>

```clj
(atan2 0.000000f32 -1.000000f32)
```


</td>
<td>

```clj
3.141593f32
```


</td>
</tr>
<tr>
<td>

```clj
(atan2 -1.000000f32 0.000000f32)
```


</td>
<td>

```clj
-1.570796f32
```


</td>
</tr>
</table>




---

## Exponential and Logarithm


### pow

Raises a base to a given exponent. The form of a `pow` expression is `(pow base exponent)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(pow 2.000000f32 10.000000f32)
```


</td>
<td>

```clj
1024.000000f32
```


</td>
</tr>
<tr>
<td>

```clj
(pow 3.000000f32 3.000000f32)
```


</td>
<td>

```clj
27.000000f32
```


</td>
</tr>
<tr>
<td>

```clj
(pow 2.000000f32 0.500000f32)
```


</td>
<td>

```clj
1.414214f32
```


</td>
</tr>
</table>




---


### exp

Computes the exponential function e raised to the power of the argument. The form of an `exp` expression is `(exp expr)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(exp 0.000000f32)
```


</td>
<td>

```clj
1.000000f32
```


</td>
</tr>
<tr>
<td>

```clj
(exp 1.000000f32)
```


</td>
<td>

```clj
2.718282f32
```


</td>
</tr>
<tr>
<td>

```clj
(exp -1.000000f32)
```


</td>
<td>

```clj
0.367879f32
```


</td>
</tr>
</table>




---


### sqrt

Computes the square root of a number. The form of a `sqrt` expression is `(sqrt expr)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(sqrt 4.000000f32)
```


</td>
<td>

```clj
2.000000f32
```


</td>
</tr>
<tr>
<td>

```clj
(sqrt 2.000000f32)
```


</td>
<td>

```clj
1.414214f32
```


</td>
</tr>
<tr>
<td>

```clj
(sqrt 0.000000f32)
```


</td>
<td>

```clj
0.000000f32
```


</td>
</tr>
</table>




---


### log

Computes the natural logarithm (base e) of a number. The form of a `log` expression is `(log expr)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(log 1.000000f32)
```


</td>
<td>

```clj
0.000000f32
```


</td>
</tr>
<tr>
<td>

```clj
(log 2.718282f32)
```


</td>
<td>

```clj
1.000000f32
```


</td>
</tr>
<tr>
<td>

```clj
(log 10.000000f32)
```


</td>
<td>

```clj
2.302585f32
```


</td>
</tr>
</table>




---


### log10

Computes the base-10 logarithm of a number. The form of a `log10` expression is `(log10 expr)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(log10 1.000000f32)
```


</td>
<td>

```clj
0.000000f32
```


</td>
</tr>
<tr>
<td>

```clj
(log10 10.000000f32)
```


</td>
<td>

```clj
1.000000f32
```


</td>
</tr>
<tr>
<td>

```clj
(log10 100.000000f32)
```


</td>
<td>

```clj
2.000000f32
```


</td>
</tr>
</table>




---

## Rounding


### floor

Rounds a number down to the nearest integer, returning a float. The form of a `floor` expression is `(floor expr)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(floor 1.600000f32)
```


</td>
<td>

```clj
1.000000f32
```


</td>
</tr>
<tr>
<td>

```clj
(floor -1.600000f32)
```


</td>
<td>

```clj
-2.000000f32
```


</td>
</tr>
<tr>
<td>

```clj
(floor 2.000000f32)
```


</td>
<td>

```clj
2.000000f32
```


</td>
</tr>
</table>




---


### ceil

Rounds a number up to the nearest integer, returning a float. The form of a `ceil` expression is `(ceil expr)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(ceil 1.200000f32)
```


</td>
<td>

```clj
2.000000f32
```


</td>
</tr>
<tr>
<td>

```clj
(ceil -1.200000f32)
```


</td>
<td>

```clj
-1.000000f32
```


</td>
</tr>
<tr>
<td>

```clj
(ceil 2.000000f32)
```


</td>
<td>

```clj
2.000000f32
```


</td>
</tr>
</table>




---


### round

Rounds a number to the nearest integer, returning a float. Halfway cases are rounded away from zero. The form of a `round` expression is `(round expr)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(round 1.400000f32)
```


</td>
<td>

```clj
1.000000f32
```


</td>
</tr>
<tr>
<td>

```clj
(round 1.500000f32)
```


</td>
<td>

```clj
2.000000f32
```


</td>
</tr>
<tr>
<td>

```clj
(round -1.500000f32)
```


</td>
<td>

```clj
-2.000000f32
```


</td>
</tr>
</table>




---

## Angle Conversion


### deg2rad

Converts degrees to radians. When called with a single argument the form is `(deg2rad expr)`. When called with multiple arguments, each is converted and the results are returned as a list. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(deg2rad 180.000000f32)
```


</td>
<td>

```clj
3.141593f32
```


</td>
</tr>
<tr>
<td>

```clj
(deg2rad 90.000000f32)
```


</td>
<td>

```clj
1.570796f32
```


</td>
</tr>
<tr>
<td>

```clj
(deg2rad 0.000000f32 90.000000f32 180.000000f32 270.000000f32 360.000000f32)
```


</td>
<td>

```clj
(0.000000f32 1.570796f32 3.141593f32 4.712389f32 6.283185f32)
```


</td>
</tr>
</table>




---


### rad2deg

Converts radians to degrees. When called with a single argument the form is `(rad2deg expr)`. When called with multiple arguments, each is converted and the results are returned as a list. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(rad2deg 3.141592f32)
```


</td>
<td>

```clj
179.999969f32
```


</td>
</tr>
<tr>
<td>

```clj
(rad2deg 1.570796f32)
```


</td>
<td>

```clj
90.000000f32
```


</td>
</tr>
<tr>
<td>

```clj
(rad2deg 0.000000f32 1.570796f32 3.141592f32)
```


</td>
<td>

```clj
(0.000000f32 90.000000f32 179.999969f32)
```


</td>
</tr>
</table>




---

## Floating-point Predicates


### is-nan

`is-nan` checks whether a floating-point value is NaN (not a number). Returns `t` if the value is NaN and `nil` otherwise. The form of an `is-nan` expression is `(is-nan expr)`. Only `float` and `double` values can be NaN; any other number type returns `nil`. NaN can arise from operations such as `(sqrt -1.0)` or `(asin 2.0)`. Note that division by zero is an error in LispBM and does not produce NaN. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(is-nan (sqrt -1.000000f32))
```


</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(is-nan (asin 2.000000f32))
```


</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(is-nan 1.000000f32)
```


</td>
<td>

```clj
nil
```


</td>
</tr>
</table>




---


### is-inf

`is-inf` checks whether a floating-point value is infinite. Returns `t` if the value is positive or negative infinity, `nil` otherwise. The form of an `is-inf` expression is `(is-inf expr)`. Only `float` and `double` values can be infinite; any other number type returns `nil`. Infinity can arise from operations such as `(exp 1000.0)` or `(log 0.0)`. Note that division by zero is an error in LispBM and does not produce infinity. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(is-inf (exp 1000.000000f32))
```


</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(is-inf (log 0.000000f32))
```


</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(is-inf 1.000000f32)
```


</td>
<td>

```clj
nil
```


</td>
</tr>
</table>




---

This document was generated by LispBM version 0.36.0 

